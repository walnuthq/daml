-- Copyright (c) 2025 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}

-- | Foundry-style colored tree renderer for a Daml ledger update. Given a
-- decoded @GetUpdateResponse@ produces an indented, ANSI-colored view of the
-- transaction's event tree, roughly analogous to @cast run@.
--
-- Used by @daml ledger update show --pretty@.
module DA.Daml.Helper.UpdatePretty
    ( prettyUpdateResponse
    , UseColor(..)
    , detectColor
    ) where

import Control.Monad.State.Strict (State, evalState, get, put)
import Data.Int (Int32)
import Data.List (sortOn)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import System.Environment (lookupEnv)
import System.IO (Handle, hIsTerminalDevice)

import qualified DA.Daml.LF.Ast as LF

import qualified Com.Daml.Ledger.Api.V2.Event as EvP
import qualified Com.Daml.Ledger.Api.V2.Reassignment as ReaP
import qualified Com.Daml.Ledger.Api.V2.TopologyTransaction as TopP
import qualified Com.Daml.Ledger.Api.V2.Transaction as TxP
import qualified Com.Daml.Ledger.Api.V2.TraceContext as TrcP
import qualified Com.Daml.Ledger.Api.V2.UpdateService as USP
import qualified Com.Daml.Ledger.Api.V2.Value as VP

--------------------------------------------------------------------------------
-- Color control
--------------------------------------------------------------------------------

data UseColor = Color | NoColor deriving (Eq, Show)

-- | @detectColor h@ returns 'Color' when @h@ is a TTY and the @NO_COLOR@ env
-- var (see <https://no-color.org>) is not set.
detectColor :: Handle -> IO UseColor
detectColor h = do
    tty <- hIsTerminalDevice h
    noCol <- lookupEnv "NO_COLOR"
    pure $ case (tty, noCol) of
        (True, Nothing) -> Color
        _               -> NoColor

data Style = Style { sFg :: Maybe Int, sBold :: Bool, sDim :: Bool }

plain, green, red, yellow, cyan, blue, magenta :: Style
plain   = Style Nothing  False False
green   = Style (Just 32) False False
red     = Style (Just 31) False False
yellow  = Style (Just 33) False False
cyan    = Style (Just 36) False False
blue    = Style (Just 34) False False
magenta = Style (Just 35) False False

bold, dim :: Style -> Style
bold s = s { sBold = True }
dim  s = s { sDim  = True }

stylize :: UseColor -> Style -> T.Text -> T.Text
stylize NoColor _ t = t
stylize Color s t =
    let codes = concat
            [ [T.pack (show n) | Just n <- [sFg s]]
            , ["1" | sBold s]
            , ["2" | sDim s]
            ]
    in "\ESC[" <> T.intercalate ";" codes <> "m" <> t <> "\ESC[0m"

--------------------------------------------------------------------------------
-- Tree structure (reconstructed from the flat event vector)
--------------------------------------------------------------------------------

data Node = Node
    { nEvent    :: EvP.Event
    , nChildren :: [Node]
    }

eventNodeId :: EvP.Event -> Int32
eventNodeId ev = case EvP.eventEvent ev of
    Just (EvP.EventEventCreated e)   -> EvP.createdEventNodeId e
    Just (EvP.EventEventExercised e) -> EvP.exercisedEventNodeId e
    Just (EvP.EventEventArchived e)  -> EvP.archivedEventNodeId e
    Nothing -> maxBound

eventLastDescendant :: EvP.Event -> Maybe Int32
eventLastDescendant ev = case EvP.eventEvent ev of
    Just (EvP.EventEventExercised e) -> Just (EvP.exercisedEventLastDescendantNodeId e)
    _ -> Nothing

-- | Reconstruct the transaction tree. The wire representation gives each
-- event a @node_id@; exercised events also carry @last_descendant_node_id@,
-- an inclusive upper bound on the node-ids of their subtree. Sorted by
-- node-id, the forest can be walked linearly with a cutoff stack.
buildForest :: V.Vector EvP.Event -> [Node]
buildForest evs =
    evalState (consumeUntil maxBound) (sortOn eventNodeId (V.toList evs))
  where
    consumeUntil :: Int32 -> State [EvP.Event] [Node]
    consumeUntil cutoff = do
        s <- get
        case s of
            [] -> pure []
            (ev:rest)
              | eventNodeId ev > cutoff -> pure []
              | otherwise -> do
                    put rest
                    children <- maybe (pure []) consumeUntil (eventLastDescendant ev)
                    siblings <- consumeUntil cutoff
                    pure (Node ev children : siblings)

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

-- | Render a 'USP.GetUpdateResponse' (the oneof wrapper returned by
-- @update show@) as a colored tree.
prettyUpdateResponse :: UseColor -> Maybe LF.World -> USP.GetUpdateResponse -> T.Text
prettyUpdateResponse uc world resp = case USP.getUpdateResponseUpdate resp of
    Just (USP.GetUpdateResponseUpdateTransaction tx)         -> prettyTransaction uc world tx
    Just (USP.GetUpdateResponseUpdateReassignment r)         -> prettyReassignment uc r
    Just (USP.GetUpdateResponseUpdateTopologyTransaction t)  -> prettyTopology uc t
    Nothing -> "(empty update)\n"

--------------------------------------------------------------------------------
-- Transaction rendering
--------------------------------------------------------------------------------

prettyTransaction :: UseColor -> Maybe LF.World -> TxP.Transaction -> T.Text
prettyTransaction uc world tx = T.unlines $
    [ stylize uc (bold cyan) "Update " <> stylize uc (dim plain) (shorten 16 (lazy (TxP.transactionUpdateId tx)))
    , row "command-id"   (lazy (TxP.transactionCommandId tx))
    , row "offset"       (T.pack (show (TxP.transactionOffset tx)))
    , row "synchronizer" (shorten 36 (lazy (TxP.transactionSynchronizerId tx)))
    ]
    <> traceparent (TxP.transactionTraceContext tx)
    <> [""]
    <> concatMap (renderNode uc world 0) (buildForest (TxP.transactionEvents tx))
  where
    row k v = "  " <> stylize uc (dim plain) (T.justifyLeft 14 ' ' k) <> v
    traceparent Nothing = []
    traceparent (Just trc) =
        [ "  " <> stylize uc (dim plain) (T.justifyLeft 14 ' ' "traceparent")
               <> stylize uc (dim plain) (lazy (TrcP.traceContextTraceparent trc))
        ]

prettyReassignment :: UseColor -> ReaP.Reassignment -> T.Text
prettyReassignment uc r = T.unlines
    [ stylize uc (bold blue) "⇄ Reassignment " <> stylize uc (dim plain) (shorten 16 (lazy (ReaP.reassignmentUpdateId r)))
    , "  offset        " <> T.pack (show (ReaP.reassignmentOffset r))
    , "  synchronizer  " <> shorten 36 (lazy (ReaP.reassignmentSynchronizerId r))
    , "  events        " <> T.pack (show (V.length (ReaP.reassignmentEvents r))) <> " reassignment event(s)"
    ]

prettyTopology :: UseColor -> TopP.TopologyTransaction -> T.Text
prettyTopology uc t = T.unlines
    [ stylize uc (bold magenta) "TopologyTransaction " <> stylize uc (dim plain) (shorten 16 (lazy (TopP.topologyTransactionUpdateId t)))
    , "  offset        " <> T.pack (show (TopP.topologyTransactionOffset t))
    , "  synchronizer  " <> shorten 36 (lazy (TopP.topologyTransactionSynchronizerId t))
    , "  events        " <> T.pack (show (V.length (TopP.topologyTransactionEvents t))) <> " topology event(s)"
    ]

--------------------------------------------------------------------------------
-- Node / event rendering (pure depth-based indentation)
--------------------------------------------------------------------------------

renderNode :: UseColor -> Maybe LF.World -> Int -> Node -> [T.Text]
renderNode uc world depth node =
    let indent   = T.replicate depth "  "
        evLines  = renderEvent uc world (nEvent node)
        childLines = concatMap (renderNode uc world (depth + 1)) (nChildren node)
    in map (indent <>) evLines <> childLines

renderEvent :: UseColor -> Maybe LF.World -> EvP.Event -> [T.Text]
renderEvent uc world ev = case EvP.eventEvent ev of
    Just (EvP.EventEventCreated c)   -> renderCreated   uc world c
    Just (EvP.EventEventExercised e) -> renderExercised uc world e
    Just (EvP.EventEventArchived a)  -> renderArchived  uc a
    Nothing -> [stylize uc (dim plain) "(empty event)"]

renderCreated :: UseColor -> Maybe LF.World -> EvP.CreatedEvent -> [T.Text]
renderCreated uc world c =
    let tmpl     = templateDisplayName (EvP.createdEventTemplateId c)
        pkgName  = lazy (EvP.createdEventPackageName c)
        cid      = shorten 14 (lazy (EvP.createdEventContractId c))
        nodeId   = T.pack (show (EvP.createdEventNodeId c))
        header   = stylize uc (bold green) "● CREATE   "
                <> stylize uc (bold cyan) (qualifiedTemplate pkgName tmpl)
                <> "  " <> stylize uc (dim plain) ("#" <> cid)
                <> "  " <> stylize uc (dim plain) ("[node " <> nodeId <> "]")
        argsFs  = maybe [] (V.toList . VP.recordFields) (EvP.createdEventCreateArguments c)
        fieldLns = map (renderField uc world) argsFs
        parties lbl ps = "  " <> stylize uc (dim plain) (T.justifyLeft 12 ' ' lbl)
                           <> renderParties uc ps
    in header
     : map ("  " <>) fieldLns
     <> [ parties "signatories" (EvP.createdEventSignatories c) ]
     <> [ parties "observers"   (EvP.createdEventObservers   c) | not (V.null (EvP.createdEventObservers c)) ]
     <> [ parties "witnesses"   (EvP.createdEventWitnessParties c) ]

renderExercised :: UseColor -> Maybe LF.World -> EvP.ExercisedEvent -> [T.Text]
renderExercised uc world e =
    let tmpl    = templateDisplayName (EvP.exercisedEventTemplateId e)
        pkgName = lazy (EvP.exercisedEventPackageName e)
        choice  = lazy (EvP.exercisedEventChoice e)
        cid     = shorten 14 (lazy (EvP.exercisedEventContractId e))
        nodeId  = T.pack (show (EvP.exercisedEventNodeId e))
        kind    = if EvP.exercisedEventConsuming e
                    then stylize uc (bold red) " (consuming)"
                    else stylize uc (dim plain) " (non-consuming)"
        header  = stylize uc (bold yellow) "▸ EXERCISE "
               <> stylize uc (bold cyan) (qualifiedTemplate pkgName tmpl)
               <> stylize uc (bold yellow) ("::" <> choice)
               <> kind
               <> "  " <> stylize uc (dim plain) ("#" <> cid)
               <> "  " <> stylize uc (dim plain) ("[node " <> nodeId <> "]")
        actingLn = "  " <> stylize uc (dim plain) (T.justifyLeft 12 ' ' "by")
                        <> renderParties uc (EvP.exercisedEventActingParties e)
        argsLn  = case EvP.exercisedEventChoiceArgument e of
            Nothing -> []
            Just v  ->
                [ "  " <> stylize uc (dim plain) (T.justifyLeft 12 ' ' "args")
                       <> renderValue uc world v ]
        resultLn = case EvP.exercisedEventExerciseResult e of
            Nothing -> []
            Just v  ->
                let rv = renderValue uc world v
                in if T.null (T.strip rv) then []
                   else [ "  " <> stylize uc (dim plain) (T.justifyLeft 12 ' ' "result")
                              <> rv ]
    in header : actingLn : argsLn <> resultLn

renderArchived :: UseColor -> EvP.ArchivedEvent -> [T.Text]
renderArchived uc a =
    let tmpl     = templateDisplayName (EvP.archivedEventTemplateId a)
        pkgName  = lazy (EvP.archivedEventPackageName a)
        cid      = shorten 14 (lazy (EvP.archivedEventContractId a))
        nodeId   = T.pack (show (EvP.archivedEventNodeId a))
    in [ stylize uc (bold red) "✖ ARCHIVE  "
       <> stylize uc (bold cyan) (qualifiedTemplate pkgName tmpl)
       <> "  " <> stylize uc (dim plain) ("#" <> cid)
       <> "  " <> stylize uc (dim plain) ("[node " <> nodeId <> "]") ]

renderField :: UseColor -> Maybe LF.World -> VP.RecordField -> T.Text
renderField uc world f =
    let label = lazy (VP.recordFieldLabel f)
        val   = maybe "_" (renderValue uc world) (VP.recordFieldValue f)
    in stylize uc (dim cyan) (T.justifyLeft 13 ' ' label) <> val

--------------------------------------------------------------------------------
-- Value rendering (inverse of LfJson.parseValueAsType, for display)
--------------------------------------------------------------------------------

renderValue :: UseColor -> Maybe LF.World -> VP.Value -> T.Text
renderValue uc world v = case VP.valueSum v of
    Nothing -> "_"
    Just vs -> renderValueSum uc world vs

renderValueSum :: UseColor -> Maybe LF.World -> VP.ValueSum -> T.Text
renderValueSum uc world = go
  where
    go (VP.ValueSumUnit _)       = "unit"
    go (VP.ValueSumBool b)       = T.pack (show b)
    go (VP.ValueSumInt64 i)      = T.pack (show i)
    go (VP.ValueSumDate d)       = "date#" <> T.pack (show d)
    go (VP.ValueSumTimestamp t)  = "time#" <> T.pack (show t)
    go (VP.ValueSumNumeric n)    = lazy n
    go (VP.ValueSumParty p)      = stylize uc green (shorten 28 (lazy p))
    go (VP.ValueSumText t)       = "\"" <> lazy t <> "\""
    go (VP.ValueSumContractId c) = stylize uc (dim plain) ("#" <> shorten 14 (lazy c))
    go (VP.ValueSumOptional o)   = case VP.optionalValue o of
        Nothing -> "None"
        Just v  -> "Some " <> renderValue uc world v
    go (VP.ValueSumList l) =
        "[" <> T.intercalate ", " (map (renderValue uc world) (V.toList (VP.listElements l))) <> "]"
    go (VP.ValueSumTextMap m) =
        let entries = [ "\"" <> lazy (VP.textMap_EntryKey e) <> "\": "
                           <> maybe "_" (renderValue uc world) (VP.textMap_EntryValue e)
                      | e <- V.toList (VP.textMapEntries m) ]
        in "{" <> T.intercalate ", " entries <> "}"
    go (VP.ValueSumGenMap m) =
        let entries = [ "(" <> maybe "_" (renderValue uc world) (VP.genMap_EntryKey e) <> ", "
                            <> maybe "_" (renderValue uc world) (VP.genMap_EntryValue e) <> ")"
                      | e <- V.toList (VP.genMapEntries m) ]
        in "[" <> T.intercalate ", " entries <> "]"
    go (VP.ValueSumRecord r) =
        let fs = [ lazy (VP.recordFieldLabel f) <> " = "
                      <> maybe "_" (renderValue uc world) (VP.recordFieldValue f)
                 | f <- V.toList (VP.recordFields r) ]
        in "{ " <> T.intercalate "; " fs <> " }"
    go (VP.ValueSumVariant v) =
        stylize uc yellow (lazy (VP.variantConstructor v)) <> " "
          <> maybe "_" (renderValue uc world) (VP.variantValue v)
    go (VP.ValueSumEnum e) =
        stylize uc yellow (lazy (VP.enumConstructor e))

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

templateDisplayName :: Maybe VP.Identifier -> T.Text
templateDisplayName Nothing = "<unknown-template>"
templateDisplayName (Just i) =
    lazy (VP.identifierModuleName i) <> ":" <> lazy (VP.identifierEntityName i)

qualifiedTemplate :: T.Text -> T.Text -> T.Text
qualifiedTemplate pkgName display
    | T.null pkgName = display
    | otherwise      = pkgName <> "::" <> display

renderParties :: UseColor -> V.Vector TL.Text -> T.Text
renderParties uc ps =
    "[" <> T.intercalate ", " [ stylize uc green (shorten 28 (lazy p)) | p <- V.toList ps ] <> "]"

lazy :: TL.Text -> T.Text
lazy = TL.toStrict

shorten :: Int -> T.Text -> T.Text
shorten n t
    | T.length t <= n = t
    | n < 8           = T.take n t
    | otherwise       = T.take (n - 6) t <> "…" <> T.takeEnd 4 t
-- suppress unused-import warnings for LF (kept for future schema-aware rendering)
_unused :: LF.World -> ()
_unused _ = ()
{-# ANN _unused ("HLint: ignore Use const" :: String) #-}
