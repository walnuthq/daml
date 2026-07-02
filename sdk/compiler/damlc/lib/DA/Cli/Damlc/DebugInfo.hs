-- Copyright (c) 2026 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

-- | Emission of the experimental @daml-debug-info/v1@ metadata artifact.
--
-- The artifact maps a compiled Daml package back to its source files, source
-- spans, symbols (templates, choices, interfaces, exceptions, data types and
-- top-level values), Daml-LF references, deterministic evaluation step spans,
-- and value slots with explicit availability labels.
--
-- All structural information is derived from the compiled Daml-LF package
-- (never from textual scanning of the sources), and all emitted source paths
-- are package-relative. Source files that do not resolve to a location under
-- the package source root are omitted rather than leaking absolute local
-- paths.
module DA.Cli.Damlc.DebugInfo
  ( writeDamlDebugInfo
  , buildDebugInfo
  , debugInfoSchema
  , debugInfoDarEntryPath
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch, throwIO)
import Control.Monad (forM)
import Control.Monad.Trans.Maybe (runMaybeT)
import qualified "zip-archive" Codec.Archive.Zip as ZipArchive
import qualified Crypto.Hash as Hash
import DA.Daml.Compiler.Dar (getDamlFiles, getSrcRoot)
import qualified DA.Daml.LF.Ast as LF
import qualified DA.Daml.LF.Proto3.Archive as Archive
import DA.Daml.LF.Reader (Dalfs (..), readDalfs)
import DA.Daml.Package.Config (PackageConfigFields (..))
import DA.Pretty (renderPretty)
import qualified DA.Service.Logger as Logger
import Data.Aeson ((.=), ToJSON (..), object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson.Pretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.Generics.Uniplate.Data (universeBi)
import Data.List (isPrefixOf)
import Data.List.Extra (nubOrd)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import qualified Data.NameMap as NM
import qualified Data.Set as Set
import qualified Data.Text as T
import Development.IDE.Core.API (runActionSync)
import qualified Development.IDE.Core.Service as IDE
import Development.IDE.Types.Location (fromNormalizedFilePath)
import qualified SdkVersion.Class
import System.FilePath ((<.>), isAbsolute, joinPath, makeRelative, normalise, replaceExtension)

-- | Stable schema identifier of the emitted artifact.
debugInfoSchema :: T.Text
debugInfoSchema = "daml-debug-info/v1"

-- | Path of the metadata member embedded into the DAR.
debugInfoDarEntryPath :: LF.PackageId -> FilePath
debugInfoDarEntryPath pkgId =
  "META-INF/daml-debug-info/" <> T.unpack (LF.unPackageId pkgId) <> ".json"

------------------------------------------------------------------------------
-- JSON model
------------------------------------------------------------------------------

-- | A resolved package source file, identified by a package-relative path.
data DebugInfoSource = DebugInfoSource
  { srcId :: !T.Text
  , srcModule :: !LF.ModuleName
  , srcUri :: !T.Text
  , srcPath :: !FilePath
  , srcSha256 :: !T.Text
  }

data DebugInfoSpan = DebugInfoSpan
  { spanId :: !T.Text
  , spanSource :: !T.Text
  , spanKind :: !T.Text
  , spanLoc :: !LF.SourceLoc
  }

data DebugInfoSymbol = DebugInfoSymbol
  { symId :: !T.Text
  , symKind :: !T.Text
  , symModule :: !T.Text
  , symName :: !T.Text
  , symQualifiedName :: !T.Text
  , symParent :: !(Maybe T.Text)
  , symSpan :: !(Maybe T.Text)
  , symSource :: !(Maybe T.Text)
  , symType :: !(Maybe T.Text)
  , symLfRef :: !(Maybe DebugInfoLfRef)
  }

data DebugInfoLfRef = DebugInfoLfRef
  { lfRefPackageId :: !LF.PackageId
  , lfRefModule :: !T.Text
  , lfRefEntity :: !T.Text
  , lfRefChoice :: !(Maybe T.Text)
  }

-- | A value location that a consumer may try to populate. The availability
-- label states where the value can legitimately come from:
--
-- * @transaction-visible@: present in participant-visible transaction data
--   (payload fields, choice arguments and results, signatories, observers,
--   acting parties, contract keys) for parties entitled to see the event.
-- * @interpreter-only@: only observable with interpreter/runtime support
--   (preconditions, exception messages, intermediate expressions). Trace
--   tools must not claim these from transaction data alone.
data DebugInfoValueSlot = DebugInfoValueSlot
  { slotId :: !T.Text
  , slotSymbol :: !T.Text
  , slotName :: !T.Text
  , slotKind :: !T.Text
  , slotType :: !(Maybe T.Text)
  , slotSpan :: !(Maybe T.Text)
  , slotAvailability :: !T.Text
  }

-- | A deterministic evaluation step: one source span crossed while evaluating
-- the body of a symbol (a choice update or a top-level value). Step indices
-- are stable for a given package: they follow the pre-order traversal of the
-- compiled Daml-LF expression. Runtime debug events that carry source
-- locations can be joined against these spans.
data DebugInfoStep = DebugInfoStep
  { stepId :: !T.Text
  , stepSymbol :: !T.Text
  , stepIndex :: !Int
  , stepSource :: !T.Text
  , stepLoc :: !LF.SourceLoc
  }

data DebugInfo = DebugInfo
  { diPackageId :: !LF.PackageId
  , diPackageName :: !LF.PackageName
  , diPackageVersion :: !(Maybe LF.PackageVersion)
  , diLfVersion :: !LF.Version
  , diSdkVersion :: !String
  , diSources :: ![DebugInfoSource]
  , diSpans :: ![DebugInfoSpan]
  , diSymbols :: ![DebugInfoSymbol]
  , diValueSlots :: ![DebugInfoValueSlot]
  , diSteps :: ![DebugInfoStep]
  }

instance ToJSON DebugInfo where
  toJSON DebugInfo{..} = object
    [ "schema" .= debugInfoSchema
    , "producer" .= object
        [ "tool" .= ("damlc" :: T.Text)
        , "version" .= diSdkVersion
        , "buildMode" .= ("experimental" :: T.Text)
        , "features" .=
            (["source-spans", "symbols", "lf-refs", "value-slots", "steps"] :: [T.Text])
        ]
    , "package" .= object (catMaybes
        [ Just $ "packageId" .= diPackageId
        , Just $ "name" .= diPackageName
        , ("version" .=) <$> diPackageVersion
        , Just $ "lfVersion" .= LF.renderVersion diLfVersion
        , Just $ "sdkVersion" .= diSdkVersion
        ])
    , "sources" .= diSources
    , "spans" .= diSpans
    , "symbols" .= diSymbols
    , "valueSlots" .= diValueSlots
    , "steps" .= diSteps
    , "compatibility" .= object
        [ "minConsumerSchema" .= debugInfoSchema
        , "ignoreUnknownFields" .= True
        ]
    ]

instance ToJSON DebugInfoSource where
  toJSON DebugInfoSource{..} = object
    [ "id" .= srcId
    , "module" .= moduleNameText srcModule
    , "uri" .= srcUri
    , "path" .= srcPath
    , "sha256" .= srcSha256
    ]

instance ToJSON DebugInfoSpan where
  toJSON DebugInfoSpan{..} = object
    [ "id" .= spanId
    , "source" .= spanSource
    , "kind" .= spanKind
    , "start" .= locStartJson spanLoc
    , "end" .= locEndJson spanLoc
    ]

instance ToJSON DebugInfoSymbol where
  toJSON DebugInfoSymbol{..} = object $ catMaybes
    [ Just $ "id" .= symId
    , Just $ "kind" .= symKind
    , Just $ "module" .= symModule
    , Just $ "name" .= symName
    , Just $ "qualifiedName" .= symQualifiedName
    , ("parent" .=) <$> symParent
    , ("span" .=) <$> symSpan
    , ("source" .=) <$> symSource
    , ("type" .=) <$> symType
    , ("lfRef" .=) <$> symLfRef
    ]

instance ToJSON DebugInfoLfRef where
  toJSON DebugInfoLfRef{..} = object $ catMaybes
    [ Just $ "packageId" .= lfRefPackageId
    , Just $ "module" .= lfRefModule
    , Just $ "entity" .= lfRefEntity
    , ("choice" .=) <$> lfRefChoice
    ]

instance ToJSON DebugInfoValueSlot where
  toJSON DebugInfoValueSlot{..} = object $ catMaybes
    [ Just $ "id" .= slotId
    , Just $ "symbol" .= slotSymbol
    , Just $ "name" .= slotName
    , Just $ "kind" .= slotKind
    , ("type" .=) <$> slotType
    , ("span" .=) <$> slotSpan
    , Just $ "availability" .= slotAvailability
    ]

instance ToJSON DebugInfoStep where
  toJSON DebugInfoStep{..} = object
    [ "id" .= stepId
    , "symbol" .= stepSymbol
    , "index" .= stepIndex
    , "source" .= stepSource
    , "start" .= locStartJson stepLoc
    , "end" .= locEndJson stepLoc
    ]

-- Daml-LF source locations are 0-based; the artifact uses 1-based positions.
locStartJson :: LF.SourceLoc -> Aeson.Value
locStartJson loc = object
  [ "line" .= (LF.slocStartLine loc + 1)
  , "column" .= (LF.slocStartCol loc + 1)
  ]

locEndJson :: LF.SourceLoc -> Aeson.Value
locEndJson loc = object
  [ "line" .= (LF.slocEndLine loc + 1)
  , "column" .= (LF.slocEndCol loc + 1)
  ]

encodeDebugInfo :: DebugInfo -> BSL.ByteString
encodeDebugInfo debugInfo =
  Aeson.Pretty.encodePretty' config (toJSON debugInfo)
  where
    config = Aeson.Pretty.defConfig
      { Aeson.Pretty.confCompare =
          Aeson.Pretty.keyOrder
            [ "schema", "producer", "package"
            , "sources", "spans", "symbols", "valueSlots", "steps", "compatibility"
            , "id", "kind", "tool", "version", "buildMode", "features"
            , "packageId", "name", "module", "entity", "choice"
            , "uri", "path", "sha256"
            , "qualifiedName", "parent", "symbol", "source", "span", "type"
            , "availability", "index", "start", "end", "line", "column"
            ] <> compare
      }

------------------------------------------------------------------------------
-- Name and reference helpers
------------------------------------------------------------------------------

moduleNameText :: LF.ModuleName -> T.Text
moduleNameText = T.intercalate "." . LF.unModuleName

dottedName :: LF.TypeConName -> T.Text
dottedName = T.intercalate "." . LF.unTypeConName

-- | Package-relative source path implied by a module name.
moduleRelPath :: LF.ModuleName -> FilePath
moduleRelPath moduleName =
  joinPath (map T.unpack (LF.unModuleName moduleName)) <.> "daml"

-- | The source locations reachable from an expression, in pre-order,
-- following references into compiler-generated values of this package.
--
-- GHC lambda-lifts choice bodies, controllers, signatories and similar
-- expressions into generated top-level definitions (names containing @$@),
-- so the location nodes live in those definitions rather than inline.
-- User-defined values are not followed: they have their own symbols and
-- steps.
collectLocs :: LF.Package -> LF.Expr -> [LF.SourceLoc]
collectLocs pkg root = go Set.empty [root]
  where
    go _ [] = []
    go visited (expr : pending) =
      let locs = universeBi expr :: [LF.SourceLoc]
          refs =
            [ qval
            | qval <- universeBi expr :: [LF.Qualified LF.ExprValName]
            , LF.qualPackage qval == LF.SelfPackageId
            , isGeneratedName (LF.unExprValName (LF.qualObject qval))
            , not (Set.member qval visited)
            ]
          bodies =
            [ (qval, LF.dvalBody value)
            | qval <- nubOrd refs
            , Just value <- [lookupSelfValue pkg qval]
            ]
      in locs
           ++ go
                (foldr (Set.insert . fst) visited bodies)
                (map snd bodies ++ pending)

lookupSelfValue :: LF.Package -> LF.Qualified LF.ExprValName -> Maybe LF.DefValue
lookupSelfValue pkg qval = case LF.qualPackage qval of
  LF.ImportedPackageId _ -> Nothing
  LF.SelfPackageId -> do
    m <- NM.lookup (LF.qualModule qval) (LF.packageModules pkg)
    NM.lookup (LF.qualObject qval) (LF.moduleValues m)

-- | The first source location reachable from an expression; used to anchor
-- value-slot expressions (signatories, controllers, keys, ...) to source.
exprLoc :: LF.Package -> LF.Expr -> Maybe LF.SourceLoc
exprLoc pkg = listToMaybe . collectLocs pkg

-- | The type constructor at the head of a type application, if any.
tconHead :: LF.Type -> Maybe (LF.Qualified LF.TypeConName)
tconHead = \case
  LF.TCon qtcon -> Just qtcon
  LF.TApp fun _ -> tconHead fun
  _ -> Nothing

-- | Record fields of a type constructor defined in this package.
lookupRecordFields :: LF.Package -> LF.Qualified LF.TypeConName -> Maybe [(LF.FieldName, LF.Type)]
lookupRecordFields pkg qtcon = case LF.qualPackage qtcon of
  LF.ImportedPackageId _ -> Nothing
  LF.SelfPackageId -> do
    m <- NM.lookup (LF.qualModule qtcon) (LF.packageModules pkg)
    dataType <- NM.lookup (LF.qualObject qtcon) (LF.moduleDataTypes m)
    case LF.dataCons dataType of
      LF.DataRecord fields -> Just fields
      _ -> Nothing

-- | Compiler-generated definitions carry @$@ in their names; user-written
-- Daml identifiers cannot.
isGeneratedName :: T.Text -> Bool
isGeneratedName = T.isInfixOf "$"

------------------------------------------------------------------------------
-- Builder
------------------------------------------------------------------------------

type Emitted = ([DebugInfoSpan], [DebugInfoSymbol], [DebugInfoValueSlot], [DebugInfoStep])

-- | Assemble the debug-info artifact from the compiled package. Pure, so it
-- can be exercised in tests without a build environment.
buildDebugInfo
  :: String
  -- ^ SDK version of the producing compiler.
  -> LF.PackageName
  -> Maybe LF.PackageVersion
  -> LF.PackageId
  -> LF.Package
  -> [DebugInfoSource]
  -- ^ Resolved package sources; modules without a resolved source still get
  -- symbols and LF references, but no spans.
  -> DebugInfo
buildDebugInfo sdkVersion pkgName mbPkgVersion pkgId pkg sources = DebugInfo
  { diPackageId = pkgId
  , diPackageName = pkgName
  , diPackageVersion = mbPkgVersion
  , diLfVersion = LF.packageLfVersion pkg
  , diSdkVersion = sdkVersion
  , diSources = sources
  , diSpans = spans
  , diSymbols = symbols
  , diValueSlots = slots
  , diSteps = steps
  }
  where
    sourceByModule = Map.fromList [(srcModule src, src) | src <- sources]
    (spans, symbols, slots, steps) =
      mconcat
        [ moduleEntries pkg pkgId sourceByModule m
        | m <- NM.toList (LF.packageModules pkg)
        ]

moduleEntries
  :: LF.Package
  -> LF.PackageId
  -> Map.Map LF.ModuleName DebugInfoSource
  -> LF.Module
  -> Emitted
moduleEntries pkg pkgId sourceByModule m =
  moduleSymbol <> templates <> interfaces <> exceptions <> dataTypes <> values
  where
    moduleName = LF.moduleName m
    mnT = moduleNameText moduleName
    mbSrc = Map.lookup moduleName sourceByModule

    moduleSymbol =
      ( []
      , [ DebugInfoSymbol
            { symId = "sym:" <> mnT
            , symKind = "module"
            , symModule = mnT
            , symName = mnT
            , symQualifiedName = mnT
            , symParent = Nothing
            , symSpan = Nothing
            , symSource = srcId <$> mbSrc
            , symType = Nothing
            , symLfRef = Nothing
            }
        ]
      , []
      , []
      )

    templates = mconcat
      [ templateEntries pkg pkgId sourceByModule moduleName mbSrc tpl
      | tpl <- NM.toList (LF.moduleTemplates m)
      ]

    interfaces = mconcat
      [ interfaceEntries pkg pkgId sourceByModule moduleName mbSrc iface
      | iface <- NM.toList (LF.moduleInterfaces m)
      ]

    exceptions = mconcat
      [ exceptionEntries pkgId moduleName mbSrc exn
      | exn <- NM.toList (LF.moduleExceptions m)
      ]

    -- Data types that back templates, interfaces, exceptions, or choice
    -- arguments are already covered by their owning symbol and its value
    -- slots; only standalone user-defined types get their own symbol.
    ownedTypeCons = Set.fromList $
      [ LF.tplTypeCon tpl | tpl <- NM.toList (LF.moduleTemplates m) ]
      <> [ LF.intName iface | iface <- NM.toList (LF.moduleInterfaces m) ]
      <> [ LF.exnName exn | exn <- NM.toList (LF.moduleExceptions m) ]
      <> [ LF.qualObject qtcon
         | tpl <- NM.toList (LF.moduleTemplates m)
         , choice <- NM.toList (LF.tplChoices tpl)
         , Just qtcon <- [tconHead (snd (LF.chcArgBinder choice))]
         , LF.qualPackage qtcon == LF.SelfPackageId
         , LF.qualModule qtcon == moduleName
         ]
      <> [ LF.qualObject qtcon
         | iface <- NM.toList (LF.moduleInterfaces m)
         , choice <- NM.toList (LF.intChoices iface)
         , Just qtcon <- [tconHead (snd (LF.chcArgBinder choice))]
         , LF.qualPackage qtcon == LF.SelfPackageId
         , LF.qualModule qtcon == moduleName
         ]

    dataTypes = mconcat
      [ dataTypeEntries pkgId moduleName mbSrc dataType
      | dataType <- NM.toList (LF.moduleDataTypes m)
      , not (Set.member (LF.dataTypeCon dataType) ownedTypeCons)
      , not (isGeneratedName (dottedName (LF.dataTypeCon dataType)))
      , LF.dataCons dataType /= LF.DataInterface
      ]

    values = mconcat
      [ valueEntries pkg sourceByModule moduleName mbSrc value
      | value <- NM.toList (LF.moduleValues m)
      , not (isGeneratedName (LF.unExprValName (fst (LF.dvalBinder value))))
      ]

templateEntries
  :: LF.Package
  -> LF.PackageId
  -> Map.Map LF.ModuleName DebugInfoSource
  -> LF.ModuleName
  -> Maybe DebugInfoSource
  -> LF.Template
  -> Emitted
templateEntries pkg pkgId sourceByModule moduleName mbSrc tpl =
  templateEntry <> choiceEntriesAll
  where
    mnT = moduleNameText moduleName
    tplName = dottedName (LF.tplTypeCon tpl)
    symKey = mnT <> ":" <> tplName
    sid = "sym:" <> symKey
    tplSpan = mkSpan mbSrc ("span:" <> symKey) "template-definition" (LF.tplLocation tpl)

    payloadFields = maybe [] id $ do
      dataType <- NM.lookup (LF.tplTypeCon tpl) . LF.moduleDataTypes
        =<< NM.lookup moduleName (LF.packageModules pkg)
      case LF.dataCons dataType of
        LF.DataRecord fields -> Just fields
        _ -> Nothing

    payloadSlots =
      [ DebugInfoValueSlot
          { slotId = "slot:" <> symKey <> ":payload:" <> LF.unFieldName fieldName
          , slotSymbol = sid
          , slotName = LF.unFieldName fieldName
          , slotKind = "contract-payload-field"
          , slotType = Just (renderPretty fieldType)
          , slotSpan = Nothing
          , slotAvailability = "transaction-visible"
          }
      | (fieldName, fieldType) <- payloadFields
      ]

    exprSlot slotSuffix kind availability mbExpr = case mbExpr of
      Nothing -> ([], [])
      Just expr ->
        let mbExprSpan = mkSpan mbSrc ("span:" <> symKey <> ":" <> slotSuffix)
              (kind <> "-expression") (exprLoc pkg expr)
        in ( maybe [] pure mbExprSpan
           , [ DebugInfoValueSlot
                 { slotId = "slot:" <> symKey <> ":" <> slotSuffix
                 , slotSymbol = sid
                 , slotName = slotSuffix
                 , slotKind = kind
                 , slotType = Nothing
                 , slotSpan = spanId <$> mbExprSpan
                 , slotAvailability = availability
                 }
             ]
           )

    (signatorySpans, signatorySlots) =
      exprSlot "signatories" "signatories" "transaction-visible" (Just (LF.tplSignatories tpl))
    (observerSpans, observerSlots) =
      exprSlot "observers" "observers" "transaction-visible" (Just (LF.tplObservers tpl))
    (preconditionSpans, preconditionSlots) =
      case exprLoc pkg (LF.tplPrecondition tpl) of
        -- A trivial precondition has no source span; skip the noise.
        Nothing -> ([], [])
        Just _ -> exprSlot "ensure" "precondition" "interpreter-only" (Just (LF.tplPrecondition tpl))
    (keySpans, keySlots) = case LF.tplKey tpl of
      Nothing -> ([], [])
      Just key ->
        exprSlot "key" "contract-key" "transaction-visible" (Just (LF.tplKeyBody key))
        <> exprSlot "key-maintainers" "key-maintainers" "transaction-visible"
             (Just (LF.tplKeyMaintainers key))

    templateEntry =
      ( maybe [] pure tplSpan
          <> signatorySpans <> observerSpans <> preconditionSpans <> keySpans
      , [ DebugInfoSymbol
            { symId = sid
            , symKind = "template"
            , symModule = mnT
            , symName = tplName
            , symQualifiedName = symKey
            , symParent = Nothing
            , symSpan = spanId <$> tplSpan
            , symSource = srcId <$> mbSrc
            , symType = Nothing
            , symLfRef = Just DebugInfoLfRef
                { lfRefPackageId = pkgId
                , lfRefModule = mnT
                , lfRefEntity = tplName
                , lfRefChoice = Nothing
                }
            }
        ]
      , payloadSlots <> signatorySlots <> observerSlots <> preconditionSlots <> keySlots
      , []
      )

    choiceEntriesAll = mconcat
      [ choiceEntries pkg pkgId sourceByModule moduleName mbSrc symKey sid "choice"
          tplName choice
      | choice <- NM.toList (LF.tplChoices tpl)
      ]

choiceEntries
  :: LF.Package
  -> LF.PackageId
  -> Map.Map LF.ModuleName DebugInfoSource
  -> LF.ModuleName
  -> Maybe DebugInfoSource
  -> T.Text
  -- ^ Symbol key of the owning template or interface, e.g. @Asset:Asset@.
  -> T.Text
  -- ^ Symbol id of the owning template or interface.
  -> T.Text
  -- ^ Symbol kind to emit, @choice@ or @interface-choice@.
  -> T.Text
  -- ^ Entity name of the owning template or interface.
  -> LF.TemplateChoice
  -> Emitted
choiceEntries pkg pkgId sourceByModule moduleName mbSrc parentKey parentSym kind entityName choice =
  ( maybe [] pure choiceSpan
      <> controllerSpans <> choiceObserverSpans <> authorizerSpans
  , [ DebugInfoSymbol
        { symId = sid
        , symKind = kind
        , symModule = mnT
        , symName = choiceName
        , symQualifiedName = parentKey <> "." <> choiceName
        , symParent = Just parentSym
        , symSpan = spanId <$> choiceSpan
        , symSource = srcId <$> mbSrc
        , symType = Nothing
        , symLfRef = Just DebugInfoLfRef
            { lfRefPackageId = pkgId
            , lfRefModule = mnT
            , lfRefEntity = entityName
            , lfRefChoice = Just choiceName
            }
        }
    ]
  , argumentSlots <> resultSlots <> selfSlots
      <> controllerSlots <> choiceObserverSlots <> authorizerSlots
  , stepEntries pkg sourceByModule moduleName symKey (LF.chcUpdate choice)
  )
  where
    mnT = moduleNameText moduleName
    choiceName = LF.unChoiceName (LF.chcName choice)
    symKey = parentKey <> ":" <> choiceName
    sid = "sym:" <> symKey
    -- The compiler populates 'chcLocation' only on recent branches; the
    -- choice's argument record reliably carries the declaration span.
    choiceSpan = mkSpan mbSrc ("span:" <> symKey) "choice-definition"
      (LF.chcLocation choice <|> argumentRecordLoc)
    argumentRecordLoc = do
      qtcon <- tconHead (snd (LF.chcArgBinder choice))
      case LF.qualPackage qtcon of
        LF.ImportedPackageId _ -> Nothing
        LF.SelfPackageId -> do
          m <- NM.lookup (LF.qualModule qtcon) (LF.packageModules pkg)
          dataType <- NM.lookup (LF.qualObject qtcon) (LF.moduleDataTypes m)
          LF.dataLocation dataType

    argumentFields = maybe [] id $
      lookupRecordFields pkg =<< tconHead (snd (LF.chcArgBinder choice))

    argumentSlots =
      [ DebugInfoValueSlot
          { slotId = "slot:" <> symKey <> ":argument"
          , slotSymbol = sid
          , slotName = LF.unExprVarName (fst (LF.chcArgBinder choice))
          , slotKind = "choice-argument"
          , slotType = Just (renderPretty (snd (LF.chcArgBinder choice)))
          , slotSpan = Nothing
          , slotAvailability = "transaction-visible"
          }
      ]
      <>
      [ DebugInfoValueSlot
          { slotId = "slot:" <> symKey <> ":argument:" <> LF.unFieldName fieldName
          , slotSymbol = sid
          , slotName = LF.unFieldName fieldName
          , slotKind = "choice-argument-field"
          , slotType = Just (renderPretty fieldType)
          , slotSpan = Nothing
          , slotAvailability = "transaction-visible"
          }
      | (fieldName, fieldType) <- argumentFields
      ]

    resultSlots =
      [ DebugInfoValueSlot
          { slotId = "slot:" <> symKey <> ":result"
          , slotSymbol = sid
          , slotName = "result"
          , slotKind = "choice-result"
          , slotType = Just (renderPretty (LF.chcReturnType choice))
          , slotSpan = Nothing
          , slotAvailability = "transaction-visible"
          }
      ]

    selfSlots =
      [ DebugInfoValueSlot
          { slotId = "slot:" <> symKey <> ":self"
          , slotSymbol = sid
          , slotName = LF.unExprVarName (LF.chcSelfBinder choice)
          , slotKind = "self-contract-id"
          , slotType = Nothing
          , slotSpan = Nothing
          , slotAvailability = "transaction-visible"
          }
      ]

    exprSlot slotSuffix slotKind' availability mbExpr = case mbExpr of
      Nothing -> ([], [])
      Just expr ->
        let mbExprSpan = mkSpan mbSrc ("span:" <> symKey <> ":" <> slotSuffix)
              (slotKind' <> "-expression") (exprLoc pkg expr)
        in ( maybe [] pure mbExprSpan
           , [ DebugInfoValueSlot
                 { slotId = "slot:" <> symKey <> ":" <> slotSuffix
                 , slotSymbol = sid
                 , slotName = slotSuffix
                 , slotKind = slotKind'
                 , slotType = Nothing
                 , slotSpan = spanId <$> mbExprSpan
                 , slotAvailability = availability
                 }
             ]
           )

    (controllerSpans, controllerSlots) =
      exprSlot "controllers" "choice-controllers" "transaction-visible"
        (Just (LF.chcControllers choice))
    (choiceObserverSpans, choiceObserverSlots) =
      exprSlot "choice-observers" "choice-observers" "transaction-visible"
        (LF.chcObservers choice)
    (authorizerSpans, authorizerSlots) =
      exprSlot "authorizers" "choice-authorizers" "interpreter-only"
        (LF.chcAuthorizers choice)

interfaceEntries
  :: LF.Package
  -> LF.PackageId
  -> Map.Map LF.ModuleName DebugInfoSource
  -> LF.ModuleName
  -> Maybe DebugInfoSource
  -> LF.DefInterface
  -> Emitted
interfaceEntries pkg pkgId sourceByModule moduleName mbSrc iface =
  interfaceEntry <> methodEntries <> choiceEntriesAll
  where
    mnT = moduleNameText moduleName
    ifaceName = dottedName (LF.intName iface)
    symKey = mnT <> ":" <> ifaceName
    sid = "sym:" <> symKey
    ifaceSpan = mkSpan mbSrc ("span:" <> symKey) "interface-definition" (LF.intLocation iface)

    interfaceEntry =
      ( maybe [] pure ifaceSpan
      , [ DebugInfoSymbol
            { symId = sid
            , symKind = "interface"
            , symModule = mnT
            , symName = ifaceName
            , symQualifiedName = symKey
            , symParent = Nothing
            , symSpan = spanId <$> ifaceSpan
            , symSource = srcId <$> mbSrc
            , symType = Nothing
            , symLfRef = Just DebugInfoLfRef
                { lfRefPackageId = pkgId
                , lfRefModule = mnT
                , lfRefEntity = ifaceName
                , lfRefChoice = Nothing
                }
            }
        ]
      , [ DebugInfoValueSlot
            { slotId = "slot:" <> symKey <> ":view"
            , slotSymbol = sid
            , slotName = "view"
            , slotKind = "interface-view"
            , slotType = Just (renderPretty (LF.intView iface))
            , slotSpan = Nothing
            , slotAvailability = "transaction-visible"
            }
        ]
      , []
      )

    methodEntries = mconcat
      [ ( maybe [] pure methodSpan
        , [ DebugInfoSymbol
              { symId = "sym:" <> methodKey
              , symKind = "interface-method"
              , symModule = mnT
              , symName = methodName
              , symQualifiedName = symKey <> "." <> methodName
              , symParent = Just sid
              , symSpan = spanId <$> methodSpan
              , symSource = srcId <$> mbSrc
              , symType = Just (renderPretty (LF.ifmType method))
              , symLfRef = Nothing
              }
          ]
        , []
        , []
        )
      | method <- NM.toList (LF.intMethods iface)
      , let methodName = LF.unMethodName (LF.ifmName method)
      , let methodKey = symKey <> ":method:" <> methodName
      , let methodSpan = mkSpan mbSrc ("span:" <> methodKey) "interface-method-definition"
              (LF.ifmLocation method)
      ]

    choiceEntriesAll = mconcat
      [ choiceEntries pkg pkgId sourceByModule moduleName mbSrc symKey sid
          "interface-choice" ifaceName choice
      | choice <- NM.toList (LF.intChoices iface)
      ]

exceptionEntries
  :: LF.PackageId
  -> LF.ModuleName
  -> Maybe DebugInfoSource
  -> LF.DefException
  -> Emitted
exceptionEntries pkgId moduleName mbSrc exn =
  ( maybe [] pure exnSpan
  , [ DebugInfoSymbol
        { symId = sid
        , symKind = "exception"
        , symModule = mnT
        , symName = exnName
        , symQualifiedName = symKey
        , symParent = Nothing
        , symSpan = spanId <$> exnSpan
        , symSource = srcId <$> mbSrc
        , symType = Nothing
        , symLfRef = Just DebugInfoLfRef
            { lfRefPackageId = pkgId
            , lfRefModule = mnT
            , lfRefEntity = exnName
            , lfRefChoice = Nothing
            }
        }
    ]
  , [ DebugInfoValueSlot
        { slotId = "slot:" <> symKey <> ":message"
        , slotSymbol = sid
        , slotName = "message"
        , slotKind = "exception-message"
        , slotType = Nothing
        , slotSpan = Nothing
        , slotAvailability = "interpreter-only"
        }
    ]
  , []
  )
  where
    mnT = moduleNameText moduleName
    exnName = dottedName (LF.exnName exn)
    symKey = mnT <> ":" <> exnName
    sid = "sym:" <> symKey
    exnSpan = mkSpan mbSrc ("span:" <> symKey) "exception-definition" (LF.exnLocation exn)

dataTypeEntries
  :: LF.PackageId
  -> LF.ModuleName
  -> Maybe DebugInfoSource
  -> LF.DefDataType
  -> Emitted
dataTypeEntries pkgId moduleName mbSrc dataType =
  ( maybe [] pure typeSpan
  , [ DebugInfoSymbol
        { symId = sid
        , symKind = kind
        , symModule = mnT
        , symName = typeName
        , symQualifiedName = mnT <> ":" <> typeName
        , symParent = Nothing
        , symSpan = spanId <$> typeSpan
        , symSource = srcId <$> mbSrc
        , symType = Nothing
        , symLfRef = Just DebugInfoLfRef
            { lfRefPackageId = pkgId
            , lfRefModule = mnT
            , lfRefEntity = typeName
            , lfRefChoice = Nothing
            }
        }
    ]
  , []
  , []
  )
  where
    mnT = moduleNameText moduleName
    typeName = dottedName (LF.dataTypeCon dataType)
    symKey = mnT <> ":type:" <> typeName
    sid = "sym:" <> symKey
    kind = case LF.dataCons dataType of
      LF.DataRecord{} -> "record"
      LF.DataVariant{} -> "variant"
      LF.DataEnum{} -> "enum"
      LF.DataInterface -> "interface-marker"
    typeSpan = mkSpan mbSrc ("span:" <> symKey) "data-type-definition" (LF.dataLocation dataType)

valueEntries
  :: LF.Package
  -> Map.Map LF.ModuleName DebugInfoSource
  -> LF.ModuleName
  -> Maybe DebugInfoSource
  -> LF.DefValue
  -> Emitted
valueEntries pkg sourceByModule moduleName mbSrc value =
  ( maybe [] pure valueSpan
  , [ DebugInfoSymbol
        { symId = sid
        , symKind = "value"
        , symModule = mnT
        , symName = valueName
        , symQualifiedName = symKey
        , symParent = Nothing
        , symSpan = spanId <$> valueSpan
        , symSource = srcId <$> mbSrc
        , symType = Just (renderPretty (snd (LF.dvalBinder value)))
        , symLfRef = Nothing
        }
    ]
  , []
  , stepEntries pkg sourceByModule moduleName symKey (LF.dvalBody value)
  )
  where
    mnT = moduleNameText moduleName
    valueName = LF.unExprValName (fst (LF.dvalBinder value))
    symKey = mnT <> ":" <> valueName
    sid = "sym:" <> symKey
    valueSpan = mkSpan mbSrc ("span:" <> symKey) "value-definition" (LF.dvalLocation value)

-- | Emit a span only when it provably belongs to the module's own source
-- file: either it carries no module reference, or the reference points back
-- into this package at this module.
mkSpan
  :: Maybe DebugInfoSource
  -> T.Text
  -> T.Text
  -> Maybe LF.SourceLoc
  -> Maybe DebugInfoSpan
mkSpan mbSrc sid kind mbLoc = do
  src <- mbSrc
  loc <- mbLoc
  case LF.slocModuleRef loc of
    Nothing -> Just ()
    Just (LF.SelfPackageId, mn) | mn == srcModule src -> Just ()
    _ -> Nothing
  pure DebugInfoSpan
    { spanId = sid
    , spanSource = srcId src
    , spanKind = kind
    , spanLoc = loc
    }

-- | Deterministic evaluation steps of an expression body: the source spans
-- of all @ELocation@ nodes in pre-order, de-duplicated, restricted to spans
-- that resolve into this package's own sources.
stepEntries
  :: LF.Package
  -> Map.Map LF.ModuleName DebugInfoSource
  -> LF.ModuleName
  -> T.Text
  -> LF.Expr
  -> [DebugInfoStep]
stepEntries pkg sourceByModule moduleName symKey body =
  [ DebugInfoStep
      { stepId = "step:" <> symKey <> ":" <> T.pack (show index)
      , stepSymbol = "sym:" <> symKey
      , stepIndex = index
      , stepSource = srcId src
      , stepLoc = loc
      }
  | (index, (src, loc)) <- zip [0 :: Int ..] resolvedLocs
  ]
  where
    allLocs = nubOrd (collectLocs pkg body)
    resolvedLocs = mapMaybe resolve allLocs
    resolve loc = do
      locModule <- case LF.slocModuleRef loc of
        Nothing -> Just moduleName
        Just (LF.SelfPackageId, mn) -> Just mn
        Just (LF.ImportedPackageId _, _) -> Nothing
      src <- Map.lookup locModule sourceByModule
      pure (src, loc)

------------------------------------------------------------------------------
-- Emission
------------------------------------------------------------------------------

-- | Emit the @daml-debug-info/v1@ sidecar next to the DAR and embed the same
-- content into the DAR under 'debugInfoDarEntryPath'.
writeDamlDebugInfo
  :: SdkVersion.Class.SdkVersioned
  => Logger.Handle IO
  -> IDE.IdeState
  -> PackageConfigFields
  -> FilePath
  -> IO ()
writeDamlDebugInfo loggerH ideState PackageConfigFields{pName, pVersion, pSrc} darPath = do
  mbFiles <- resolvePackageFiles ideState pSrc
  case mbFiles of
    Nothing ->
      Logger.logWarning loggerH
        "Skipping debug-info: package source files could not be resolved."
    Just relFiles -> do
      darBytes <- B.readFile darPath
      let archive = ZipArchive.toArchive (BSL.fromStrict darBytes)
      case readDalfs archive of
        Left err ->
          Logger.logWarning loggerH $
            "Skipping debug-info: cannot read DAR: " <> T.pack err
        Right Dalfs{mainDalf} ->
          case Archive.decodeArchive Archive.DecodeAsMain (BSL.toStrict mainDalf) of
            Left err ->
              Logger.logWarning loggerH $
                "Skipping debug-info: cannot decode main DALF: " <> T.pack (show err)
            Right (pkgId, pkg) -> do
              sources <- resolveModuleSources pName relFiles pkg
              let debugInfo = buildDebugInfo
                    SdkVersion.Class.sdkVersion pName pVersion pkgId pkg sources
                  encoded = encodeDebugInfo debugInfo
                  sidecarPath = replaceExtension darPath "debug-info.json"
                  darEntryPath = debugInfoDarEntryPath pkgId
              BSL.writeFile sidecarPath encoded
              embedDebugInfoInDar darPath darEntryPath encoded
              Logger.logInfo loggerH $ "Created " <> T.pack sidecarPath
              Logger.logInfo loggerH $ "Embedded " <> T.pack darEntryPath <> " in DAR."

-- | Package source files as (package-relative path, readable path) pairs.
-- Files outside the package source root are dropped: emitting them would
-- require non-portable paths.
resolvePackageFiles :: IDE.IdeState -> FilePath -> IO (Maybe [(FilePath, FilePath)])
resolvePackageFiles ideState src = do
  mbInput <- runActionSync ideState $ runMaybeT $
    (,) <$> getSrcRoot src <*> getDamlFiles src
  pure $ flip fmap mbInput $ \(srcRoot, files) ->
    let rootPath = normalise (fromNormalizedFilePath srcRoot)
    in  [ (relPath, filePath)
        | file <- files
        , let filePath = normalise (fromNormalizedFilePath file)
        , let relPath = makeRelative rootPath filePath
        , not (isAbsolute relPath)
        , not (".." `isPrefixOf` relPath)
        ]

-- | Match the package's modules against the resolved source files and hash
-- the file contents. Modules whose implied path is not among the package
-- files are skipped.
resolveModuleSources
  :: LF.PackageName
  -> [(FilePath, FilePath)]
  -> LF.Package
  -> IO [DebugInfoSource]
resolveModuleSources pkgName relFiles pkg = do
  let byRelPath = Map.fromList relFiles
  fmap catMaybes $ forM (NM.toList (LF.packageModules pkg)) $ \m ->
    case Map.lookup (moduleRelPath (LF.moduleName m)) byRelPath of
      Nothing -> pure Nothing
      Just filePath -> do
        contents <- B.readFile filePath
        let relPath = moduleRelPath (LF.moduleName m)
        pure $ Just DebugInfoSource
          { srcId = "src:" <> moduleNameText (LF.moduleName m)
          , srcModule = LF.moduleName m
          , srcUri = "daml://" <> LF.unPackageName pkgName <> "/" <> T.pack relPath
          , srcPath = relPath
          , srcSha256 = T.pack (show (Hash.hash contents :: Hash.Digest Hash.SHA256))
          }

-- | Add the metadata entry to the already-written DAR. Retries because the
-- file was just closed by the writer; a lazy read can keep the handle open
-- long enough for the rewrite to fail on macOS, hence the strict read.
embedDebugInfoInDar :: FilePath -> FilePath -> BSL.ByteString -> IO ()
embedDebugInfoInDar darPath entryPath encoded = go (10 :: Int)
  where
    go retries = writeEntry `catch` handleRetry retries

    handleRetry :: Int -> IOException -> IO ()
    handleRetry retries e
      | retries > 0 = threadDelay 100000 >> go (retries - 1)
      | otherwise = throwIO e

    writeEntry = do
      archive <- ZipArchive.toArchive . BSL.fromStrict <$> B.readFile darPath
      let entry = ZipArchive.toEntry entryPath 0 encoded
      BSL.writeFile darPath $ ZipArchive.fromArchive $ ZipArchive.addEntryToArchive entry archive
