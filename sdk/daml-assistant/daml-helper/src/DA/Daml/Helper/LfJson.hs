-- Copyright (c) 2025 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}

-- | Translate user-provided JSON (or `--arg k=v` pairs) into the proto `Value`
-- shape expected by CommandService, using a Daml-LF `World` as the type
-- schema. Mirrors the JSON API's "compressed" codec so users familiar with
-- the HTTP surface can use identical inputs.
module DA.Daml.Helper.LfJson
    ( -- * Template references
      TemplateRef(..)
    , parseTemplateRef
    , templateRefToIdentifier
      -- * Schema resolution
    , SchemaSource(..)
    , ResolvedTemplate(..)
    , resolveFromDar
    , resolveTemplateRef
      -- * Value encoding
    , parseValueAsType
    , buildCreateRecord
      -- * k=v sugar
    , ArgsInput(..)
    , encodeArgsInput
    ) where

import Control.Monad (foldM, unless, when)
import qualified "zip-archive" Codec.Archive.Zip as Zip
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isDigit, isHexDigit)
import qualified Data.HashMap.Strict as H
import Data.Int (Int32, Int64)
import Data.List (find, group, sort, sortOn)
import qualified Data.NameMap as NM
import Data.Ord (Down(..))
import Data.Scientific (Scientific, floatingOrInteger)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V

import qualified DA.Daml.LF.Ast as LF
import qualified DA.Daml.LF.Ast.Subst as LFSubst
import DA.Daml.Dar.Reader (InspectInfo(..), DalfInfo(..), collectInfo)

import qualified Com.Daml.Ledger.Api.V2.Value as VP
import qualified Google.Protobuf.Empty

--------------------------------------------------------------------------------
-- Template references
--------------------------------------------------------------------------------

-- | A template identifier as written on the command line.
--
-- * 'TRPackageName'   — @\#my-package:Mod.Sub:Entity@
-- * 'TRPackageId'     — @122034ab...:Mod.Sub:Entity@
data TemplateRef
    = TRPackageName T.Text LF.ModuleName LF.TypeConName
    | TRPackageId   LF.PackageId LF.ModuleName LF.TypeConName
    deriving (Show, Eq)

-- | Parse a template reference string. The module part may contain dots for
-- submodules (e.g. @Foo.Bar:Baz@).
parseTemplateRef :: String -> Either String TemplateRef
parseTemplateRef s = case T.splitOn ":" (T.pack s) of
    [pkg, modTxt, entTxt]
      | T.null pkg -> Left "template reference: empty package part"
      | T.null modTxt -> Left "template reference: empty module part"
      | T.null entTxt -> Left "template reference: empty entity part"
      | otherwise -> do
          let modName = LF.ModuleName (T.splitOn "." modTxt)
              entName = LF.TypeConName [entTxt]
          if T.head pkg == '#'
              then Right $ TRPackageName (T.drop 1 pkg) modName entName
              else do
                  unless (T.all isHexDigit pkg) $
                      Left "template reference: package-id must be lowercase hex or start with '#'"
                  Right $ TRPackageId (LF.PackageId pkg) modName entName
    _ -> Left $ "template reference must be '#pkg-name:Mod:Entity' or '<hex>:Mod:Entity', got: " <> s

-- | Convert a template reference to the proto `Identifier` used in the
-- command's `template_id` field. Canton resolves @\#name@ refs server-side
-- against the currently vetted package; we pass them through verbatim.
templateRefToIdentifier :: TemplateRef -> VP.Identifier
templateRefToIdentifier ref = VP.Identifier
    { VP.identifierPackageId  = TL.fromStrict pkgPart
    , VP.identifierModuleName = TL.fromStrict (T.intercalate "." modParts)
    , VP.identifierEntityName = TL.fromStrict (T.intercalate "." entParts)
    }
  where
    (pkgPart, LF.ModuleName modParts, LF.TypeConName entParts) = case ref of
        TRPackageName name m e -> ("#" <> name, m, e)
        TRPackageId   p m e    -> (LF.unPackageId p, m, e)

--------------------------------------------------------------------------------
-- Schema resolution
--------------------------------------------------------------------------------

data SchemaSource
    = FromLedger LF.World       -- ^ pre-assembled world from PackageService
    | FromDar    FilePath       -- ^ path to a local DAR on disk

-- | Everything the encoder needs to translate user arguments against a template.
data ResolvedTemplate = ResolvedTemplate
    { rtPackageId   :: LF.PackageId
    , rtModule      :: LF.ModuleName
    , rtEntity      :: LF.TypeConName
    , rtWorld       :: LF.World
    , rtTemplate    :: LF.Template
    , rtFields      :: [(LF.FieldName, LF.Type)]   -- ^ record fields of the template
    , rtKeyType     :: Maybe LF.Type               -- ^ template-key type (if any)
    }

-- | Load a DAR from disk and build a `World` over all its DALFs. Returns the
-- world plus the main package id.
--
-- The main package is exposed BOTH as the world's self-package (so
-- @SelfPackageId@ references inside its own code resolve via
-- 'LF.lookupDataType') AND as an external package (so name-based
-- resolution in 'resolveTemplateRef' can find it).
resolveFromDar :: FilePath -> IO (LF.World, LF.PackageId)
resolveFromDar darPath = do
    archive <- Zip.toArchive <$> BSL.readFile darPath
    info <- either fail pure (collectInfo archive)
    let mainPid = mainPackageId info
        allPkgs = H.toList (packages info)
    case lookup mainPid allPkgs of
        Nothing -> fail $ "main package " <> show mainPid <> " not found in DAR"
        Just mainDalf ->
            let externals =
                    [ LF.ExternalPackage pid (dalfPackage d) | (pid, d) <- allPkgs ]
            in pure (LF.initWorldSelf externals (dalfPackage mainDalf), mainPid)

-- | Look up a template reference in a world. For package-name refs, we scan
-- every package in the world's import set for a metadata match and pick the
-- highest `packageVersion`. For package-id refs we look up directly.
resolveTemplateRef :: LF.World -> TemplateRef -> Either String ResolvedTemplate
resolveTemplateRef world ref = do
    (pid, pkg) <- lookupPkg
    let mods = LF.packageModules pkg
        (modName, entName) = case ref of
            TRPackageName _ m e -> (m, e)
            TRPackageId   _ m e -> (m, e)
    mdl <- maybeToEither
        ("module " <> showModName modName <> " not in package " <> T.unpack (LF.unPackageId pid))
        (NM.lookup modName mods)
    tpl <- maybeToEither
        ("template " <> showTpl entName <> " not in module " <> showModName modName)
        (NM.lookup entName (LF.moduleTemplates mdl))
    dt <- maybeToEither
        ("data type " <> showTpl entName <> " not found (template has no companion record)")
        (NM.lookup entName (LF.moduleDataTypes mdl))
    fields <- case LF.dataCons dt of
        LF.DataRecord fs -> Right fs
        _ -> Left $ "template " <> showTpl entName <> " is not a record"
    let keyType = fmap LF.tplKeyType (LF.tplKey tpl)
    pure ResolvedTemplate
        { rtPackageId = pid
        , rtModule    = modName
        , rtEntity    = entName
        , rtWorld     = world
        , rtTemplate  = tpl
        , rtFields    = fields
        , rtKeyType   = keyType
        }
  where
    lookupPkg :: Either String (LF.PackageId, LF.Package)
    lookupPkg = case ref of
        TRPackageId pid _ _ ->
            case find ((pid ==) . LF.extPackageId) (LF.getWorldImported world) of
                Just p  -> Right (pid, LF.extPackagePkg p)
                Nothing -> Left $ "package-id " <> T.unpack (LF.unPackageId pid) <> " not in world"
        TRPackageName name _ _ ->
            let matches =
                    [ (LF.extPackageId ep, LF.extPackagePkg ep)
                    | ep <- LF.getWorldImported world
                    , LF.unPackageName (LF.packageName (LF.packageMetadata (LF.extPackagePkg ep))) == name
                    ]
            in case sortOn (Down . packageVersionKey . snd) matches of
                (top : _) -> Right top
                []        -> Left $ "no package named '" <> T.unpack name <> "' in world"

    packageVersionKey :: LF.Package -> T.Text
    packageVersionKey = LF.unPackageVersion . LF.packageVersion . LF.packageMetadata

    showModName (LF.ModuleName xs) = T.unpack (T.intercalate "." xs)
    showTpl     (LF.TypeConName xs) = T.unpack (T.intercalate "." xs)

--------------------------------------------------------------------------------
-- Value encoding
--------------------------------------------------------------------------------

-- | Encode a JSON value against an expected Daml-LF type.
--
-- The accepted JSON shape mirrors the JSON API's "compressed" codec:
--
-- * @Int64@, @Numeric n@              — JSON string or number
-- * @Text@, @Party@, @ContractId T@   — JSON string
-- * @Bool@                            — JSON @true@ / @false@
-- * @Unit@                            — JSON @{}@ or @null@
-- * @Date@                            — @"YYYY-MM-DD"@
-- * @Timestamp@                       — RFC 3339 (@"2026-01-01T00:00:00Z"@)
-- * @Optional a@                      — @null@ to mean nothing, else wrap
-- * @List a@                          — JSON array
-- * @TextMap a@                       — JSON object
-- * @GenMap k v@                      — JSON array of @[k, v]@ pairs
-- * record (@TCon@ → record)          — JSON object keyed by field name
-- * variant (@TCon@ → variant)        — @{ "tag": "Ctor", "value": v }@
-- * enum    (@TCon@ → enum)           — JSON string of constructor name
parseValueAsType :: LF.World -> LF.Type -> A.Value -> Either String VP.Value
parseValueAsType world ty0 j0 = wrap <$> go ty0 j0
  where
    wrap vs = VP.Value { VP.valueSum = Just vs }

    go :: LF.Type -> A.Value -> Either String VP.ValueSum
    go ty j = case ty of
        LF.TBuiltin LF.BTUnit      -> goUnit j
        LF.TBuiltin LF.BTBool      -> goBool j
        LF.TBuiltin LF.BTInt64     -> goInt64 j
        LF.TBuiltin LF.BTText      -> goText j
        LF.TBuiltin LF.BTParty     -> goParty j
        LF.TBuiltin LF.BTDate      -> goDate j
        LF.TBuiltin LF.BTTimestamp -> goTimestamp j
        LF.TApp (LF.TBuiltin LF.BTNumeric)    _     -> goNumeric j
        LF.TApp (LF.TBuiltin LF.BTOptional)   inner -> goOptional inner j
        LF.TApp (LF.TBuiltin LF.BTList)       inner -> goList inner j
        LF.TApp (LF.TBuiltin LF.BTTextMap)    inner -> goTextMap inner j
        LF.TApp (LF.TApp (LF.TBuiltin LF.BTGenMap) k) v -> goGenMap k v j
        LF.TApp (LF.TBuiltin LF.BTContractId) _     -> goContractId j
        LF.TBuiltin LF.BTContractId                 -> goContractId j
        LF.TCon _                                   -> goTCon ty [] j
        LF.TApp _ _                                 -> goTCon ty [] j
        LF.TSynApp _ _ -> Left "type synonyms should have been reduced before encoding"
        _ -> Left $ "unsupported Daml-LF type for JSON encoding: " <> show ty

    -- Unit
    goUnit A.Null = Right (VP.ValueSumUnit Google.Protobuf.Empty.Empty)
    goUnit (A.Object o) | AKM.null o = Right (VP.ValueSumUnit Google.Protobuf.Empty.Empty)
    goUnit x = Left $ "expected {} or null for Unit, got " <> describeJson x

    goBool (A.Bool b) = Right (VP.ValueSumBool b)
    goBool x = Left $ "expected Bool, got " <> describeJson x

    goInt64 :: A.Value -> Either String VP.ValueSum
    goInt64 j = case j of
        A.String s -> case reads (T.unpack s) :: [(Integer, String)] of
            [(n, "")] | inRange64 n -> Right (VP.ValueSumInt64 (fromInteger n))
            _ -> Left $ "invalid Int64 string: " <> T.unpack s
        A.Number n -> case floatingOrInteger n :: Either Double Integer of
            Right i | inRange64 i -> Right (VP.ValueSumInt64 (fromInteger i))
            _ -> Left $ "number does not fit in Int64: " <> show n
        _ -> Left $ "expected Int64, got " <> describeJson j

    goText (A.String s) = Right (VP.ValueSumText (TL.fromStrict s))
    goText x = Left $ "expected Text, got " <> describeJson x

    goParty (A.String s) = Right (VP.ValueSumParty (TL.fromStrict s))
    goParty x = Left $ "expected Party, got " <> describeJson x

    goDate (A.String s) = case parseIsoDate (T.unpack s) of
        Just d  -> Right (VP.ValueSumDate d)
        Nothing -> Left $ "expected Date (YYYY-MM-DD), got " <> T.unpack s
    goDate x = Left $ "expected Date, got " <> describeJson x

    goTimestamp (A.String s) = case parseIsoTimestamp (T.unpack s) of
        Just micros -> Right (VP.ValueSumTimestamp micros)
        Nothing -> Left $ "expected RFC 3339 Timestamp, got " <> T.unpack s
    goTimestamp x = Left $ "expected Timestamp, got " <> describeJson x

    goContractId (A.String s) = Right (VP.ValueSumContractId (TL.fromStrict s))
    goContractId x = Left $ "expected ContractId, got " <> describeJson x

    goNumeric (A.String s) = Right (VP.ValueSumNumeric (TL.fromStrict s))
    goNumeric (A.Number n) = Right (VP.ValueSumNumeric (TL.pack (showNumeric n)))
    goNumeric x = Left $ "expected Numeric, got " <> describeJson x

    goOptional _ A.Null = Right (VP.ValueSumOptional (VP.Optional Nothing))
    goOptional inner j = do
        v <- parseValueAsType world inner j
        Right (VP.ValueSumOptional (VP.Optional (Just v)))

    goList inner (A.Array xs) = do
        vs <- traverse (parseValueAsType world inner) (V.toList xs)
        Right (VP.ValueSumList (VP.List (V.fromList vs)))
    goList _ x = Left $ "expected List, got " <> describeJson x

    goTextMap inner (A.Object o) = do
        entries <- traverse (\(k, vjson) -> do
            val <- parseValueAsType world inner vjson
            pure VP.TextMap_Entry
                { VP.textMap_EntryKey   = TL.fromStrict (AK.toText k)
                , VP.textMap_EntryValue = Just val
                })
            (AKM.toList o)
        Right (VP.ValueSumTextMap (VP.TextMap (V.fromList entries)))
    goTextMap _ x = Left $ "expected TextMap (JSON object), got " <> describeJson x

    goGenMap kTy vTy (A.Array pairs) = do
        entries <- traverse (\p -> case p of
            A.Array av | V.length av == 2 -> do
                key <- parseValueAsType world kTy (av V.! 0)
                val <- parseValueAsType world vTy (av V.! 1)
                pure VP.GenMap_Entry
                    { VP.genMap_EntryKey   = Just key
                    , VP.genMap_EntryValue = Just val
                    }
            _ -> Left $ "expected GenMap entry as [k, v] array, got " <> describeJson p)
            (V.toList pairs)
        Right (VP.ValueSumGenMap (VP.GenMap (V.fromList entries)))
    goGenMap _ _ x = Left $ "expected GenMap (JSON array of [k,v]), got " <> describeJson x

    -- Reference to a named data type, possibly applied to type arguments.
    goTCon :: LF.Type -> [LF.Type] -> A.Value -> Either String VP.ValueSum
    goTCon (LF.TApp f x) args j = goTCon f (x : args) j
    goTCon (LF.TCon qualTcon) args j = do
        dt <- first show (LF.lookupDataType qualTcon world)
        let params = LF.dataParams dt
        when (length params /= length args) $
            Left $ "type-param arity mismatch for " <> show qualTcon <>
                   ": expected " <> show (length params) <> ", got " <> show (length args)
        let subst = foldr (\((v, _), t) acc -> LFSubst.typeSubst v t <> acc) mempty (zip params args)
            cons  = LF.dataCons dt
            ident = qualifiedToIdentifier qualTcon
        case cons of
            LF.DataRecord fs -> do
                obj <- asObject j
                let fs' = [ (fn, LFSubst.applySubstInType subst ft) | (fn, ft) <- fs ]
                rfs <- foldM (\acc (LF.FieldName fn, ft) -> do
                    case AKM.lookup (AK.fromText fn) obj of
                        Just jv -> do
                            val <- parseValueAsType world ft jv
                            pure $ VP.RecordField
                                { VP.recordFieldLabel = TL.fromStrict fn
                                , VP.recordFieldValue = Just val
                                } : acc
                        Nothing -> Left $ "record " <> show qualTcon <>
                                          " missing field '" <> T.unpack fn <> "'")
                    [] fs'
                Right (VP.ValueSumRecord VP.Record
                    { VP.recordRecordId = Just ident
                    , VP.recordFields = V.fromList (reverse rfs)
                    })
            LF.DataVariant variants -> do
                (tag, mPayload) <- parseVariantTag j
                ctorTy <- maybeToEither
                    ("variant " <> show qualTcon <> " has no constructor '" <> T.unpack tag <> "'")
                    (lookup (LF.VariantConName tag) variants)
                let ctorTy' = LFSubst.applySubstInType subst ctorTy
                payloadVal <- case mPayload of
                    Just jp -> parseValueAsType world ctorTy' jp
                    Nothing -> parseValueAsType world ctorTy' (A.Object AKM.empty)
                Right (VP.ValueSumVariant VP.Variant
                    { VP.variantVariantId   = Just ident
                    , VP.variantConstructor = TL.fromStrict tag
                    , VP.variantValue       = Just payloadVal
                    })
            LF.DataEnum ctors -> do
                s <- case j of
                    A.String t -> Right t
                    _ -> Left $ "enum " <> show qualTcon <> " expects a JSON string"
                unless (LF.VariantConName s `elem` ctors) $
                    Left $ "enum " <> show qualTcon <> " has no constructor '" <> T.unpack s <> "'"
                Right (VP.ValueSumEnum VP.Enum
                    { VP.enumEnumId       = Just ident
                    , VP.enumConstructor  = TL.fromStrict s
                    })
            LF.DataInterface ->
                Left $ "cannot construct a value of interface type: " <> show qualTcon
    goTCon other _ _ = Left $ "cannot encode value for type: " <> show other

    asObject (A.Object o) = Right o
    asObject x            = Left $ "expected JSON object, got " <> describeJson x

parseVariantTag :: A.Value -> Either String (T.Text, Maybe A.Value)
parseVariantTag (A.Object o) = case (AKM.lookup "tag" o, AKM.lookup "value" o) of
    (Just (A.String t), Just v)  -> Right (t, Just v)
    (Just (A.String t), Nothing) -> Right (t, Nothing)
    _ -> Left "variant expects {\"tag\": \"Ctor\", \"value\": ...}"
parseVariantTag x = Left $ "variant expects JSON object, got " <> describeJson x

qualifiedToIdentifier :: LF.Qualified LF.TypeConName -> VP.Identifier
qualifiedToIdentifier (LF.Qualified selfOrImp (LF.ModuleName mods) (LF.TypeConName ent)) =
    VP.Identifier
        { VP.identifierPackageId =
            TL.fromStrict $ case selfOrImp of
                LF.ImportedPackageId (LF.PackageId p) -> p
                LF.SelfPackageId -> ""
        , VP.identifierModuleName = TL.fromStrict (T.intercalate "." mods)
        , VP.identifierEntityName = TL.fromStrict (T.intercalate "." ent)
        }

--------------------------------------------------------------------------------
-- buildCreateRecord: given resolved template + JSON object → proto Record
--------------------------------------------------------------------------------

buildCreateRecord :: ResolvedTemplate -> A.Value -> Either String VP.Record
buildCreateRecord rt j = do
    val <- parseValueAsType (rtWorld rt) templateTy j
    case VP.valueSum val of
        Just (VP.ValueSumRecord r) -> Right r
        _ -> Left "internal: buildCreateRecord expected a record value"
  where
    templateTy = LF.TCon (LF.Qualified
                            (LF.ImportedPackageId (rtPackageId rt))
                            (rtModule rt)
                            (rtEntity rt))

--------------------------------------------------------------------------------
-- k=v sugar
--------------------------------------------------------------------------------

-- | Either user-supplied JSON, or a list of `name=value` pairs to be
-- translated against the expected record type.
data ArgsInput
    = AIJson A.Value
    | AIPairs [(String, String)]
    deriving Show

-- | Produce an Aeson value suitable for 'parseValueAsType' against a record
-- type. For JSON input, returns it verbatim. For key=value pairs, builds a
-- flat JSON object; each value is coerced based on the target field type.
encodeArgsInput :: [(LF.FieldName, LF.Type)] -> ArgsInput -> Either String A.Value
encodeArgsInput _ (AIJson j) = Right j
encodeArgsInput fields (AIPairs pairs) = do
    when (hasDuplicates (map fst pairs)) $
        Left "duplicate --arg/--create-arg/--choice-arg key"
    entries <- traverse coerce pairs
    pure $ A.Object (AKM.fromList [ (AK.fromText k, v) | (k, v) <- entries ])
  where
    coerce :: (String, String) -> Either String (T.Text, A.Value)
    coerce (k, rawV) = do
        let keyT = T.pack k
        case lookup (LF.FieldName keyT) fields of
            Nothing -> Left $ "unknown field '" <> k <> "' — known fields: " <>
                show [ T.unpack n | (LF.FieldName n, _) <- fields ]
            Just t  -> do
                v <- coerceFlat t (T.pack rawV)
                pure (keyT, v)

    hasDuplicates xs = any ((> 1) . length) (group (sort xs))

-- | Coerce a raw shell token into a JSON value appropriate for a given
-- primitive LF type. Returns an error if the target type isn't flat.
coerceFlat :: LF.Type -> T.Text -> Either String A.Value
coerceFlat t raw = case t of
    LF.TBuiltin LF.BTBool -> case T.toLower raw of
        "true"  -> Right (A.Bool True)
        "false" -> Right (A.Bool False)
        _ -> Left $ "expected 'true' or 'false' for Bool, got " <> T.unpack raw
    LF.TBuiltin LF.BTInt64     -> Right (A.String raw)
    LF.TApp (LF.TBuiltin LF.BTNumeric) _ -> Right (A.String raw)
    LF.TBuiltin LF.BTText      -> Right (A.String raw)
    LF.TBuiltin LF.BTParty     -> Right (A.String raw)
    LF.TBuiltin LF.BTDate      -> Right (A.String raw)
    LF.TBuiltin LF.BTTimestamp -> Right (A.String raw)
    LF.TApp (LF.TBuiltin LF.BTContractId) _ -> Right (A.String raw)
    LF.TApp (LF.TBuiltin LF.BTOptional) inner
      | T.null raw -> Right A.Null
      | otherwise  -> coerceFlat inner raw
    _ -> Left $ "--arg sugar supports only flat-record templates (int/text/party/bool/date/timestamp/contract-id/optional-of-those); field type: " <> show t

--------------------------------------------------------------------------------
-- Date / timestamp parsing
--------------------------------------------------------------------------------

-- | Parse @YYYY-MM-DD@ into days since 1970-01-01 (proto Date is Int32).
parseIsoDate :: String -> Maybe Int32
parseIsoDate s = case s of
    (y1:y2:y3:y4:'-':m1:m2:'-':d1:d2:[])
      | all isDigit [y1,y2,y3,y4,m1,m2,d1,d2] ->
          let y = read [y1,y2,y3,y4]
              m = read [m1,m2]
              d = read [d1,d2]
          in if validDate y m d
                 then Just (fromIntegral (daysFromCivil y m d))
                 else Nothing
    _ -> Nothing
  where
    validDate y m d = y >= 1 && m >= 1 && m <= 12 && d >= 1 && d <= 31

-- | Days from 1970-01-01 using Hinnant's civil-from-ymd algorithm.
daysFromCivil :: Int -> Int -> Int -> Int
daysFromCivil y0 m d =
    let y = y0 - if m <= 2 then 1 else 0
        era = (if y >= 0 then y else y - 399) `div` 400
        yoe = y - era * 400
        doy = (153 * (m + (if m > 2 then -3 else 9)) + 2) `div` 5 + d - 1
        doe = yoe * 365 + yoe `div` 4 - yoe `div` 100 + doy
    in era * 146097 + doe - 719468

-- | Parse an RFC 3339 timestamp into microseconds since epoch (proto Timestamp is Int64).
-- Accepts: YYYY-MM-DDTHH:MM:SS(.ffffff)?Z
parseIsoTimestamp :: String -> Maybe Int64
parseIsoTimestamp s = case s of
    (y1:y2:y3:y4:'-':m1:m2:'-':d1:d2:'T':hh1:hh2:':':mi1:mi2:':':ss1:ss2:rest)
      | all isDigit [y1,y2,y3,y4,m1,m2,d1,d2,hh1,hh2,mi1,mi2,ss1,ss2] -> do
          let days  = daysFromCivil (read [y1,y2,y3,y4]) (read [m1,m2]) (read [d1,d2])
              hour  = read [hh1,hh2] :: Int
              minu  = read [mi1,mi2] :: Int
              sec   = read [ss1,ss2] :: Int
          (frac, tz) <- splitFrac rest
          ensure (hour < 24 && minu < 60 && sec < 60)
          case tz of
              "Z" -> Just $ fromInteger (toMicros days hour minu sec frac)
              _   -> Nothing
    _ -> Nothing
  where
    splitFrac ('.':xs) = do
        let (digs, rest) = span isDigit xs
        let scaled = take 6 (digs <> repeat '0')
        Just (read scaled :: Integer, rest)
    splitFrac xs = Just (0, xs)

    ensure True  = Just ()
    ensure False = Nothing

    toMicros days hour minu sec frac =
        fromIntegral days * 86_400_000_000
        + fromIntegral hour * 3_600_000_000
        + fromIntegral minu * 60_000_000
        + fromIntegral sec  * 1_000_000
        + frac

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

describeJson :: A.Value -> String
describeJson = \case
    A.Null     -> "null"
    A.Bool _   -> "boolean"
    A.Number _ -> "number"
    A.String _ -> "string"
    A.Array _  -> "array"
    A.Object _ -> "object"

showNumeric :: Scientific -> String
showNumeric = show

inRange64 :: Integer -> Bool
inRange64 n = n >= -9_223_372_036_854_775_808 && n <= 9_223_372_036_854_775_807

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither e Nothing  = Left e
maybeToEither _ (Just x) = Right x
