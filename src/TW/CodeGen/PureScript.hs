{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TW.CodeGen.PureScript
    ( makeFileName, makeModule
    , libraryInfo
    )
where

import TW.Ast
import TW.BuiltIn
import TW.JsonRepr
import TW.Types
import TW.Utils

import Data.Maybe
import Data.Monoid
import System.FilePath
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T

libraryInfo :: LibraryInfo
libraryInfo = LibraryInfo "Purescript" "purescript-typed-wire" "0.2.0"

makeFileName :: ModuleName -> FilePath
makeFileName (ModuleName parts) =
    (L.foldl' (</>) "" $ map T.unpack parts) ++ ".purs"

makeModule :: Module -> T.Text
makeModule m =
    T.unlines
    [ "module " <> printModuleName (m_name m) <> " where"
    , ""
    , T.intercalate "\n" (map makeImport $ m_imports m)
    , ""
    , "import Data.TypedWire.Prelude"
    , "import Data.Set as Set"
    , "import Data.Map as Map"
    , if not (null (m_apis m)) then "import Data.TypedWire.Api" else ""
    , ""
    , T.intercalate "\n" (map makeTypeDef $ m_typeDefs m)
    , T.intercalate "\n" (map makeApiDef $ m_apis m)
    -- Add any extra code from 'ModuleName.extra.purs'
    , M.findWithDefault "" "purs" (m_extraCode m)
    ]

makeApiDef :: ApiDef -> T.Text
makeApiDef ad =
    T.unlines $
    catMaybes
    [ apiHeader
    , Just $ T.intercalate "\n" (map makeEndPoint (ad_endpoints ad))
    ]
    where
      apiHeader =
        case not (null (ad_headers ad)) of
          True -> Just $ makeHeaderType apiHeaderType (ad_headers ad)
          False -> Nothing
      apiCapitalized = capitalizeText (unApiName $ ad_name ad)
      handlerType = "ApiHandler" <> apiCapitalized
      apiHeaderType = handlerType <> "Headers"
      headerType ep = handlerType <> capitalizeText (unEndpointName (aed_name ep)) <> "Headers"
      makeHeaderName hdr = uncapitalizeText $ makeSafePrefixedFieldName (ah_name hdr)
      makeHeaderType ty headers =
        T.unlines
        [ "type " <> ty <> " = "
        , "    { " <> T.intercalate "\n    , " (map makeHeaderField headers)
        , "    }"
        ]
      makeHeaderField hdr =
        makeHeaderName hdr <> " :: String"
      makeEndPoint ep =
        T.unlines $
        catMaybes
        [ epHeader
        , Just $ funName <> " :: forall m. (Monad m) => "
          <> (maybe "" (const $ apiHeaderType <> " -> ") apiHeader)
          <> (maybe "" (const $ headerType ep <> " -> ") epHeader)
          <> urlParamSig
          <> (maybe "" (\t -> makeType t <> " -> ") $ aed_req ep)
          <> "ApiCall m " <> (maybe "Unit" makeType $ aed_req ep) <> " " <> makeType (aed_resp ep)
        , Just $ funName
          <> " "
          <> (maybe "" (const "apiHeaders ") apiHeader)
          <> (maybe "" (const "endpointHeaders ") epHeader)
          <> urlParams
          <> (maybe "" (const "reqBody ") $ aed_req ep)
          <> "runRequest = do"
        , Just $ "    let coreHeaders = [" <> T.intercalate ", " (map (headerPacker "apiHeaders") $ ad_headers ad) <> "]"
        , Just $ "    let fullHeaders = coreHeaders <> [" <> T.intercalate ", " (map (headerPacker "endpointHeaders") $ aed_headers ep) <> "]"
        , Just $ "    let url = " <> T.intercalate " <> \"/\" <> " (map urlPacker routeInfo)
        , Just $ "    let method = " <> T.pack (show $ aed_verb ep)
        , Just $ "    let body = " <> (maybe "Nothing" (const "Just $ encodeJson reqBody") $ aed_req ep)
        , Just $ "    let req = { headers: fullHeaders, method: method, body: body, url: url }"
        , Just $ "    resp <- runRequest req"
        , Just $ "    return $ if (resp.statusCode /= 200) then Left \"Return code was not 200\" else decodeJson resp.body"
        ]
        where
          urlPacker (r, p) =
            case r of
              ApiRouteStatic t -> T.pack (show t)
              ApiRouteDynamic _ -> "toPathPiece p" <> T.pack (show p) <> ""
          headerPacker apiVar hdr =
             "{ key: " <> T.pack (show $ ah_name hdr) <> ", value: " <> apiVar <> "." <> makeHeaderName hdr <> " }"
          funName = unApiName (ad_name ad) <> capitalizeText (unEndpointName $ aed_name ep)
          routeInfo = zip (aed_route ep) ([0..] :: [Int])
          urlParams =
            T.concat $ flip mapMaybe routeInfo $ \(r,p) ->
            case r of
              ApiRouteStatic _ -> Nothing
              ApiRouteDynamic _ -> Just $ "p" <> T.pack (show p) <> " "
          urlParamSig =
            T.concat $ flip mapMaybe (aed_route ep) $ \r ->
            case r of
              ApiRouteStatic _ -> Nothing
              ApiRouteDynamic ty -> Just (makeType ty <> " -> ")
          epHeader =
            case not (null (aed_headers ep)) of
              True -> Just $ makeHeaderType (headerType ep) (aed_headers ep)
              False -> Nothing

makeImport :: Import -> T.Text
makeImport NativeImport{importName} = "import " <> printModuleName importName
-- makeImport NativeImport{importName, qualifiedName=Just q} =
--   "import qualified " <> printModuleName importName <> " as " <> printModuleName q
-- Qualify the imports (import A.B as A.B) so we can refer to A.B.Foobar.
-- TODO de-qualify the import
makeImport TWImport{importName} = "import " <> printModuleName importName <> " as " <> printModuleName importName

makeTypeDef :: TypeDef -> T.Text
makeTypeDef td =
    case td of
      TypeDefEnum ed ->
          makeEnumDef ed
      TypeDefStruct sd ->
          makeStructDef sd
      TypeDefNewtype nd ->
          makeNewtypeDef nd

decoderName :: TypeName -> T.Text
decoderName ty = "dec" <> unTypeName ty

encoderName :: TypeName -> T.Text
encoderName ty = "enc" <> unTypeName ty

showName :: TypeName -> T.Text
showName ty = "show" <> unTypeName ty

-- | Full type name, including any type parameters.
fullType :: TypeName -> [TypeVar] -> T.Text
fullType name vars =
  T.strip $ unTypeName name <> " " <> T.intercalate " " (map unTypeVar vars)

deriveInstance, deriveNewtypeInstance :: T.Text -> TypeName -> [TypeVar] -> T.Text
deriveInstance = deriveAnyInstance ""
deriveNewtypeInstance = deriveAnyInstance "newtype"

deriveAnyInstance :: T.Text -> T.Text -> TypeName -> [TypeVar] -> T.Text
deriveAnyInstance qual cls name vars =
  T.unwords [ "derive", qual, "instance", instanceName, "::", cls
            , parenthesize typeConstructor]
  where
    instanceName = T.toLower cls <> unTypeName name
    typeConstructor = fullType name vars

-- | As 'deriveInstance', but with an extra inferred type parameter. Used for
-- Generic and Newtype, which have an inferrable parameter (respectively, the
-- 'rep' and the wrapped type) in the final position.
deriveInstance1 :: T.Text -> TypeName -> [TypeVar] -> T.Text
deriveInstance1 c n a = deriveInstance c n a <> " _"

-- | Generate bare newtypes for 'newtype' declarations.
makeNewtypeDef :: NewtypeDef -> T.Text
makeNewtypeDef NewtypeDef{nd_name, nd_args, nd_field} =
  T.unlines
  [ T.unwords ["newtype", typeConstructor, "=", dataConstructor, wrappedType ]
  , deriveInstance "Eq" nd_name nd_args
  , deriveInstance "Ord" nd_name nd_args
  , deriveInstance1 "Newtype" nd_name nd_args
  , deriveNewtypeInstance "DecodeJson" nd_name nd_args
  , deriveNewtypeInstance "EncodeJson" nd_name nd_args
  , T.unwords [ "instance", showInstanceName
              , "::", "Show", parenthesize typeConstructor, "where" ]
    -- show (C x) = "(C " <> show x <> ")"
  , "  show (" <> dataConstructor <> " x) = \"(" <> dataConstructor <> " \" <> show x <> \")\""
  , ""
  -- The eliminator is taken from the struct name:
  --   newtype Foo { unFoo: Int; }
  , T.unwords [ eliminator, "::", typeConstructor, "->", wrappedType ]
  , T.unwords [ eliminator, "(" <> dataConstructor <> " wrapped) = wrapped"]
  ]
  where
    eliminator = unFieldName $ sf_name nd_field
    typeConstructor = parenthesize $ fullType nd_name nd_args
    wrappedType = parenthesize $ makeType (sf_type nd_field)
    dataConstructor = unTypeName nd_name
    showInstanceName = "show" <> dataConstructor

makeStructDef :: StructDef -> T.Text
makeStructDef StructDef{sd_name, sd_args, sd_fields} =
    T.unlines
    [ "data " <> fullType sd_name sd_args
    , "   = " <> justType
    , "   { " <> T.intercalate "\n   , " (map makeStructField sd_fields)
    , "   }"
    , ""
    , deriveInstance "Eq" sd_name sd_args
    , deriveInstance "Ord" sd_name sd_args
    , deriveInstance1 "Generic" sd_name sd_args
    , "instance " <> showName sd_name <> " :: "
      <> tcPreds sd_args ["Show"] <> "Show (" <> fullType sd_name sd_args <> ") where "
      <> "show (" <> justType <> " a) = " <> T.pack (show justType) <> " <> \"{\" <> "
      <> T.intercalate " <> \", \" <> " (map makeFieldShow sd_fields)
      <> " <> \"}\""
    , "instance " <> encoderName sd_name <> " :: "
      <> tcPreds sd_args ["EncodeJson"] <> "EncodeJson" <> " (" <> fullType sd_name sd_args <> ") where"
    , "    encodeJson (" <> unTypeName sd_name <> " objT) ="
    , "        " <> T.intercalate "\n         ~> " (map makeToJsonFld sd_fields)
    , "        ~> jsonEmptyObject"
    , "instance " <> decoderName sd_name <> " :: "
      <> tcPreds sd_args ["DecodeJson"] <> "DecodeJson" <> " (" <> fullType sd_name sd_args <> ") where"
    , "    decodeJson jsonT = do"
    , "        objT <- decodeJson jsonT"
    , "        " <> T.intercalate "\n        " (map makeFromJsonFld sd_fields)
    , "        pure $ " <> unTypeName sd_name <> " { " <> T.intercalate ", " (map makeFieldSetter sd_fields) <> " }"
    ]
    where
      makeFieldShow fld =
          let name = unFieldName $ sf_name fld
          in  T.pack (show name) <> " <> \": \" <> show a." <> name
      makeFieldSetter fld =
          let name = unFieldName $ sf_name fld
          in name <> " : " <> "v" <> name
      makeFromJsonFld fld =
          let name = unFieldName $ sf_name fld
          in case sf_type fld of
               (TyCon q _) | q == bi_name tyMaybe ->
                  "v" <> name <> " <- objT .:? " <> T.pack (show name)
               _ ->
                  "v" <> name <> " <- objT .: " <> T.pack (show name)
      makeToJsonFld fld =
          let name = unFieldName $ sf_name fld
          in T.pack (show name) <> " " <> ":=" <> " objT." <> name
      justType = unTypeName sd_name

makeStructField :: StructField -> T.Text
makeStructField sf =
    unFieldName (sf_name sf) <> " :: " <> makeType (sf_type sf)

tcPreds :: [TypeVar] -> [T.Text] -> T.Text
tcPreds args tyClasses =
    if null args
    then ""
    else let mkPred (TypeVar tv) =
                 T.intercalate "," $ flip map tyClasses $ \tyClass -> tyClass <> " " <> tv
         in "(" <> T.intercalate "," (map mkPred args) <> ") => "

makeEnumDef :: EnumDef -> T.Text
makeEnumDef EnumDef{ed_name, ed_args, ed_choices} =
    T.unlines
    [ "data " <> fullType ed_name ed_args
    , "   = " <> T.intercalate "\n   | " (map makeEnumChoice ed_choices)
    , ""
    , deriveInstance "Eq" ed_name ed_args
    , deriveInstance "Ord" ed_name ed_args
    , deriveInstance1 "Generic" ed_name ed_args
    , "instance " <> showName ed_name <> " :: "
      <> tcPreds ed_args ["Show"] <> "Show (" <> fullType ed_name ed_args <> ") where "
    , "    " <> T.intercalate "\n    " (map makeChoiceShow ed_choices)
    , "instance " <> encoderName ed_name <> " :: "
      <> tcPreds ed_args ["EncodeJson"] <> "EncodeJson" <> " (" <> fullType ed_name ed_args <> ") where"
    , "    encodeJson x ="
    , "        case x of"
    , "          " <> T.intercalate "\n          " (map mkToJsonChoice ed_choices)
    , "instance " <> decoderName ed_name <> " :: "
      <> tcPreds ed_args ["DecodeJson"] <> "DecodeJson" <> " (" <> fullType ed_name ed_args <> ") where"
    , "    decodeJson jsonT ="
    , "        decodeJson jsonT >>= \\objT -> "
    , "        " <> T.intercalate "\n        <|> " (map mkFromJsonChoice ed_choices)
    ]
    where
      makeChoiceShow ec =
          let constr = unChoiceName $ ec_name ec
          in case ec_arg ec of
                 Nothing -> "show (" <> constr <> ") = " <> T.pack (show constr)
                 Just _ -> "show (" <> constr <> " a) = " <> T.pack (show constr) <> " <> \" \" <> show a"
      mkFromJsonChoice ec =
          let constr = unChoiceName $ ec_name ec
              tag = camelTo2 '_' $ T.unpack constr
              (op, opEnd) =
                  case ec_arg ec of
                    Nothing -> ("<$ (eatBool <$> (", "))")
                    Just _ -> ("<$>", "")
          in "(" <> constr <> " " <> op <> " objT " <> ".:" <> " " <> T.pack (show tag) <> opEnd <> ")"
      mkToJsonChoice ec =
          let constr = unChoiceName $ ec_name ec
              tag = camelTo2 '_' $ T.unpack constr
              (argParam, argVal) =
                  case ec_arg ec of
                    Nothing -> ("", "true")
                    Just _ -> ("y", "y")
          in constr <> " " <> argParam <> " -> "
             <> " " <> T.pack (show tag) <> " " <> " := " <> " " <> argVal <> " ~> jsonEmptyObject"

makeEnumChoice :: EnumChoice -> T.Text
makeEnumChoice ec =
    (unChoiceName $ ec_name ec) <> fromMaybe "" (fmap ((<>) " " . makeType) $ ec_arg ec)

makeType :: Type -> T.Text
makeType t =
    case isBuiltIn t of
      Nothing ->
          case t of
            TyVar (TypeVar x) -> x
            TyCon qt args ->
                let ty = makeQualTypeName qt
                in case args of
                     [] -> ty
                     _ -> "(" <> ty <> " " <> T.intercalate " " (map makeType args) <> ")"
      Just (bi, tvars)
          | bi == tyString -> "String"
          | bi == tyInt -> "Int"
          | bi == tyBool -> "Boolean"
          | bi == tyFloat -> "Number"
          | bi == tyMaybe -> "(Maybe " <> T.intercalate " " (map makeType tvars) <> ")"
          | bi == tyBytes -> "AsBase64"
          | bi == tyList -> "(Array " <> T.intercalate " " (map makeType tvars) <> ")"
          | bi == tyDateTime -> "DateTime"
          | bi == tyTime -> "TimeOfDay"
          | bi == tyDate -> "Day"
          | bi == tyPair -> "(Tuple " <> T.intercalate " " (map makeType tvars) <> ")"
          | bi == tyMap -> "(Map " <> T.intercalate " " (map makeType tvars) <> ")"
          | bi == tySet -> "(Set " <> T.intercalate " " (map makeType tvars) <> ")"
          | otherwise ->
              error $ "PureScript: Unimplemented built in type: " ++ show t

makeQualTypeName :: QualTypeName -> T.Text
makeQualTypeName qtn =
    case unModuleName $ qtn_module qtn of
      [] -> ty
      _ -> printModuleName (qtn_module qtn) <> "." <> ty
    where
      ty = unTypeName $ qtn_type qtn
