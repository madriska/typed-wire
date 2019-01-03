{-# LANGUAGE FlexibleContexts #-}
module TW.Check where

import TW.Ast
import TW.BuiltIn

import Control.Monad.Except
import qualified Data.Map as M

data DefinedType
   = DefinedType
   { dt_name :: QualTypeName
   , dt_args :: [TypeVar]
   } deriving (Show, Eq)

builtInToDefTy :: BuiltIn -> DefinedType
builtInToDefTy bi =
    DefinedType
    { dt_name = bi_name bi
    , dt_args = bi_args bi
    }

typeDefToDefTy :: Maybe ModuleName -> TypeDef -> DefinedType
typeDefToDefTy qualOwner td =
    let mkTy =
            case qualOwner of
              Nothing -> QualTypeName (ModuleName [])
              Just qt -> QualTypeName qt
    in case td of
         TypeDefEnum ed ->
             DefinedType (mkTy (ed_name ed)) (ed_args ed)
         TypeDefStruct sd ->
             DefinedType (mkTy (sd_name sd)) (sd_args sd)

checkModules :: [Module] -> Either String [Module]
checkModules modules =
    runExcept $
    forM modules $ \m ->
    do let currentMStr = printModuleNameS $ m_name m
       defTypes <-
           M.fromList . map (\dt -> (dt_name dt, dt_args dt)) <$>
           getDefinedTypes m
       let isValidType args t =
               case t of
                 TyVar tv ->
                     unless (tv `elem` args) $
                     throwError $ "Undefined type variable " ++ show tv ++ " in " ++ currentMStr
                 TyCon qt qtArgs ->
                     case M.lookup qt defTypes of
                       Nothing ->
                           throwError $ "Undefined type variable " ++ show qt ++ " in " ++ currentMStr
                       Just tvars ->
                           do forM_ qtArgs (isValidType args)
                              when (length tvars /= length qtArgs) $
                                   throwError $
                                   "Type " ++ show qt ++ " got applied wrong number of arguments in " ++ currentMStr
           checkRoute r =
               case r of
                 ApiRouteDynamic t ->
                   unless (isPathPiece t) $
                   throwError $
                   "Invalid route parameter " ++ show t ++ ". Route parameters can only be primitive types!"
                 _ -> return ()
       forM_ (m_typeDefs m) $ \td ->
           case td of
             TypeDefEnum ed ->
                 forM_ (ed_choices ed) $ \ch ->
                 forM_ (ec_arg ch) (isValidType (ed_args ed))
             TypeDefStruct sd ->
                 forM_ (sd_fields sd) $ \fld ->
                 isValidType (sd_args sd) (sf_type fld)
       forM_ (m_apis m) $ \api ->
          forM_ (ad_endpoints api) $ \ep ->
          do mapM_ (isValidType []) (aed_req ep)
             isValidType [] (aed_resp ep)
             mapM_ checkRoute (aed_route ep)
       return m
    where
      getDefinedTypes m =
          do importedTypes <-
                 forM (m_imports m) $ \im ->
                 case im of
                   NativeImport{} -> return []
                   TWImport twImportName ->
                     case M.lookup twImportName moduleMap of
                       Nothing ->
                           throwError $
                           "Unknown module " ++ printModuleNameS twImportName
                           ++ " referenced from " ++ (printModuleNameS $ m_name m)
                       Just imModel ->
                           return $ map (typeDefToDefTy (Just twImportName)) $ m_typeDefs imModel
             return $ concat importedTypes
                        ++ (map (typeDefToDefTy Nothing) $ m_typeDefs m)
                        ++ map builtInToDefTy allBuiltIns
      moduleMap =
          M.fromList $
          map (\m -> (m_name m, m)) modules
