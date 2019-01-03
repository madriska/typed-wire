{-# LANGUAGE LambdaCase #-}
module TW.Loader where

import TW.Ast
import TW.Parser

import Control.Monad.Except
import Data.Maybe
import System.Directory
import System.FilePath
import System.FilePath.Glob (glob)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

loadModules :: [FilePath] -> [ModuleName] -> IO (Either String [Module])
loadModules srcDirs entryPoints =
    runExceptT $ loadLoop srcDirs S.empty (S.fromList entryPoints) []

twModuleNames :: [Import] -> [ModuleName]
twModuleNames = mapMaybe $ \case
  TWImport m -> Just m
  _ -> Nothing

loadLoop :: [FilePath] -> S.Set ModuleName -> S.Set ModuleName -> [Module] -> ExceptT String IO [Module]
loadLoop srcDirs visited queue accum =
    case S.toList queue of
      [] -> return accum
      (x:xs) ->
          do (correctModuleName, filePath) <- getModuleFp srcDirs x
             loaded <- loadModuleFp filePath
             unless (correctModuleName (m_name loaded)) $
                    throwError $ "Wrong module name found in " ++ filePath ++ ": "
                               ++ T.unpack (printModuleName $ m_name loaded)
             let visited' = S.insert (m_name loaded) visited
                 queue' = S.fromList (twModuleNames $ m_imports loaded) `S.union` S.fromList xs
             loadLoop srcDirs visited' (queue' `S.difference` visited') (loaded : accum)

getModuleFp :: [FilePath] -> ModuleName -> ExceptT String IO (ModuleName -> Bool, FilePath)
getModuleFp srcDirs m@(ModuleName comps) =
    do let fileName = (L.foldl' (</>) "" $ map T.unpack comps) ++ ".tywi"
       discovered <-
           catMaybes <$>
           (forM srcDirs $ \srcDir ->
            do let fn = srcDir </> fileName
               isThere <- liftIO $ doesFileExist fn
               return $ if isThere then Just fn else Nothing)
       let errPrefix =
               "Can not resolve module " ++ T.unpack (printModuleName m) ++ ": "
       case discovered of
         [] ->
             throwError $ errPrefix ++ fileName ++ " not available in " ++ L.intercalate ", " srcDirs
         [x] ->
             return ((==) m, x)
         _ ->
             throwError $ errPrefix ++ "multiple possibilities found: " ++ L.intercalate ", " discovered

-- | Given a FilePath to a .tywi file, find any corresponding .extra.* files
-- and shovel them into a map by extension. For example:
--
--   Map.fromList [ ("hs", "instance Monoid Foobar where...")
--                , ("purs", "instance monoidFoobar :: Monoid Foobar where...")]
--
loadExtraCode :: FilePath -> IO (M.Map T.Text T.Text)
loadExtraCode fp = do
  files <- glob (fp -<.> "extra.*")
  fmap M.fromList . forM files $ \file -> do
    let extension = T.pack $ tail (takeExtension file) -- tail: drop the leading period
    contents <- T.readFile file
    return (extension, contents)


loadModuleFp :: FilePath -> ExceptT String IO Module
loadModuleFp fp =
    do m <- liftIO $ moduleFromFile fp
       extraCode <- liftIO $ loadExtraCode fp
       case m of
         Left err -> throwError err
         Right ok -> return $ ok { m_extraCode = extraCode }
