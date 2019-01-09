{-# LANGUAGE OverloadedStrings #-}
module TW.Utils where

import Data.Char (toUpper, toLower, isAlphaNum)
import qualified Data.Text as T

capitalizeText :: T.Text -> T.Text
capitalizeText =
    T.pack . go . T.unpack
    where
       go (x:xs) = toUpper x : xs
       go [] = []

uncapitalizeText :: T.Text -> T.Text
uncapitalizeText =
    T.pack . go . T.unpack
    where
       go (x:xs) = toLower x : xs
       go [] = []

makeSafePrefixedFieldName :: T.Text -> T.Text
makeSafePrefixedFieldName = T.filter isAlphaNum

parenthesize :: T.Text -> T.Text
parenthesize t
  | " " `T.isInfixOf` t = "(" <> t <> ")"
  | otherwise = t
