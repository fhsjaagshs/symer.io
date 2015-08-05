{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-do-bind #-}

--
-- This module contains instances for
-- storing arrays in Postgres using
-- postgres-simple
--

module Blog.Database.PGExtensions 
(
  parse1DArray
)
where

import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.List (intersperse)

import           Data.Attoparsec.Text as A

import           Database.PostgreSQL.Simple.FromField as PG.FromField
import           Database.PostgreSQL.Simple.ToField as PG.ToField
import           Blaze.ByteString.Builder (fromByteString)
import           Database.PostgreSQL.Simple.Internal

import Debug.Trace
  
instance FromField [Text] where
  fromField f@(Field _ _ (Oid 1009)) (Just fdata) = case parse1DArray $ T.decodeUtf8 $ traceShowId fdata of
    Right ary -> return ary
    Left err -> returnError Incompatible f $ "Failed to parse list: " ++ err
  fromField (Field _ _ (Oid 1009)) Nothing = return []
  fromField f _ = returnError Incompatible f "Field is not a valid text list."
  
instance ToField [Text] where
  toField v  = Many [plain "ARRAY[", csList v, plain "]"]
    where
      plain = Plain . fromByteString
      csList = Many . intersperse (plain ",") . map (Escape . T.encodeUtf8)

parse1DArray :: Text -> Either String [Text]
parse1DArray = fmap reverse . eitherResult . A.parse arrayParser

arrayParser :: Parser [Text]
arrayParser = do
  char '{'
  v <- arrayParser' []
  char '}'
  return v

arrayParser' :: [Text] -> Parser [Text]
arrayParser' accum = do
  skipWhile skipable
  nextChar <- peekChar
  case nextChar of
    Just '}' -> return accum
    Nothing -> return accum
    _ -> do
      v <- takeTill skipable
      arrayParser' (v:accum)
  where
    skipable v = (v == ',') || (v == '\"')