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
  
instance FromField [Text] where
  fromField f@(Field _ _ (Oid 1009)) (Just fdata) = case parse1DArray $ T.decodeUtf8 fdata of
    Just ary -> return ary
    Nothing -> returnError Incompatible f "Field is not a valid string list."
  fromField (Field _ _ (Oid 1009)) Nothing = return []
  fromField f _ = returnError Incompatible f "Field is not a valid string list."
  
instance ToField [Text] where
  toField v  = Many [plain "ARRAY[", csList v, plain "]"]
    where
      plain = Plain . fromByteString
      csList = Many . intersperse (plain ",") . map (Escape . T.encodeUtf8)

parse1DArray :: Text -> Maybe [Text]
parse1DArray = fmap reverse . maybeResult . A.parse arrayParser

arrayParser :: Parser [Text]
arrayParser = do
  char '{'
  v <- arrayParser' []
  char '}'
  return v

arrayParser' :: [Text] -> Parser [Text]
arrayParser' accum = do
  skipWhile skipable
  v <- takeTill skipable
  skipWhile skipable
  nextChar <- peekChar
  case nextChar of
    Just '}' -> return (v:accum)
    Nothing -> return (v:accum)
    _ -> arrayParser' (v:accum)
  where
    skipable v = (v == ',') || (v == '\"')