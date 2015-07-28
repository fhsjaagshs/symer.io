{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

--
-- This module contains instances for
-- storing arrays in Postgres using
-- postgres-simple
--

module Blog.Database.PGExtensions where

import qualified Data.ByteString.Char8 as B
import           Database.PostgreSQL.Simple.FromField as PG.FromField
import           Database.PostgreSQL.Simple.ToField as PG.ToField
import           Blaze.ByteString.Builder (fromByteString)
import           Database.PostgreSQL.Simple.Internal
  
-- Treats text[] as a String
instance FromField [String] where
  fromField (Field _ _ (Oid 1009)) (Just fdata) = return (parseArray $ B.unpack fdata)
  fromField (Field _ _ (Oid 1009)) Nothing = return []
  fromField f _ = returnError Incompatible f "Field is not a text array."

-- Treats text[] as a String
instance ToField [String] where
  toField [] = Plain $ fromByteString "ARRAY[]"
  toField [s] = Many [Plain $ fromByteString "ARRAY[",
                      Escape $ B.pack s,
                      Plain $ fromByteString "]"
                      ]
  toField v  = Many [Plain $ fromByteString "ARRAY[",
                     Many $ map (\str -> do Many [Escape $ B.pack str, Plain $ fromByteString ","]) (Prelude.init v),
                     Escape $ B.pack $ last v,
                     Plain $ fromByteString "]"
                     ]
                     
parseArray :: String -> [String]
parseArray "{}" = []
parseArray ""   = []
parseArray (x:xs)
 | x == '{'  = parseArray $ init xs
 | x == '\"' = concat [[takeWhile (\c -> c /= '\"') xs], (parseArray $ tail $ dropWhile (\c -> c /= '\"') xs)]
 | x == ','  = parseArray xs
 | otherwise = concat [[takeWhile (\c -> c /= ',') (x:xs)], (parseArray $ dropWhile (\c -> c /= ',') (x:xs))]