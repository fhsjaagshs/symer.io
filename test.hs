{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

main :: IO ()
main = putStrLn $ show $ parseArray "{\"this,\",that}"

-- {"th,is",that}   []
-- "th,is",that   []
-- ,that  ["th,is"]
-- that ["th,is"]
-- ["th,is","that"]

parseArray :: String -> [String]
parseArray "{}" = []
parseArray ""   = []
parseArray (x:xs)
 | x == '{'  = parseArray $ init xs
 | x == '\"' = concat [[takeWhile (\c -> c /= '\"') xs], (parseArray $ tail $ dropWhile (\c -> c /= '\"') xs)]
 | x == ','  = parseArray xs
 | otherwise = concat [[takeWhile (\c -> c /= ',') (x:xs)], (parseArray $ dropWhile (\c -> c /= ',') (x:xs))]
