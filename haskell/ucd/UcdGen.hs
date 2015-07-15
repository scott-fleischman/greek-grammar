module Main where

import System.FilePath

unicodeDataPath :: FilePath
unicodeDataPath = "data" </> "ucd" </> "UnicodeData" <.> "txt"

-- http://www.unicode.org/reports/tr44/tr44-16.html

data UnicodeData = UnicodeData
  { codePoint :: String
  , name :: String
  , generalCategory :: String
  , canonicalCombiningClass :: String
  , bidiClass :: String
  , decompositionTypeMapping :: String
  , numericTypeValue6 :: String
  , numericTypeValue7 :: String
  , numericTypeValue8 :: String
  , bidiMirrored :: String
  , unicode1Name :: String
  , isoComment :: String
  , simpleUppercaseMapping :: String
  , simpleLowercaseMapping :: String
  , simpleTitlecaseMapping :: String
  }

toUnicodeData :: [String] -> Maybe UnicodeData
toUnicodeData [v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14] = Just $ UnicodeData v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14
toUnicodeData _ = Nothing

main :: IO ()
main = do
  d <- readFile unicodeDataPath
  putStrLn . show . length $ d
