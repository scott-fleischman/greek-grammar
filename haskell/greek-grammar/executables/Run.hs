{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Greek.Paths
import Text.Greek.Source.Sblgnt
import Text.Greek.Xml.Parse
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  result <- readParseEvents sblgntParser sblgntXmlPath
  case result of
    Left es -> mapM_ (T.putStrLn . T.pack . show) es
    Right xs -> T.putStrLn . T.pack . show . length . sblgntBooks $ xs
