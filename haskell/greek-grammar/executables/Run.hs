{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Greek.Source.All
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  result <- loadAll
  case result of
    Left es -> mapM_ (T.putStrLn . T.pack . show) es
    Right xs -> T.putStrLn . T.pack . show . sum . fmap (length . workWords) $ xs
