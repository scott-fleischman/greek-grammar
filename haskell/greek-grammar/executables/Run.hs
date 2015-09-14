{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Text.Greek.Source.All
import Text.Greek.Script.Raw
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  result <- loadAll
  case result of
    Left es -> mapM_ (T.putStrLn . T.pack . show) es
    Right xs -> mapM_ (T.putStrLn . T.pack . show) $ 
      filter (isn't _Just . view _2) . fmap (\s -> (s, ensureText (wordSurface s) (wordFileReference s))) . concatMap workWords $
      xs
