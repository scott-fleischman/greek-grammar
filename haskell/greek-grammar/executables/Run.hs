{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Text.Greek.Source.All
import Text.Greek.Script.Raw
import qualified Data.Text as T
import qualified Data.Text.Format as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  result <- loadAll
  case result of
    Left es -> mapM_ (T.putStrLn . T.pack . show) es
    Right xs -> mapM_ (\x -> T.print "{}\n" (T.Only . T.pack . show $ x)) $ 
      take 10 . catMaybes . fmap (\(Word s r) -> ensureText s r) . concatMap workWords $
      xs

