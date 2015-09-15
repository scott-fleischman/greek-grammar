{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either
import Text.Greek.Render
import Text.Greek.Source.All
import Text.Greek.Script.Unit
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L

main :: IO ()
main = do
  result <- loadAll
  case result of
    Left es -> printErrors es
    Right xs -> works xs
      
works :: [Work] -> IO ()
works ws = case errors of
  _ : _ -> printErrors errors
  [] -> mapM_ (\x -> T.putStrLn . L.toStrict . render $ x) . concat . take 200 . drop 1000 $ results
  where
    (errors, results) = partitionEithers . fmap (\(Word s r) -> toUnits s r) . concatMap workWords $ ws

printErrors :: (Show e, Foldable t) => t e -> IO ()
printErrors = mapM_ (T.putStrLn . T.pack . show)
