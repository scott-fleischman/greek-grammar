{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either
import Text.Greek.Source.All
import Text.Greek.Script.Raw
import qualified Data.Text as T
import qualified Data.Text.Format as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  result <- loadAll
  case result of
    Left es -> printErrors es
    Right xs -> works xs
      
works :: [Work] -> IO ()
works ws = case errors of
  _ : _ -> printErrors errors
  [] -> mapM_ (\x -> T.print "{}\n" (T.Only . T.pack . show $ x)) . take 10 $ results
  where
    (errors, results) = partitionEithers . fmap (\(Word s r) -> ensureText s r) . concatMap workWords $ ws

printErrors :: (Show e, Foldable t) => t e -> IO ()
printErrors = mapM_ (T.putStrLn . T.pack . show)
