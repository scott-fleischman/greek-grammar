{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either
import Text.Greek.FileReference
import Text.Greek.Source.All
import Text.Greek.Script.Raw
import qualified Data.Text as T
import qualified Data.Text.Format as T
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
  [] -> mapM_ (\x -> T.print "{}\n" (T.Only . formatCharPair $ x)) . concat . take 200 . drop 1000 $ results
  where
    (errors, results) = partitionEithers . fmap (\(Word s r) -> decompose s r) . concatMap workWords $ ws

printErrors :: (Show e, Foldable t) => t e -> IO ()
printErrors = mapM_ (T.putStrLn . T.pack . show)

formatCharPair :: (Char, FileCharReference) -> L.Text
formatCharPair (c, r) = T.format "({},{})" (L.singleton c, formatFileCharReference r)

formatFileCharReference :: FileCharReference -> L.Text
formatFileCharReference (FileCharReference p l) = T.format "{}:{}" (T.Shown p, formatLineReference l)

formatLineReference :: LineReference -> L.Text
formatLineReference (LineReference (Line l) (Column c)) = T.format "{}:{}" (l, c)
