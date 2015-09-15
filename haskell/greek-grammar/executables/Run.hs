{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either
import Data.Foldable
import Text.Greek.FileReference
import Text.Greek.Source.All
import Text.Greek.Script.Unit
import qualified Data.Map as M
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
  [] -> mapM_ (\x -> T.putStrLn . L.toStrict . formatUnit $ x) . concat . take 200 . drop 1000 $ results
  where
    (errors, results) = partitionEithers . fmap (\(Word s r) -> toUnits s r) . concatMap workWords $ ws

printErrors :: (Show e, Foldable t) => t e -> IO ()
printErrors = mapM_ (T.putStrLn . T.pack . show)

formatCharPair :: (Char, FileCharReference) -> L.Text
formatCharPair (c, r) = T.format "({},{})" (L.singleton c, formatFileCharReference r)

formatMarkPair :: (Char, FileCharReference) -> L.Text
formatMarkPair (c, r) = T.format "({},{})" (formatMark c, formatFileCharReference r)

formatUnit :: Unit -> L.Text
formatUnit (Unit c r ms) = T.format "({},{},{})" (L.singleton c, formatFileCharReference r, formatAll formatMarkPair (M.toList ms))

formatMark :: Char -> L.Text
formatMark = T.format "\x25CC{}" . T.Only . L.singleton

formatAll :: Foldable t => (a -> L.Text) -> t a -> L.Text
formatAll f = L.intercalate "," . fmap f . toList

formatFileCharReference :: FileCharReference -> L.Text
formatFileCharReference (FileCharReference p l) = T.format "{}:{}" (T.Shown p, formatLineReference l)

formatLineReference :: LineReference -> L.Text
formatLineReference (LineReference (Line l) (Column c)) = T.format "{}:{}" (l, c)
