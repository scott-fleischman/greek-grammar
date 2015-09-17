{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Either
import Data.List
import Data.Set (Set)
import Text.Greek.Render
import Text.Greek.Source.All
import Text.Greek.Utility
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Text.Greek.Script.Unit as U

main :: IO ()
main = do
  result <- loadAll
  case result of
    Left es -> printErrors es
    Right xs -> works xs
      
works :: [Work] -> IO ()
works ws = case errors of
  _ : _ -> printErrors errors
  [] -> renderSummary . unitMarkLetterPairs . concat $ results
  where
    (errors, results) = partitionEithers . fmap (\(Word s _) -> U.toUnits s) . concatMap workWords $ ws

renderSummary :: (Render b, Ord b, Foldable t) => [(b, t a)] -> IO ()
renderSummary = renderAll . fmap (over _2 length) . sortOn fst

unitMarkLetterPairs :: (Ord l, Ord m) => [U.Unit l m] -> [((m, l), [U.Unit l m])]
unitMarkLetterPairs = concatQuery (U.getMarkLetterPairs)

unitLetterMarkSets :: (Ord l, Ord m) => [U.Unit l m] -> [((l, Set m), [U.Unit l m])]
unitLetterMarkSets = query U.getLetterMarkSet

unitLetters :: Ord l => [U.Unit l m] -> [(l, [U.Unit l m])]
unitLetters = query U.unitLetter

unitMarks :: Ord m => [U.Unit l m] -> [(m, [U.Unit l m])]
unitMarks = concatQuery U.getMarks

renderAll :: Render t => [t] -> IO ()
renderAll = mapM_ (T.putStrLn . L.toStrict . render)

printErrors :: (Show e, Foldable t) => t e -> IO ()
printErrors = mapM_ (T.putStrLn . T.pack . show)
