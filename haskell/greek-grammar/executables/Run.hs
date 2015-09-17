{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main where

import Control.Lens
import Data.Either
import Data.List
import Text.Greek.Render
import Text.Greek.Source.All
import Text.Greek.Utility
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Text.Greek.Script.Unit as U

main :: IO ()
main = loadAll >>= handleEither >>= textWorks

textWorks :: [TextWork] -> IO ()
textWorks ws = case errors of
  _ : _ -> printErrors errors
  [] -> renderSummary . unitCharLetters . concat $ results
  where
    (errors, results) = partitionEithers . fmap (\(BasicWord s _) -> U.toUnits s) . concatMap workContent $ ws

renderSummary :: (Render b, Ord b, Foldable t) => [(b, t a)] -> IO ()
renderSummary = renderAll . fmap (over _2 length) . sortOn fst

--unitMarkLetterPairs :: (Ord l, Ord m) => [U.Unit l m] -> [((m, l), [U.Unit l m])]
--unitMarkLetterPairs = concatQuery (U.getMarkLetterPairs)

--unitLetterMarkSets :: (Ord l, Ord m) => [U.Unit l m] -> [((l, Set m), [U.Unit l m])]
--unitLetterMarkSets = query U.getLetterMarkSet

unitCharLetters :: [U.UnitChar] -> [(U.LetterChar, [U.UnitChar])]
unitCharLetters = query U.unitLetter

unitCharMarks :: [U.UnitChar] -> [(U.MarkChar, [U.UnitChar])]
unitCharMarks = concatQuery U.getMarks

renderAll :: Render t => [t] -> IO ()
renderAll = mapM_ (T.putStrLn . L.toStrict . render)

printErrors :: (Show e, Foldable t) => t e -> IO a
printErrors es = do
  mapM_ (T.putStrLn . T.pack . show) es
  fail "failure"

handleEither :: (Show e, Foldable t) => Either (t e) x -> IO x
handleEither (Left es) = printErrors es
handleEither (Right x) = return x

handleListEither :: (Show e) => [Either e x] -> IO [x]
handleListEither eithers = case errors of
  _ : _ -> printErrors errors
  [] -> return results
  where
    (errors, results) = partitionEithers eithers
