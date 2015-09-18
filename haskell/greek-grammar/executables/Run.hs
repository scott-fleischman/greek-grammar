{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main where

import Control.Category ((>>>))
import Control.Lens
import Data.Either
import Data.List
import Text.Greek.FileReference
import Text.Greek.Render
import Text.Greek.Source.All
import Text.Greek.Utility
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Text.Greek.Script.Unicode as U
import qualified Text.Greek.Script.Unit as U

main :: IO ()
main = loadAll >>= handleEither
  >>= (workToUnitChar >>> handleListEither)
  >>= (workToUnitUnicode >>> handleListEither)
  >>= (length >>> show >>> putStrLn)

--  >>= (globalConcatSurface >>> unitCharMarks >>> renderSummary)

workToUnitChar ::        [Work [BasicWord (T.Text, FileReference)]]
  -> [Either U.UnitError (Work [BasicWord [U.UnitChar]])]
workToUnitChar = fmap ((workContent . traverse . basicWordSurface) U.toUnitChar)

workToUnitUnicode ::        [Work [BasicWord [U.UnitChar]]]
  -> [Either U.UnicodeError (Work [BasicWord [U.UnitUnicode]])]
workToUnitUnicode = fmap ((workContent . traverse . basicWordSurface . traverse) U.toUnitUnicode)


renderSummary :: (Render b, Ord b, Foldable t) => [(b, t a)] -> IO ()
renderSummary = renderAll . fmap (over _2 length) . sortOn fst

unitCharLetters :: [U.UnitChar] -> [(U.LetterChar, [U.UnitChar])]
unitCharLetters = query U.getLetter

unitCharMarks :: [U.UnitChar] -> [(U.MarkChar, [U.UnitChar])]
unitCharMarks = concatQuery U.getMarks

--unitMarkLetterPairs :: (Ord l, Ord m) => [U.Unit l m] -> [((m, l), [U.Unit l m])]
--unitMarkLetterPairs = concatQuery (U.getMarkLetterPairs)

--unitLetterMarkSets :: (Ord l, Ord m) => [U.Unit l m] -> [((l, Set m), [U.Unit l m])]
--unitLetterMarkSets = query U.getLetterMarkSet

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
