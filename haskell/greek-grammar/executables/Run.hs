{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (Word)
import Control.Lens
import Data.List
import Data.Set (Set)
import Text.Greek.IO
import Text.Greek.Render
import Text.Greek.Source.All
import Text.Greek.Utility
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Text.Greek.Script.Mark as Mark
import qualified Text.Greek.Script.Unit as U
import qualified Text.Greek.Script.Word as Word

main :: IO ()
main = handleAll
  >>= renderAll . fmap (over workContent (take 100)) . take 1

summarize :: (Foldable twork, Foldable tword, Foldable tc) => twork (Work (tword (Word.Cased (tc (U.Unit l Mark.AllPair))))) -> IO ()
summarize = renderSummary . query (Mark.forgetAllReference . view U.unitMarks) . globalConcatSurface Word.casedSurface

unitCharLetters :: [U.UnitChar] -> [(U.LetterChar, [U.UnitChar])]
unitCharLetters = query U.getLetter

unitCharMarks :: [U.UnitChar] -> [(U.MarkChar, [U.UnitChar])]
unitCharMarks = concatQuery U.getMarks

unitMarkLetterPairs :: (Ord l, Ord m) => [U.UnitMarkList l m] -> [((m, l), [U.UnitMarkList l m])]
unitMarkLetterPairs = concatQuery (U.getMarkLetterPairs)

unitLetterMarkSets :: (Ord l, Ord m) => [U.UnitMarkList l m] -> [((l, Set m), [U.UnitMarkList l m])]
unitLetterMarkSets = query U.getLetterMarkSet

renderSummary :: (Render b, Ord b, Foldable t) => [(b, t a)] -> IO ()
renderSummary = renderAll . fmap (over _2 length) . sortOn fst

renderAll :: Render t => [t] -> IO ()
renderAll = mapM_ (T.putStrLn . L.toStrict . render)
