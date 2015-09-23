{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.List
import Data.Set (Set)
import Text.Greek.IO
import Text.Greek.Render
import Text.Greek.Source.All
import Text.Greek.Utility
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Text.Greek.Script.Unit as U

main :: IO ()
main = handleAll
  >>= renderSummary . unitMarkLetterPairs . globalConcatSurface

unitCharLetters :: [U.UnitChar] -> [(U.LetterChar, [U.UnitChar])]
unitCharLetters = query U.getLetter

unitCharMarks :: [U.UnitChar] -> [(U.MarkChar, [U.UnitChar])]
unitCharMarks = concatQuery U.getMarks

unitMarkLetterPairs :: (Ord l, Ord m) => [U.Unit l m] -> [((m, l), [U.Unit l m])]
unitMarkLetterPairs = concatQuery (U.getMarkLetterPairs)

unitLetterMarkSets :: (Ord l, Ord m) => [U.Unit l m] -> [((l, Set m), [U.Unit l m])]
unitLetterMarkSets = query U.getLetterMarkSet

renderSummary :: (Render b, Ord b, Foldable t) => [(b, t a)] -> IO ()
renderSummary = renderAll . fmap (over _2 length) . sortOn fst

renderAll :: Render t => [t] -> IO ()
renderAll = mapM_ (T.putStrLn . L.toStrict . render)
