{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (Word)
--import Control.Lens
--import Data.List
--import Data.Set (Set)
--import Text.Greek.IO
--import Text.Greek.Render
--import Text.Greek.Source.All
--import Text.Greek.Utility
--import qualified Data.Text.IO as T
--import qualified Data.Text.Lazy as L
--import qualified Text.Greek.Script.Word as Word
import qualified Text.Greek.IO.Process as Process

main :: IO ()
main = Process.runProcess

--commandLine :: IO ()
--commandLine = handleAll
--  >>= renderSummary . query fst . concat . fmap (\(xs, y) -> fmap (\x -> (x, y)) xs) . fmap (clusters . view Word.casedSurface) . concat . fmap (view workContent)
--    where
--      clusters ws =
--          ( drop 1 . groupEithers $ fmap getPair ws
--          , snd . Unit._unitItem $ ws !! 0
--          )
--        where
--          getPair w = over _Right (\x -> (x, reference)) . over _Left (\x -> (x, reference)) $ w ^. Unit.unitItem . _1
--            where
--              reference = over _Just fst $ w ^. Unit.unitMarks . _3

--renderSome :: Render a => [Work [Word.Cased [a]]] -> IO ()
--renderSome = renderAll . fmap (over workContent (take 100)) . take 1 . drop 2

--unitCharLetters :: [Unit.UnitChar] -> [(Unit.LetterChar, [Unit.UnitChar])]
--unitCharLetters = query Unit.getLetter

--unitCharMarks :: [Unit.UnitChar] -> [(Unit.MarkChar, [Unit.UnitChar])]
--unitCharMarks = concatQuery Unit.getMarks

--unitMarkLetterPairs :: (Ord l, Ord m) => [Unit.UnitMarkList l m] -> [((m, l), [Unit.UnitMarkList l m])]
--unitMarkLetterPairs = concatQuery (Unit.getMarkLetterPairs)

--unitLetterMarkSets :: (Ord l, Ord m) => [Unit.UnitMarkList l m] -> [((l, Set m), [Unit.UnitMarkList l m])]
--unitLetterMarkSets = query Unit.getLetterMarkSet

--renderSummary :: (Render b, Ord b, Foldable t) => [(b, t a)] -> IO ()
--renderSummary = renderAll . fmap (over _2 length) . sortOn fst

--renderAll :: Render t => [t] -> IO ()
--renderAll = mapM_ (T.putStrLn . L.toStrict . render)
