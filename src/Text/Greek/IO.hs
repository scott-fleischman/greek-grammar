{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.IO where

import Prelude hiding (putStrLn)
import Data.Text.IO
import Data.Text.Format
import Data.Text.Format.Strict
import Text.Greek.Lexicon.Lemmas
import Text.Greek.Morphology.Noun
import Text.Greek.Phonology.Phoneme
import Text.Greek.Phonology.ShowText

dumpLemma :: Lemma -> IO ()
dumpLemma lem = dumpParadigm (paradigm lem)

dumpParadigm :: [(NounInflection, [Phoneme])] -> IO ()
dumpParadigm = mapM_ (\(NounInflection g n c,ps) -> putStrLn $ format' "{} {} {} {}" (Shown g, Shown n, Shown c, showWord ps))

dumpWord :: [Phoneme] -> IO ()
dumpWord = putStrLn . showWord
