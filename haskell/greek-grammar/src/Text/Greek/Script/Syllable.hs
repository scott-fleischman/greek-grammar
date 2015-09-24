{-# LANGUAGE RankNTypes #-}

module Text.Greek.Script.Syllable where

import Control.Lens
import Text.Greek.FileReference
import Text.Parsec.Error (ParseError)
import qualified Text.Greek.Script.Letter as Letter
import qualified Text.Greek.Script.Mark as Mark

data Vocalic a b c
  = OneVowel a
  | IotaSubscriptVowel b
  | TwoVowel c

type VocalicSimple a = Vocalic a a (a, a)
type VocalicVowel = VocalicSimple Letter.Vowel
type VocalicPair = VocalicSimple (Letter.Vowel, FileCharReference)

type VocalicConsonant = Either VocalicPair (Letter.Consonant, FileCharReference)

parseVocalicSyllable
  :: Show s => Lens s t (Letter.VowelConsonant, Maybe Mark.SyllabicAllPair) (VocalicConsonant, ())
  -> [s]
  -> Either ParseError [t]
parseVocalicSyllable = undefined
