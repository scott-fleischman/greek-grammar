{-# LANGUAGE RankNTypes #-}

module Text.Greek.Script.Syllable where

import Control.Lens
import Text.Greek.FileReference
import Text.Greek.Script.Unit (Unit)
import Text.Parsec.Error (ParseError)
import qualified Text.Greek.Script.Letter as Letter
import qualified Text.Greek.Script.Mark as Mark
--import qualified Text.Greek.Script.Unit as Unit

data Vocalic a b c
  = OneVowel a
  | IotaSubscriptVowel b
  | TwoVowel c

type VocalicSimple a = Vocalic a a (a, a)
type VocalicVowel = VocalicSimple Letter.Vowel
type VocalicPair = VocalicSimple (Letter.Vowel, FileCharReference)

type VocalicConsonant = Either VocalicPair (Letter.Consonant, FileCharReference)

parseVocalicSyllable
  :: (Show l, Show m)
  => Lens l l' (Letter.VowelConsonant, FileCharReference) VocalicConsonant
  -> Lens m m' (Maybe Mark.SyllabicAllPair) ()
  -> [Unit l m]
  -> Either ParseError [Unit l' m']
parseVocalicSyllable = undefined
