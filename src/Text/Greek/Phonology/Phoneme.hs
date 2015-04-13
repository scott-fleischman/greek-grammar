module Text.Greek.Phonology.Phoneme where

import Text.Greek.Phonology.Consonants
import Text.Greek.Phonology.Vowels

data Phoneme =
    MkConsonant ConsonantPhoneme
  | MkVowel VowelPhoneme
  deriving (Eq, Ord, Show)
