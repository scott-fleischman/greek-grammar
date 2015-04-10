module Text.Greek.Lexicon.Lemmas where

import Text.Greek.Phonology.Phoneme
import Text.Greek.Phonology.Shorthand
import Text.Greek.Morphology.Noun
import Text.Greek.Morphology.PartOfSpeech

data Lemma = Lemma
  { stem :: [Phoneme]
  , partOfSpeech :: PartOfSpeech
  }

theos :: Lemma
theos = Lemma [θ, ε, σ] (MkNoun (SecondDeclension Masculine))
