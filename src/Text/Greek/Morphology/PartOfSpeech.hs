module Text.Greek.Morphology.PartOfSpeech where

import Text.Greek.Morphology.Noun

data PartOfSpeech =
    MkNoun Noun
  | MkVerb
