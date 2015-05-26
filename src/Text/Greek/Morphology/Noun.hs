{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Morphology.Noun where

import Control.Lens

data Case = Nominative | Genitive | Dative | Accusative | Vocative
  deriving (Eq, Show)

allCases :: [Case]
allCases = [Nominative, Genitive, Dative, Accusative, Vocative]

data Gender = Feminine | Masculine | Neuter
  deriving (Eq, Show)

allGenders :: [Gender]
allGenders = [Feminine, Masculine, Neuter]

data Number = Singular | Plural -- | Dual
  deriving (Eq, Show)

allNumbers :: [Number]
allNumbers = [Singular, Plural] -- Dual

data NounInflection = NounInflection
  { _gender :: Gender
  , _number :: Number
  , _nounCase :: Case
  }
  deriving (Eq, Show)
makeLenses ''NounInflection

allNounInflections :: [NounInflection]
allNounInflections = [NounInflection g n c | g <- allGenders, n <- allNumbers, c <- allCases]
