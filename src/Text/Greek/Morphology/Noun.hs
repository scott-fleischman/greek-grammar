{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Morphology.Noun where

import Control.Lens
import Text.Greek.Phonology.Phoneme
import Text.Greek.Phonology.Shorthand

data Case = Nominative | Genitive | Dative | Accusative | Vocative
  deriving (Eq, Show)

data Gender = Feminine | Masculine | Neuter
  deriving (Eq, Show)

data Number = Singular | Plural -- | Dual
  deriving (Eq, Show)

data NounInflection = NounInflection
  { _gender :: Gender
  , _number :: Number
  , _nounCase :: Case
  }
  deriving (Eq, Show)
makeLenses ''NounInflection

data Noun =
    FirstDeclension
  | SecondDeclension Gender
  | ThirdDeclension


data OptionNomSg = OptionNomSg_s | OptionNomSg_null
data OptionGenSg = OptionGenSg_s | OptionGenSg_io

data VowelDeclension = VowelDeclension
  { _thematicVowel :: NounInflection -> Phoneme
  , _optionNomSg :: OptionNomSg
  , _optionGenSg :: OptionGenSg
  }
makeLenses ''VowelDeclension

nom_Sg_MascFem :: OptionNomSg -> [Phoneme]
nom_Sg_MascFem OptionNomSg_s = [σ]
nom_Sg_MascFem OptionNomSg_null = []

gen_Sg :: OptionGenSg -> [Phoneme]
gen_Sg OptionGenSg_s = [σ]
gen_Sg OptionGenSg_io = [ι', ο]

-- smyth 210
caseEnding :: VowelDeclension -> NounInflection -> [Phoneme]
caseEnding d (NounInflection Masculine  Singular Nominative)  = nom_Sg_MascFem (d ^. optionNomSg)
caseEnding d (NounInflection Feminine   Singular Nominative)  = nom_Sg_MascFem (d ^. optionNomSg)
caseEnding _ (NounInflection Neuter     Singular Nominative)  = [ν]

caseEnding d (NounInflection _          Singular Genitive)    = gen_Sg (d ^. optionGenSg)

caseEnding _ (NounInflection _          Singular Dative)      = [ῐ]

caseEnding _ (NounInflection _          Singular Accusative)  = [ν]

caseEnding _ (NounInflection Masculine  Singular Vocative)    = []
caseEnding _ (NounInflection Feminine   Singular Vocative)    = []
caseEnding _ (NounInflection Neuter     Singular Vocative)    = [ν]

caseEnding _ (NounInflection Masculine  Plural   Nominative)  = [ῐ]
caseEnding _ (NounInflection Feminine   Plural   Nominative)  = [ῐ]
caseEnding _ (NounInflection Neuter     Plural   Nominative)  = [ᾰ]

caseEnding _ (NounInflection _          Plural   Genitive)    = [ω, ν]

caseEnding _ (NounInflection _          Plural   Dative)      = [ῐ, σ]

caseEnding _ (NounInflection Masculine  Plural   Accusative)  = [ν, σ]
caseEnding _ (NounInflection Feminine   Plural   Accusative)  = [ν, σ]
caseEnding _ (NounInflection _          Plural   Accusative)  = [ᾰ]

caseEnding _ (NounInflection Masculine  Plural   Vocative)    = [ῐ]
caseEnding _ (NounInflection Feminine   Plural   Vocative)    = [ῐ]
caseEnding _ (NounInflection Neuter     Plural   Vocative)    = [ᾰ]

secondDeclension :: VowelDeclension
secondDeclension = VowelDeclension v OptionNomSg_s OptionGenSg_io
  where
    v (NounInflection _ Singular Vocative) = ε
    v _ = ο

