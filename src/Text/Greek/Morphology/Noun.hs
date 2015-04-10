module Text.Greek.Morphology.Noun where

data NounInflection = NounInflection Case Gender Number

data Case = Nominative | Genitive | Dative | Accusative | Vocative

data Gender = Feminine | Masculine | Neuter

data Number = Singular | Plural | Dual

data Noun =
    FirstDeclension
  | SecondDeclension Gender
  | ThirdDeclension
