module Text.Greek.Morphology.Noun where

data Noun = Noun Case Gender Number

data Case = Nominative | Genitive | Dative | Accusative | Vocative

data Gender = Feminine | Masculine | Neuter

data Number = Singular | Plural | Dual

data Declension = FirstDeclension | SecondDeclension | ThirdDeclension
