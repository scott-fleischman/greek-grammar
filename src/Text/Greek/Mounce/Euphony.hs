{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Greek.Mounce.Euphony where

import Text.Greek.Grammar
import Text.Greek.Mounce.Phonology
import Text.Greek.Mounce.Quote

mounceEuphony :: [Cited Euphony]
mounceEuphony =
  [ mounce § "2.3" $
    Euphony "Two like vowels form their common long" [rules|
      α + α } α
      ι + ι } ι
      υ + υ } υ
      η + η } η
      ω + ω } ω
      ε + η } η
      η + ε } η
      ο + ω } ω
      ω + ο } ω
    |]
  , mounce § "2.4" $
    Euphony "Exceptions to §2.3" [rules|
      ε + ε } ει
      ο + ο } ου
    |]
  , mounce § "2.5" $
    Euphony "ο or ω overcome α, ε, or ὴ regardless of the order, and form ω" [rules|
      α + ο } ω
      ο + η } ω
    |]

  , mounce § "2.13a" $
    Euphony "Single vowel + diphthong (beginning with the same vowel as the single vowel)" [rules|
      α + αι } αι
      α + α } ᾳ
    |]

  , mounce § "14.1" $
    Euphony "Aspiration" [rules|
      π + ῾ } φ῾
      κ + ῾ } χ῾
      τ + ῾ } θ῾
    |]
  ]
