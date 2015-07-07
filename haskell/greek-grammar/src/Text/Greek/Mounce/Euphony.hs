{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Greek.Mounce.Euphony where

import Text.Greek.Grammar
import Text.Greek.Mounce.Phonology
import Text.Greek.Mounce.Quote

mounceEuphony :: [Cited Euphony]
mounceEuphony =
  [mounce § "2.2c" $
    Euphony "ι followed by long α, η, ω subscripts" [rules|
      α + ι } ᾳ
      η + ι } ῃ
      ω + ι } ῳ
   |]
  ,mounce § "2.3" $
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
  , mounce § "2.4a" $
    Euphony "ει is formed by εε" [rules|
      ε + ε } ει 
    |]
  , mounce § "2.4b" $
    Euphony "ου is formed by εο" [rules|
      ε + ο } ου
      ο + ε } ου  
      ο + ο } ου
    |]
  , mounce § "2.5" $
    Euphony "ο or ω overcome α, ε, or ὴ regardless of the order, and form ω" [rules|
      α + ο } ω
      ο + η } ω
    |]
  , mounce § "2.6" $
    Euphony "exceptions to 2.5" [rules|
      ε + ο } ου  
      ο + ε } ου  
    |]
  , mounce § "2.7" $
    Euphony "If α comes before ε or η, they will contract to α. If ε or η comes before α, they will contract to η ('progressive assimilation,' §2.2)" [rules|
      α + ε } α
      α + η } α
      ε + α } α
      η + α } α
    |]
  , mounce § "2.7a" $
    Euphony "α is formed from αε" [rules|
      α + ε } α
    |]
  , mounce § "2.7b" $
    Euphony "η is formed from εα" [rules|
      ε + α } η
    |]
  , mounce § "2.13a" $
    Euphony "Single vowel + diphthong (beginning with the same vowel as the single vowel)" [rules|
      α + αι } αι
      α + αι } ᾳ
      ο + ου } ου
    |]
  , mounce § "2.13b" $
    Euphony "Single vowel + diphthong (beginning with a vowel different from the single vowel)" [rules|
      ε + οι } οι
      ο + ει } οι
      ο + ῃ  } οι
    |]
  , mounce § "14.1" $
    Euphony "Aspiration" [rules|
      π + ῾ } φ῾
      κ + ῾ } χ῾
      τ + ῾ } θ῾
    |]
  , mounce § "14.4" $
    Euphony "labial + θ" [rules|
      π + θ } φθ
      β + θ } φθ
      ψ + θ } φθ
    |]
  , mounce § "14.5" $
    Euphony "velar + θ" [rules|
      κ + θ } χθ
      γ + θ } χθ
      ξ + θ } χθ
    |]
  
  


  ]
