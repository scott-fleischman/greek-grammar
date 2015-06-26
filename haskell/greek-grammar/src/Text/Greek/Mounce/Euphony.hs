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
  -- , mounce § "2.8" $
  --   Euphony "When three vowels come into contact, and if the first two or the last two do not form a diphthong, then the second and third vowels contract first, and the result contracts with the first vowel. In other words, contraction is from the right to the left ('progressive assimilation'; §2.2; Smyth §55). This is especially frequent in contract verbs." [rules|
  --     αει } αι } ᾳ
  --     εαι } εᾳ } ῃ
  --   |]
  , mounce § "2.13a" $
    Euphony "Single vowel + diphthong (beginning with the same vowel as the single vowel)" [rules|
      α + αι } αι
      α + ᾳ  } ᾳ
    |]
  -- , mounce § "2.13b" $
  --   Euphony "Single vowel + diphthong (beginning with a vowel different from the single vowel)." [rules|
  --     ο + ου } ουυ } ου
  --     α + αι } αι  } ᾳ
  --   |]
  , mounce § "2.13b" $
    Euphony "Single vowel + diphthong (beginning with a vowel different from the single vowel) - Exceptions." [rules|
      ε + οι } οι
      ο + ει } οι
      ο + ῃ  } οι
    |]
  -- , mounce § "2.13c" $
  --   Euphony "When ει or ου are spurious (§2.14), the diphthong is regarded as simple ε or ο (i.e., the final vowel of the diphthong is dropped" [rules|
  --     α + ει } αι  } α
  --     ε + ει } ειι } ει
  --     η + ει } ηι  } η
  --     ο + ει } ουι } ου
  --     α + ου } ωυ  } ω
  --     ε + ου } ουυ } ου 
  --     ο + ου } ουυ } ου
  --   |]
  , mounce § "14.1" $
    Euphony "Aspiration" [rules|
      π + ῾ } φ῾
      κ + ῾ } χ῾
      τ + ῾ } θ῾
    |]
  ]
