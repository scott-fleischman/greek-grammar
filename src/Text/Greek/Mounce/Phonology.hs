{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Greek.Mounce.Phonology where

import Text.Greek.Grammar
import Text.Greek.Mounce.Quote

data Euphony = Euphony
  { euphonyName :: String
  , euphonyRules :: String
  }

euphony :: [Cited Euphony]
euphony =
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
  ]
