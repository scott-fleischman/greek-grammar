module Text.Greek.Mounce.Noun where

import Text.Greek.Mounce.Morphology

nouns :: [Noun]
nouns =
  [ Noun "n-1a" "Feminine nouns with stems ending in εα, ια, or ρα and a genitive in ας"
    [nounCaseEndings|
      - ι   
      ς ων
      ι ις
      ν νς
    |]

  ]
