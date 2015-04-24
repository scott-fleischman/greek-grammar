{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Greek.Mounce.Noun where

import Text.Greek.Grammar
import Text.Greek.Mounce.Morphology
import Text.Greek.Mounce.Quote

nouns :: [Cited Noun]
nouns = 
  [ mounce § "n-1a" $
    Noun "Feminine nouns with stems ending in εα, ια, or ρα and a genitive in ας" [nounCaseEndings|
        - ι   
        ς ων
        ι ις
        ν νς
      |]
  ]
