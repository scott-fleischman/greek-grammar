{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Greek.Mounce.NounThirdDeclension where

import Text.Greek.Grammar
import Text.Greek.Mounce.Morphology
import Text.Greek.Mounce.Quote

thirdDeclensionNouns :: [Cited Noun]
thirdDeclensionNouns = 
  [ mounce § "n-3a" $
    Noun "Stems ending in a labial (π β φ)"
      [nounCaseEndings|
        ς ες
        ος ων
        ι σι
        α ας
        ς ες
      |]
      []
      [greekWords| Αἰθίοψ κώνωψ λαῖλαψ μώλωψ σκόλοψ |]
  , mounce § "n-3b" $
    Noun "Stems ending in a velar (κ γ χ)"
      [nounCaseEndings|
        ς ες
        ος ων
        ι σι
        α ας
        ς ες
      |]
      []
      [greekWords| 
        ἀλώπηξ ἄνθραξ γυνή δεσμοφύλαξ θώραξ
        κῆρυξ κίλιξ κόραξ ὄρνιξ πίναξ
        πλάξ σάρξ σκώληξ Φῆλιξ Φοῖνιξ
        φοῖνιξ φύλαξ χάραξ χοῖνιξ
      |]
  ]
