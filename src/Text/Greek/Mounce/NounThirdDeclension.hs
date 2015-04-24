{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Greek.Mounce.NounThirdDeclension where

import Text.Greek.Grammar
import Text.Greek.Mounce.Morphology
import Text.Greek.Mounce.Quote

thirdDeclensionNouns :: [Cited Noun]
thirdDeclensionNouns = 
  [ mounce §§ ["n-3a", "n-3b"] $
    Noun "Stems ending in a labial (π β φ) or a velar (κ γ χ)"
      [nounCaseEndings|
        ς ες
        ος ων
        ι σι
        α ας
        ς ες
      |]
      []
      [greekWords|
        Αἰθίοψ κώνωψ λαῖλαψ μώλωψ σκόλοψ

        Ἄραψ λίψ

        ἀλώπηξ ἄνθραξ γυνή δεσμοφύλαξ θώραξ
        κῆρυξ κίλιξ κόραξ ὄρνιξ πίναξ
        πλάξ σάρξ σκώληξ Φῆλιξ Φοῖνιξ
        φοῖνιξ φύλαξ χάραξ χοῖνιξ

        αἴξ ἅρπαξ λάρυγξ μάστιξ πτέρυξ
        σάλπιγξ φάραγξ φλόξ

        θρίξ σαρδόνυξ ψίξ
      |]
  ]
