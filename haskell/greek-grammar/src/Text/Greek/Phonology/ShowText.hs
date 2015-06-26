{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Phonology.ShowText where

import Prelude hiding (lookup)
import Data.Map.Strict (Map, fromList, lookup)
import Data.Text (Text, intercalate)
import Data.Text.Format (Shown(..), Only(..))
import Data.Text.Format.Strict (format')
import Data.Tuple (swap)
import Text.Greek.Phonology.Phoneme
import Text.Greek.Phonology.Consonants
import Text.Greek.Phonology.Vowels

showWord :: [Phoneme] -> Text
showWord = intercalate " " . fmap showText

showText :: Phoneme -> Text
showText p = case lookup p phonemeToText of
  Just t -> t
  Nothing -> format' "Unknown phoneme {}" (Only $ Shown p)

parsePhoneme :: Text -> Maybe Phoneme
parsePhoneme t = lookup t textToPhoneme

textToPhoneme :: Map Text Phoneme
textToPhoneme = fromList . fmap swap $ phonemeTextPairs

phonemeToText :: Map Phoneme Text
phonemeToText = fromList phonemeTextPairs

phonemeTextPairs :: [(Phoneme, Text)]
phonemeTextPairs =
  [ ((MkVowel (VowelPhoneme Alpha Short)), "ᾰ")
  , ((MkVowel (VowelPhoneme Alpha Long)), "ᾱ")
  , ((MkVowel (VowelPhoneme Omicron Short)), "ο")
  , ((MkConsonant Theta), "θ")
  , ((MkVowel (Diphthong Omicron Upsilon)), "ου")
  , ((MkVowel (VowelPhoneme Epsilon Short)), "ε")
  , ((MkVowel (VowelPhoneme Eta Long)), "η")
  , ((MkVowel (VowelPhoneme Iota Long)), "ῑ")
  , ((MkVowel (VowelPhoneme Iota Short)), "ῐ")
  , ((MkVowel (VowelPhoneme Upsilon Long)), "ῡ")
  , ((MkVowel (VowelPhoneme Upsilon Short)), "ῠ")
  , ((MkVowel (SpuriousDiphthong Omicron Upsilon)), "ου'")
  , ((MkVowel (Diphthong Alpha Upsilon)), "αυ")
  , ((MkVowel (Diphthong Epsilon Upsilon)), "ευ")
  , ((MkVowel (Diphthong Eta Upsilon)), "ηυ")
  , ((MkVowel (Diphthong Alpha Iota)), "αι")
  , ((MkVowel (Diphthong Epsilon Iota)), "ει" )
  , ((MkVowel (SpuriousDiphthong Epsilon Iota)), "ει'")
  , ((MkVowel (Diphthong Omicron Iota)), "οι")
  , ((MkVowel (ImproperDiphthong Alpha)), "ᾳ")
  , ((MkVowel (ImproperDiphthong Eta)), "ῃ")
  , ((MkVowel (ImproperDiphthong Omega)), "ῳ")
  , ((MkConsonant Gamma), "γ")
  , ((MkConsonant Delta), "δ")
  , ((MkConsonant Zeta), "ζ")
  , ((MkConsonant Kappa), "κ")
  , ((MkConsonant Lambda), "λ")
  , ((MkConsonant Mu), "μ")
  , ((MkConsonant Nu), "ν")
  , ((MkConsonant Xi), "ξ")
  , ((MkConsonant Pi), "π")
  , ((MkConsonant Rho), "ρ")
  , ((MkConsonant Sigma), "σ")
  , ((MkConsonant Tau), "τ")
  , ((MkConsonant Phi), "φ")
  , ((MkConsonant Chi), "χ")
  , ((MkConsonant Psi), "ψ")
  , ((MkConsonant ConsonantalIota), "ι'")
  , ((MkConsonant RoughBreathing), "h")
  , ((MkConsonant Beta), "β")
  , ((MkVowel (VowelPhoneme Omega Long)), "ω")
  , ((MkConsonant GammaNasal), "γ'")
  , ((MkConsonant Digamma), "ϝ")
  , ((MkConsonant RhoRough), "ῥ")
  ]
