{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Phonology.ShowText where

import Data.Text (Text)
import Text.Greek.Phonology.Phoneme
import Text.Greek.Phonology.Consonants
import Text.Greek.Phonology.Vowels

showText :: Phoneme -> Text
showText (MkVowel (VowelPhoneme Alpha Short)) = "ᾰ"
showText (MkVowel (VowelPhoneme Alpha Long)) = "ᾱ"
showText (MkVowel (VowelPhoneme Omicron _)) = "ο"
showText (MkConsonant Theta) = "θ"
showText (MkVowel (Diphthong Omicron Upsilon)) = "ου"
showText (MkVowel (VowelPhoneme Epsilon _)) = "ε"
showText (MkVowel (VowelPhoneme Eta _)) = "η"
showText (MkVowel (VowelPhoneme Iota Long)) = "ῑ"
showText (MkVowel (VowelPhoneme Iota Short)) = "ῐ"
showText (MkVowel (VowelPhoneme Upsilon Long)) = "ῡ"
showText (MkVowel (VowelPhoneme Upsilon Short)) = "ῠ"
showText (MkVowel (SpuriousDiphthong Omicron Upsilon)) = "ου'"
showText (MkVowel (Diphthong Alpha Upsilon)) = "αυ"
showText (MkVowel (Diphthong Epsilon Upsilon)) = "ευ"
showText (MkVowel (Diphthong Eta Upsilon)) = "ηυ"
showText (MkVowel (Diphthong Alpha Iota)) = "αι"
showText (MkVowel (Diphthong Epsilon Iota)) = "ει" 
showText (MkVowel (SpuriousDiphthong Epsilon Iota)) = "ει'"
showText (MkVowel (Diphthong Omicron Iota)) = "οι"
showText (MkVowel (ImproperDiphthong Alpha)) = "ᾳ"
showText (MkVowel (ImproperDiphthong Eta)) = "ῃ"
showText (MkVowel (ImproperDiphthong Omega)) = "ῳ"
showText (MkConsonant Gamma) = "γ"
showText (MkConsonant Delta) = "δ"
showText (MkConsonant Zeta) = "ζ"
showText (MkConsonant Kappa) = "κ"
showText (MkConsonant Lambda) = "λ"
showText (MkConsonant Mu) = "μ"
showText (MkConsonant Nu) = "ν"
showText (MkConsonant Xi) = "ξ"
showText (MkConsonant Pi) = "π"
showText (MkConsonant Rho) = "ρ"
showText (MkConsonant Sigma) = "σ"
showText (MkConsonant Tau) = "τ"
showText (MkConsonant Phi) = "φ"
showText (MkConsonant Chi) = "χ"
showText (MkConsonant Psi) = "ψ"
showText (MkConsonant ConsonantalIota) = "ι'"
showText (MkConsonant RoughBreathing) = "h"
showText (MkConsonant Beta) = "β"
showText (MkVowel (VowelPhoneme Omega _)) = "ω"
showText (MkConsonant GammaNasal) = "γγ"
showText (MkConsonant Digamma) = "ϝ"
showText (MkConsonant RhoRough) = "hρ"
showText (MkVowel (Diphthong _ _)) = "unknown diphthong"
showText (MkVowel (SpuriousDiphthong _ _)) = "unknown spuriousdiphthong"
showText (MkVowel (ImproperDiphthong _)) = "unknown improperdiphthong"

