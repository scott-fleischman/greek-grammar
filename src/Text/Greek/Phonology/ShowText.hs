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
showText _ = "error"
