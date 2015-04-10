module Text.Greek.Phonology.Shorthand where

import Text.Greek.Phonology.Phoneme
import Text.Greek.Phonology.Consonants
import Text.Greek.Phonology.Vowels

θ :: Phoneme
θ = MkConsonant Theta

ε :: Phoneme
ε = MkVowel (VowelPhoneme Epsilon Short)

ο :: Phoneme
ο = MkVowel (VowelPhoneme Omicron Short)

σ :: Phoneme
σ = MkConsonant Sigma

ῠ :: Phoneme
ῠ = MkVowel (VowelPhoneme Upsilon Short)

ῡ :: Phoneme
ῡ = MkVowel (VowelPhoneme Upsilon Long)
