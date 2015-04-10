module Text.Greek.Phonology.Shorthand where

import Text.Greek.Phonology.Phoneme
import Text.Greek.Phonology.Consonants
import Text.Greek.Phonology.Vowels

ᾰ :: Phoneme
ᾰ = MkVowel (alpha Short)

ᾱ :: Phoneme
ᾱ = MkVowel (alpha Long)

ε :: Phoneme
ε = MkVowel epsilon

η :: Phoneme
η = MkVowel eta

θ :: Phoneme
θ = MkConsonant Theta

ῐ :: Phoneme
ῐ = MkVowel (iota Short)

ῑ :: Phoneme
ῑ = MkVowel (iota Long)

ο :: Phoneme
ο = MkVowel omicron

σ :: Phoneme
σ = MkConsonant Sigma

ῠ :: Phoneme
ῠ = MkVowel (upsilon Short)

ῡ :: Phoneme
ῡ = MkVowel (upsilon Long)

ω :: Phoneme
ω = MkVowel omega


αι :: Phoneme
αι = MkVowel alphaIota

αυ :: Phoneme
αυ = MkVowel alphaUpsilon

ει :: Phoneme
ει = MkVowel epsilonIota

ει' :: Phoneme
ει' = MkVowel spuriousEI

ευ :: Phoneme
ευ = MkVowel epsilonUpsilon

ηυ :: Phoneme
ηυ = MkVowel etaUpsilon

οι :: Phoneme
οι = MkVowel omicronIota

ου :: Phoneme
ου = MkVowel omicronUpsilon

ου' :: Phoneme
ου' = MkVowel spuriousOU


ᾳ :: Phoneme
ᾳ = MkVowel improperAlpha

ῃ :: Phoneme
ῃ = MkVowel improperAlpha

ῳ :: Phoneme
ῳ = MkVowel improperOmega

