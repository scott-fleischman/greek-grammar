module Text.Greek.Lexicon.Lemmas where

import Text.Greek.Phonology.Phoneme
import Text.Greek.Phonology.Shorthand
import Text.Greek.Morphology.Noun
import Text.Greek.Morphology.PartOfSpeech

data Lemma = Lemma
  { stem :: [Phoneme]
  , partOfSpeech :: PartOfSpeech
  }

mas2D :: PartOfSpeech
mas2D =  MkNoun (SecondDeclension Masculine)

theos :: Lemma
theos = Lemma [θ, ε, σ] mas2D

hippos :: Lemma 
hippos = Lemma [ῐ, π, π] mas2D

anthropos :: Lemma
anthropos = Lemma [ᾰ, ν, θ, ρ, π] mas2D

doron :: Lemma
doron = Lemma [δ, ω, ρ] mas2D

nous :: Lemma
nous = Lemma [ν, ο] mas2D

periplous :: Lemma
periplous = Lemma [π, ε, ρ, ῑ, λ, ο] mas2D

ostous :: Lemma
ostous = Lemma [ο, σ, τ, ε] mas2D

anthropos :: Lemma
anthropos = [ᾰ, ν, θ, ρ, ω, π] mas2D

odos :: Lemma
odos = Lemma [ο, δ] mas2D

doron :: Lemma
doron = Lemma [δ, ω, ρ]

neos :: Lemma
neos = Lemma [ν, η, ο]






