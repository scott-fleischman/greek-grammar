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

neut2D :: PartOfSpeech
neut2D = MkNoun (SecondDeclension Neuter)

fem2D :: PartOfSpeech
fem2D = MkNoun (SecondDeclension Feminine)

theos :: Lemma
theos = Lemma [θ, ε, σ] mas2D

hippos :: Lemma 
hippos = Lemma [ῐ, π, π] mas2D

anthropos :: Lemma
anthropos = Lemma [ᾰ, ν, θ, ρ, π] mas2D

nous :: Lemma
nous = Lemma [ν, ο] mas2D

periplous :: Lemma
periplous = Lemma [π, ε, ρ, ῑ, λ, ο] mas2D

ostous :: Lemma
ostous = Lemma [ο, σ, τ, ε] mas2D

odos :: Lemma
odos = Lemma [ο, δ] mas2D

neos :: Lemma
neos = Lemma [ν, η, ο] mas2D

logos :: Lemma
logos = Lemma [λ, ο, γ] mas2D

deros :: Lemma
deros = Lemma [δ, η, ρ] mas2D

doulos :: Lemma
doulos = Lemma [δ, ο, ῠ, λ] mas2D

kindunos :: Lemma
kindunos = Lemma [κ, ῐ, ν, δ, ν] mas2D

polemos :: Lemma
polemos = Lemma [π, ο, λ, ε, μ] mas2D

argos :: Lemma
argos = Lemma [ᾰ, ρ, γ] mas2D

potamos :: Lemma
potamos = Lemma [π, ο, τ, ᾰ, μ] mas2D

arithmos :: Lemma
arithmos = Lemma [ᾰ, ρ, ῐ, θ, μ] mas2D

nesos :: Lemma
nesos = Lemma [ν, ε, σ] fem2D

epeiros :: Lemma
epeiros = Lemma [η, π, ει, ρ] fem2D

tropos :: Lemma
tropos = Lemma [τ, ρ, ο, π] fem2D

doron :: Lemma
doron = Lemma [δ, ω, ρ] neut2D

ergon :: Lemma
ergon = Lemma [ε, ρ, γ] neut2D

pteron :: Lemma
pteron = Lemma [π, τ, ε, ρ] neut2D

deipnon :: Lemma
deipnon = Lemma [δ, ει, π, ν] neut2D











