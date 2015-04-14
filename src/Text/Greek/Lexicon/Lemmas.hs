module Text.Greek.Lexicon.Lemmas where

import Control.Lens
import Text.Greek.Phonology.Phoneme
import Text.Greek.Phonology.Shorthand
import Text.Greek.Morphology.Noun
import Text.Greek.Morphology.PartOfSpeech

data Lemma = Lemma
  { stem :: [Phoneme]
  , partOfSpeech :: PartOfSpeech
  }

paradigm :: Lemma -> [(NounInflection, [Phoneme])]
paradigm (Lemma s (MkNoun n)) = [e & _2 %~ (s ++) | e <- allNounEndings n]
paradigm _ = []

masc2Ds :: [Lemma]
masc2Ds = flip Lemma (MkNoun (SecondDeclension Masculine)) <$>
  [ [ᾰ, ν, θ, ρ, π]
  , [ᾰ, ρ, γ]
  , [ᾰ, ρ, ῐ, θ, μ]
  , [δ, η, ρ]
  , [δ, ου, λ]
  , [θ, ε, σ]
  , [rough, ῐ, π, π]
  , [κ, ῐ, ν, δ, ν]
  , [λ, ο, γ]
  , [ν, η, ο]
  , [ν, ο]
  , [π, ε, ρ, ῑ, λ, ο]
  , [π, λ, ο]
  , [π, ο, λ, ε, μ]
  , [π, ο, τ, ᾰ, μ]
  , [ρ, ο]
  ]

fem2Ds :: [Lemma]
fem2Ds = flip Lemma (MkNoun (SecondDeclension Feminine)) <$>
  [ [η, π, ει, ρ]
  , [ν, ε, σ]
  , [ο, δ]
  , [τ, ρ, ο, π]
  ]

neut2Ds :: [Lemma]
neut2Ds = flip Lemma (MkNoun (SecondDeclension Neuter)) <$>
  [ [δ, ει, π, ν]
  , [δ, ω, ρ]
  , [ε, ρ, γ]
  , [κ, ᾰ, ν, ε]
  , [ο, σ, τ, ε]
  , [π, τ, ε, ρ]
  ]
