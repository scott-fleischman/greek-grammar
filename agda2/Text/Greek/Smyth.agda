module Text.Greek.Smyth where

open import Data.Char
open import Data.Maybe

-- Smyth §1
data Letter : Set where
  α′ β′ γ′ δ′ ε′ ζ′ η′ θ′ ι′ κ′ λ′ μ′ ν′ ξ′ ο′ π′ ρ′ σ′ τ′ υ′ φ′ χ′ ψ′ ω′ : Letter

data LetterCase : Set where
  capital small : LetterCase

form : Letter → LetterCase → Char
form α′ capital = 'Α'
form α′ small = 'α'
form β′ capital = 'Β'
form β′ small = 'β'
form γ′ capital = 'Γ'
form γ′ small = 'γ'
form δ′ capital = 'Δ'
form δ′ small = 'δ'
form ε′ capital = 'Ε'
form ε′ small = 'ε'
form ζ′ capital = 'Ζ'
form ζ′ small = 'ζ'
form η′ capital = 'Η'
form η′ small = 'η'
form θ′ capital = 'Θ'
form θ′ small = 'θ'
form ι′ capital = 'Ι'
form ι′ small = 'ι'
form κ′ capital = 'Κ'
form κ′ small = 'κ'
form λ′ capital = 'Λ'
form λ′ small = 'λ'
form μ′ capital = 'Μ'
form μ′ small = 'μ'
form ν′ capital = 'Ν'
form ν′ small = 'ν'
form ξ′ capital = 'Ξ'
form ξ′ small = 'ξ'
form ο′ capital = 'Ο'
form ο′ small = 'ο'
form π′ capital = 'Π'
form π′ small = 'π'
form ρ′ capital = 'Ρ'
form ρ′ small = 'ρ'
form σ′ capital = 'Σ'
form σ′ small = 'σ'
form τ′ capital = 'Τ'
form τ′ small = 'τ'
form υ′ capital = 'Υ'
form υ′ small = 'υ'
form φ′ capital = 'Φ'
form φ′ small = 'φ'
form χ′ capital = 'Χ'
form χ′ small = 'χ'
form ψ′ capital = 'Ψ'
form ψ′ small = 'ψ'
form ω′ capital = 'Ω'
form ω′ small = 'ω'

-- Smyth §1 a
data PositionInWord : Set where
  not-end : PositionInWord
  end-of-word : PositionInWord

form-in-word : Letter → LetterCase → PositionInWord → Char
form-in-word σ′ small end-of-word = 'ς'
form-in-word ℓ c _ = form ℓ c

-- Smyth §3
data OlderLetter : Set where
  Ϝ′ : OlderLetter
  Ϙ′ : OlderLetter
  ϡ′ : OlderLetter

-- Smyth §4
data Vowel : Letter → Set where
  α-vowel : Vowel α′
  ε-vowel : Vowel ε′
  η-vowel : Vowel η′
  ι-vowel : Vowel ι′
  ο-vowel : Vowel ο′
  υ-vowel : Vowel υ′
  ω-vowel : Vowel ω′

data AlwaysShort : Letter → Set where
  ε-always-short : Vowel ε′ → AlwaysShort ε′
  ο-always-short : Vowel ο′ → AlwaysShort ο′

data AlwaysLong : Letter → Set where
  η-always-long : Vowel η′ → AlwaysLong η′
  ω-always-long : Vowel ω′ → AlwaysLong ω′

-- Smyth §4 All vowels with the circumflex (149) are long

-- Smyth §149
data Accent : Set where
  acute circumflex grave : Accent
