module Language.Greek.Script where

open import Relation.Nullary using (¬_)

data Vowel : Set where
  α ε η ι ο υ ω : Vowel

data Consonant : Set where
  β γ δ ζ θ κ λ′ μ ν ξ π ρ σ τ φ χ ψ : Consonant

data Letter : Set where
  vowel : Vowel → Letter
  consonant : Consonant → Letter

data LengthMark : Set where
  breve macron : LengthMark

data VowelLengthMark : Vowel → LengthMark → Set where
  either-α : ∀ {l} → VowelLengthMark α l
  either-ι : ∀ {l} → VowelLengthMark ι l
  either-υ : ∀ {l} → VowelLengthMark υ l

data Case : Set where
  upper lower : Case

data LetterCase : Letter → Case → Set where
  any-case : ∀ {l c} → LetterCase l c

data VowelIotaSubscript : Vowel → Set where
  ᾳ : VowelIotaSubscript α
  ῃ : VowelIotaSubscript η
  ῳ : VowelIotaSubscript ω

data VowelDiaeresis : Vowel → Set where
  diaeresis : ∀ {v} → VowelDiaeresis v

data Accent : Set where
  acute grave circumflex : Accent

data VowelAccent : Vowel → Accent → Set where
  vowelAcute : ∀ {v} → VowelAccent v acute
  vowelGrave : ∀ {v} → VowelAccent v grave
  vowelCircumflex-α : VowelAccent α circumflex
  vowelCircumflex-η : VowelAccent η circumflex
  vowelCircumflex-ι : VowelAccent ι circumflex
  vowelCircumflex-υ : VowelAccent υ circumflex
  vowelCircumflex-ω : VowelAccent ω circumflex

data Breathing : Set where
  smooth rough : Breathing

data LetterBreathing : Letter → Breathing → Set where
  vowelBreathing : ∀ {v b} → LetterBreathing v b
  rhoBreathing : ∀ {b} → LetterBreathing (consonant ρ) b

data Final : Letter → Set where
  ς : Final (consonant σ)

data MarkedLetter : Set where
  letter : (Letter) → MarkedLetter
  final : ∀ {ℓ} → (Final ℓ) → MarkedLetter
  vowelAccent : ∀ {v a} → (VowelAccent v a) → MarkedLetter
  letterBreathing : ∀ {ℓ b} → (LetterBreathing ℓ b) → MarkedLetter
  vowelAccentBreathing : ∀ {v a b} → (VowelAccent v a) → (LetterBreathing (vowel v) b) → MarkedLetter
  vowelDiaeresis : ∀ {v} → (VowelDiaeresis v) → MarkedLetter
  vowelAccentDiaeresis : ∀ {v a} → (VowelAccent v a) → (VowelDiaeresis v) → MarkedLetter
  vowelAccentLengthMark : ∀ {v a m} → (VowelAccent v a) → (VowelLengthMark v m) → MarkedLetter
  vowelAccentBreathingLengthMark : ∀ {v a b m} → (VowelAccent v a) → (LetterBreathing (vowel v) b) → (VowelLengthMark v m) → (VowelDiaeresis v) → MarkedLetter
  vowelAccentDiaeresisLengthMark : ∀ {v a m} → (VowelAccent v a) → (VowelDiaeresis v) → (VowelLengthMark v m) → MarkedLetter
  

-- proofs about iota subscript

ε-no-subscript : ¬ VowelIotaSubscript ε
ε-no-subscript ()

ι-no-subscript : ¬ VowelIotaSubscript ι
ι-no-subscript ()

ο-no-subscript : ¬ VowelIotaSubscript ο
ο-no-subscript ()

υ-no-subscript : ¬ VowelIotaSubscript υ
υ-no-subscript ()


