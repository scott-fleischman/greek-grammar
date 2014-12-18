module Language.Greek.Script where

open import Relation.Nullary using (¬_)

data Vowel : Set where
  α ε η ι ο υ ω : Vowel

data Consonant : Set where
  β γ δ ζ θ κ λ′ μ ν ξ π ρ σ τ φ χ ψ : Consonant

data Letter : Set where
  vowel : Vowel → Letter
  consonant : Consonant → Letter

data Length : Set where
  short long : Length

data VowelLength : Vowel → Length → Set where
  either-α : ∀ {l} → VowelLength α l
  either-ι : ∀ {l} → VowelLength ι l
  either-υ : ∀ {l} → VowelLength υ l
  short-ε : VowelLength ε short
  short-ο : VowelLength ο short
  long-η : VowelLength η long
  long-ω : VowelLength ω long

data Case : Set where
  upper lower : Case

data LetterCase : Letter → Case → Set where
  any-case : ∀ {l c} → LetterCase l c

data IotaSubscript : Vowel → Set where
  α_ι : IotaSubscript α
  η_ι : IotaSubscript η
  ω_ι : IotaSubscript ω


-- proofs about length

ε-not-long : ¬ VowelLength ε long
ε-not-long ()

ο-not-long : ¬ VowelLength ο long
ο-not-long ()

η-not-short : ¬ VowelLength η short
η-not-short ()

ω-not-short : ¬ VowelLength ω short
ω-not-short ()


-- proofs about iota subscript

ε-no-subscript : ¬ IotaSubscript ε
ε-no-subscript ()

ι-no-subscript : ¬ IotaSubscript ι
ι-no-subscript ()

ο-no-subscript : ¬ IotaSubscript ο
ο-no-subscript ()

υ-no-subscript : ¬ IotaSubscript υ
υ-no-subscript ()


