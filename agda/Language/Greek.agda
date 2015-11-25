module Language.Greek where

data Maybe A : Set where
  none : Maybe A
  just : A → Maybe A

data Unicode : Set where
  Α Β Γ Δ Ε Ζ Η Θ Ι Κ Λ Μ Ν Ξ Ο Π Ρ Σ Τ Υ Φ Χ Ψ Ω α β γ δ ε ζ η θ ι κ λ′ μ ν ξ ο π ρ σ ς τ υ φ χ ψ ω : Unicode
  grave acute diaeresis smooth rough circumflex iotaSubscript : Unicode

postulate
  Char : Set

{-# BUILTIN CHAR Char #-}
{-# COMPILED_TYPE Char Char #-}

charToUnicode : Char -> Maybe Unicode
charToUnicode 'Α' = just Α
charToUnicode 'Β' = just Β
charToUnicode 'Γ' = just Γ
charToUnicode 'Δ' = just Δ
charToUnicode 'Ε' = just Ε
charToUnicode 'Ζ' = just Ζ
charToUnicode 'Η' = just Η
charToUnicode 'Θ' = just Θ
charToUnicode 'Ι' = just Ι
charToUnicode 'Κ' = just Κ
charToUnicode 'Λ' = just Λ
charToUnicode 'Μ' = just Μ
charToUnicode 'Ν' = just Ν
charToUnicode 'Ξ' = just Ξ
charToUnicode 'Ο' = just Ο
charToUnicode 'Π' = just Π
charToUnicode 'Ρ' = just Ρ
charToUnicode 'Σ' = just Σ
charToUnicode 'Τ' = just Τ
charToUnicode 'Υ' = just Υ
charToUnicode 'Φ' = just Φ
charToUnicode 'Χ' = just Χ
charToUnicode 'Ψ' = just Ψ
charToUnicode 'Ω' = just Ω
charToUnicode 'α' = just α
charToUnicode 'β' = just β
charToUnicode 'γ' = just γ
charToUnicode 'δ' = just δ
charToUnicode 'ε' = just ε
charToUnicode 'ζ' = just ζ
charToUnicode 'η' = just η
charToUnicode 'θ' = just θ
charToUnicode 'ι' = just ι
charToUnicode 'κ' = just κ
charToUnicode 'λ' = just λ′
charToUnicode 'μ' = just μ
charToUnicode 'ν' = just ν
charToUnicode 'ξ' = just ξ
charToUnicode 'ο' = just ο
charToUnicode 'π' = just π
charToUnicode 'ρ' = just ρ
charToUnicode 'ς' = just ς
charToUnicode 'σ' = just σ
charToUnicode 'τ' = just τ
charToUnicode 'υ' = just υ
charToUnicode 'φ' = just φ
charToUnicode 'χ' = just χ
charToUnicode 'ψ' = just ψ
charToUnicode 'ω' = just ω
charToUnicode '\x0300' = just grave          -- COMBINING GRAVE ACCENT
charToUnicode '\x0301' = just acute          -- COMBINING ACUTE ACCENT
charToUnicode '\x0308' = just diaeresis      -- COMBINING DIAERESIS
charToUnicode '\x0313' = just smooth         -- COMBINING COMMA ABOVE
charToUnicode '\x0314' = just rough          -- COMBINING REVERSED COMMA ABOVE
charToUnicode '\x0342' = just circumflex     -- COMBINING GREEK PERISPOMENI
charToUnicode '\x0345' = just iotaSubscript  -- COMBINING GREEK YPOGEGRAMMENI
charToUnicode _ = none

data UnicodeCategory : Set where
  letter mark : UnicodeCategory

unicodeToCategory : Unicode → UnicodeCategory
unicodeToCategory Α = letter
unicodeToCategory Β = letter
unicodeToCategory Γ = letter
unicodeToCategory Δ = letter
unicodeToCategory Ε = letter
unicodeToCategory Ζ = letter
unicodeToCategory Η = letter
unicodeToCategory Θ = letter
unicodeToCategory Ι = letter
unicodeToCategory Κ = letter
unicodeToCategory Λ = letter
unicodeToCategory Μ = letter
unicodeToCategory Ν = letter
unicodeToCategory Ξ = letter
unicodeToCategory Ο = letter
unicodeToCategory Π = letter
unicodeToCategory Ρ = letter
unicodeToCategory Σ = letter
unicodeToCategory Τ = letter
unicodeToCategory Υ = letter
unicodeToCategory Φ = letter
unicodeToCategory Χ = letter
unicodeToCategory Ψ = letter
unicodeToCategory Ω = letter
unicodeToCategory α = letter
unicodeToCategory β = letter
unicodeToCategory γ = letter
unicodeToCategory δ = letter
unicodeToCategory ε = letter
unicodeToCategory ζ = letter
unicodeToCategory η = letter
unicodeToCategory θ = letter
unicodeToCategory ι = letter
unicodeToCategory κ = letter
unicodeToCategory λ′ = letter
unicodeToCategory μ = letter
unicodeToCategory ν = letter
unicodeToCategory ξ = letter
unicodeToCategory ο = letter
unicodeToCategory π = letter
unicodeToCategory ρ = letter
unicodeToCategory σ = letter
unicodeToCategory ς = letter
unicodeToCategory τ = letter
unicodeToCategory υ = letter
unicodeToCategory φ = letter
unicodeToCategory χ = letter
unicodeToCategory ψ = letter
unicodeToCategory ω = letter
unicodeToCategory grave = mark
unicodeToCategory acute = mark
unicodeToCategory diaeresis = mark
unicodeToCategory smooth = mark
unicodeToCategory rough = mark
unicodeToCategory circumflex = mark
unicodeToCategory iotaSubscript = mark

data List A : Set where
  [] : List A
  _∷_ : A → List A -> List A

infixr 6 _∷_

foldr : {A B : Set} → (A → B → B) → B → List A → B
foldr f y [] = y
foldr f y (x ∷ xs) = f x (foldr f y xs)

data _And_ : (A : Set) → (B : Set) → Set₁ where
  _and_ : {A B : Set} → A → B → A And B

data ConcreteLetter : Set where
  Α Β Γ Δ Ε Ζ Η Θ Ι Κ Λ Μ Ν Ξ Ο Π Ρ Σ Τ Υ Φ Χ Ψ Ω α β γ δ ε ζ η θ ι κ λ′ μ ν ξ ο π ρ σ ς τ υ φ χ ψ ω : ConcreteLetter

data ConcreteMark : Set where
  grave acute diaeresis smooth rough circumflex iotaSubscript : ConcreteMark
 
