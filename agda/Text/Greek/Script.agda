module Text.Greek.Script where

open import Data.Maybe
open import Data.Vec
open import Relation.Nullary using (¬_)
open import Relation.Binary.PropositionalEquality using (_≢_)

data Case : Set where
  lower upper : Case

data Letter : Set where
  α β γ δ ε ζ η θ ι κ λ′ μ ν ξ ο π ρ σ τ υ φ χ ψ ω : Letter

vowels : Vec _ _
vowels = α ∷ ε ∷ η ∷ ι ∷ ο ∷ υ ∷ ω ∷ []

always-short-letters : Vec _ _
always-short-letters = ε ∷ ο ∷ []

diaeresis-letters : Vec _ _
diaeresis-letters = ι ∷ υ ∷ []

iota-subscript-letters : Vec _ _
iota-subscript-letters = α ∷ η ∷ ω ∷ []

data _vowel : Letter → Set where
  is-vowel : ∀ {v} → v ∈ vowels → v vowel

data _always-short : Letter → Set where
  is-always-short : ∀ {v} → ⦃ p : v vowel ⦄ → v ∈ always-short-letters → v always-short

data _diaeresis : Letter → Set where
  add-diaeresis : ∀ {v} → ⦃ p : v vowel ⦄ → v ∈ diaeresis-letters → v diaeresis

data _⟦_⟧-final : Letter → Case → Set where
  make-final : σ ⟦ lower ⟧-final

data _⟦_⟧-smooth : Letter → Case → Set where
  add-smooth-lower-vowel : ∀ {v} → ⦃ p : v vowel ⦄ → v ⟦ lower ⟧-smooth
  add-smooth-ρ : ρ ⟦ lower ⟧-smooth
  add-smooth-upper-vowel-not-Υ : ∀ {v} → ⦃ p : v vowel ⦄ → v ≢ υ → v ⟦ upper ⟧-smooth

data _with-rough : Letter → Set where
  add-rough-vowel : ∀ {v} → ⦃ p : v vowel ⦄ → v with-rough
  add-rough-ρ : ρ with-rough

data _iota-subscript : Letter → Set where
  add-iota-subscript : ∀ {v} → ⦃ p : v vowel ⦄ → v ∈ iota-subscript-letters → v iota-subscript

data _⟦_⟧-breathing : Letter → Case → Set where
  smooth : ∀ {ℓ c} → ⦃ p : ℓ ⟦ c ⟧-smooth ⦄ → ℓ ⟦ c ⟧-breathing
  rough : ∀ {ℓ c} → ⦃ p : ℓ with-rough ⦄ → ℓ ⟦ c ⟧-breathing

data _long-vowel : Letter → Set where
  make-long-vowel : ∀ {v} → ⦃ p : v vowel ⦄ → ¬ v always-short → v long-vowel

data _accent : Letter → Set where
  acute : ∀ {ℓ} → ⦃ p : ℓ vowel ⦄ → ℓ accent
  grave : ∀ {ℓ} → ⦃ p : ℓ vowel ⦄ → ℓ accent
  circumflex : ∀ {ℓ} → ⦃ p : ℓ long-vowel ⦄ → ℓ accent

data Token : Letter → Case → Set where
  unmarked : ∀ {ℓ c} → Token ℓ c
  with-accent : ∀ {ℓ c} → ℓ accent → Token ℓ c
  with-breathing : ∀ {ℓ c} → ℓ ⟦ c ⟧-breathing → Token ℓ c
  with-accent-breathing : ∀ {ℓ c} → ℓ accent → ℓ ⟦ c ⟧-breathing → Token ℓ c
  with-accent-breathing-iota : ∀ {ℓ c} → ℓ accent → ℓ ⟦ c ⟧-breathing → ⦃ p : ℓ iota-subscript ⦄ → Token ℓ c
  with-diaeresis : ∀ {ℓ c} → ⦃ p : ℓ diaeresis ⦄ → Token ℓ c
  with-accent-diaeresis : ∀ {ℓ c} → ℓ accent → ⦃ p : ℓ diaeresis ⦄ → Token ℓ c
  with-accent-iota : ∀ {ℓ c} → ℓ accent → ⦃ p : ℓ iota-subscript ⦄ → Token ℓ c
  with-breathing-iota : ∀ {ℓ c} → ℓ ⟦ c ⟧-breathing → ⦃ p : ℓ iota-subscript ⦄ → Token ℓ c
  with-iota : ∀ {ℓ c} → ⦃ p : ℓ iota-subscript ⦄ → Token ℓ c
  final : ∀ {ℓ c} → ⦃ p : ℓ ⟦ c ⟧-final ⦄ → Token ℓ c

-- Constructions

-- Α α
instance α-vowel : α vowel
α-vowel = is-vowel here

¬α-always-short : ¬ α always-short
¬α-always-short (is-always-short (there (there ())))

instance α-long-vowel : α long-vowel
α-long-vowel = make-long-vowel ¬α-always-short

instance α-smooth : α ⟦ lower ⟧-smooth
α-smooth = add-smooth-lower-vowel

instance Α-smooth : α ⟦ upper ⟧-smooth
Α-smooth = add-smooth-upper-vowel-not-Υ (λ ())

instance α-rough : α with-rough
α-rough = add-rough-vowel

instance α-iota-subscript : α iota-subscript
α-iota-subscript = add-iota-subscript here

-- Ε ε
instance ε-vowel : ε vowel
ε-vowel = is-vowel (there here)

ε-always-short : ε always-short
ε-always-short = is-always-short here

instance ε-smooth : ε ⟦ lower ⟧-smooth
ε-smooth = add-smooth-lower-vowel

instance Ε-smooth : ε ⟦ upper ⟧-smooth
Ε-smooth = add-smooth-upper-vowel-not-Υ (λ ())

instance ε-rough : ε with-rough
ε-rough = add-rough-vowel

-- Η η
instance η-vowel : η vowel
η-vowel = is-vowel (there (there here))

¬η-always-short : ¬ η always-short
¬η-always-short (is-always-short (there (there ())))

instance η-long-vowel : η long-vowel
η-long-vowel = make-long-vowel ¬η-always-short

instance η-smooth : η ⟦ lower ⟧-smooth
η-smooth = add-smooth-lower-vowel

instance Η-smooth : η ⟦ upper ⟧-smooth
Η-smooth = add-smooth-upper-vowel-not-Υ (λ ())

instance η-rough : η with-rough
η-rough = add-rough-vowel

instance η-iota-subscript : η iota-subscript
η-iota-subscript = add-iota-subscript (there here)

-- Ι ι

instance ι-vowel : ι vowel
ι-vowel = is-vowel (there (there (there here)))

¬ι-always-short : ¬ ι always-short
¬ι-always-short (is-always-short (there (there ())))

instance ι-long-vowel : ι long-vowel
ι-long-vowel = make-long-vowel ¬ι-always-short

instance ι-smooth : ι ⟦ lower ⟧-smooth
ι-smooth = add-smooth-lower-vowel

instance Ι-smooth : ι ⟦ upper ⟧-smooth
Ι-smooth = add-smooth-upper-vowel-not-Υ (λ ())

instance ι-rough : ι with-rough
ι-rough = add-rough-vowel

instance ι-diaeresis : ι diaeresis
ι-diaeresis = add-diaeresis here

-- Ο ο
instance ο-vowel : ο vowel
ο-vowel = is-vowel (there (there (there (there here))))

ο-always-short : ο always-short
ο-always-short = is-always-short (there here)

instance ο-smooth : ο ⟦ lower ⟧-smooth
ο-smooth = add-smooth-lower-vowel

instance Ο-smooth : ο ⟦ upper ⟧-smooth
Ο-smooth = add-smooth-upper-vowel-not-Υ (λ ())

instance ο-rough : ο with-rough
ο-rough = add-rough-vowel

-- Ρ ρ
instance ρ-smooth : ρ ⟦ lower ⟧-smooth
ρ-smooth = add-smooth-ρ

instance ρ-rough : ρ with-rough
ρ-rough = add-rough-ρ

-- Σ σ
instance σ-final : σ ⟦ lower ⟧-final
σ-final = make-final

-- Υ υ
instance υ-vowel : υ vowel
υ-vowel = is-vowel (there (there (there (there (there here)))))

¬υ-always-short : ¬ υ always-short
¬υ-always-short (is-always-short (there (there ())))

instance υ-long-vowel : υ long-vowel
υ-long-vowel = make-long-vowel ¬υ-always-short

instance υ-smooth : υ ⟦ lower ⟧-smooth
υ-smooth = add-smooth-lower-vowel

instance υ-rough : υ with-rough
υ-rough = add-rough-vowel

instance υ-diaeresis : υ diaeresis
υ-diaeresis = add-diaeresis (there here)

-- Ω ω
instance ω-vowel : ω vowel
ω-vowel = is-vowel (there (there (there (there (there (there here))))))

¬ω-always-short : ¬ ω always-short
¬ω-always-short (is-always-short (there (there ())))

instance ω-long-vowel : ω long-vowel
ω-long-vowel = make-long-vowel ¬ω-always-short

instance ω-smooth : ω ⟦ lower ⟧-smooth
ω-smooth = add-smooth-lower-vowel

instance Ω-smooth : ω ⟦ upper ⟧-smooth
Ω-smooth = add-smooth-upper-vowel-not-Υ (λ ())

instance ω-rough : ω with-rough
ω-rough = add-rough-vowel

instance ω-iota-subscript : ω iota-subscript
ω-iota-subscript = add-iota-subscript (there (there here))


-- Mapping

data Accent : Set where
  acute-mark grave-mark circumflex-mark : Accent

letter-to-accent : ∀ {v} → v accent → Accent
letter-to-accent acute = acute-mark
letter-to-accent grave = grave-mark
letter-to-accent circumflex = circumflex-mark

get-accent : ∀ {ℓ c} → Token ℓ c → Maybe Accent
get-accent (with-accent a) = just (letter-to-accent a)
get-accent (with-accent-breathing a _) = just (letter-to-accent a)
get-accent (with-accent-breathing-iota a _) = just (letter-to-accent a)
get-accent (with-accent-diaeresis a) = just (letter-to-accent a)
get-accent (with-accent-iota a) = just (letter-to-accent a)
get-accent _ = nothing

data Breathing : Set where
  smooth-mark rough-mark : Breathing

letter-to-breathing : ∀ {ℓ c} → ℓ ⟦ c ⟧-breathing → Breathing
letter-to-breathing smooth = smooth-mark
letter-to-breathing rough = rough-mark

get-breathing : ∀ {ℓ c} → Token ℓ c → Maybe Breathing
get-breathing (with-breathing x) = just (letter-to-breathing x)
get-breathing (with-accent-breathing _ x) = just (letter-to-breathing x)
get-breathing (with-accent-breathing-iota _ x) = just (letter-to-breathing x)
get-breathing (with-breathing-iota x) = just (letter-to-breathing x)
get-breathing _ = nothing

data IotaSubscript : Set where
  iota-subscript-mark : IotaSubscript

get-iota-subscript : ∀ {ℓ c} → Token ℓ c → Maybe IotaSubscript
get-iota-subscript (with-accent-breathing-iota _ _) = just iota-subscript-mark
get-iota-subscript (with-accent-iota _) = just iota-subscript-mark
get-iota-subscript (with-breathing-iota _) = just iota-subscript-mark
get-iota-subscript _ = nothing

data Diaeresis : Set where
  diaeresis-mark : Diaeresis

get-diaeresis : ∀ {ℓ c} → Token ℓ c → Maybe Diaeresis
get-diaeresis with-diaeresis = just diaeresis-mark
get-diaeresis (with-accent-diaeresis _) = just diaeresis-mark
get-diaeresis _ = nothing

data FinalForm : Set where
  final-form : FinalForm

get-final-form : ∀ {ℓ c} → Token ℓ c → Maybe FinalForm
get-final-form final = just final-form
get-final-form _ = nothing
