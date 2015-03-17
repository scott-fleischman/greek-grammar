module Text.Greek.Script where

open import Data.Maybe
open import Data.Vec
open import Relation.Nullary using (¬_)
open import Relation.Binary.PropositionalEquality using (_≢_)

data Case : Set where
  lower upper : Case

data Letter : Set where
  α′ β′ γ′ δ′ ε′ ζ′ η′ θ′ ι′ κ′ λ′ μ′ ν′ ξ′ ο′ π′ ρ′ σ′ τ′ υ′ φ′ χ′ ψ′ ω′ : Letter

vowels : Vec _ _
vowels = α′ ∷ ε′ ∷ η′ ∷ ι′ ∷ ο′ ∷ υ′ ∷ ω′ ∷ []

always-short-letters : Vec _ _
always-short-letters = ε′ ∷ ο′ ∷ []

diaeresis-letters : Vec _ _
diaeresis-letters = ι′ ∷ υ′ ∷ []

iota-subscript-letters : Vec _ _
iota-subscript-letters = α′ ∷ η′ ∷ ω′ ∷ []

data _vowel : Letter → Set where
  is-vowel : ∀ {v} → v ∈ vowels → v vowel

data _always-short : Letter → Set where
  is-always-short : ∀ {v} → ⦃ p : v vowel ⦄ → v ∈ always-short-letters → v always-short

data _diaeresis : Letter → Set where
  add-diaeresis : ∀ {v} → ⦃ p : v vowel ⦄ → v ∈ diaeresis-letters → v diaeresis

data _⟦_⟧-final : Letter → Case → Set where
  make-final : σ′ ⟦ lower ⟧-final

data _⟦_⟧-smooth : Letter → Case → Set where
  add-smooth-lower-vowel : ∀ {v} → ⦃ p : v vowel ⦄ → v ⟦ lower ⟧-smooth
  add-smooth-ρ : ρ′ ⟦ lower ⟧-smooth
  add-smooth-upper-vowel-not-Υ : ∀ {v} → ⦃ p : v vowel ⦄ → v ≢ υ′ → v ⟦ upper ⟧-smooth

data _with-rough : Letter → Set where
  add-rough-vowel : ∀ {v} → ⦃ p : v vowel ⦄ → v with-rough
  add-rough-ρ : ρ′ with-rough

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

data Token : Set where
  unmarked : (ℓ : Letter) → (c : Case) → Token
  with-accent : (ℓ : Letter) → (c : Case) → ℓ accent → Token
  with-breathing : (ℓ : Letter) → (c : Case) → ℓ ⟦ c ⟧-breathing → Token
  with-accent-breathing : (ℓ : Letter) → (c : Case) → ℓ accent → ℓ ⟦ c ⟧-breathing → Token
  with-accent-breathing-iota : (ℓ : Letter) → (c : Case) → ℓ accent → ℓ ⟦ c ⟧-breathing → ⦃ p : ℓ iota-subscript ⦄ → Token
  with-diaeresis : (ℓ : Letter) → (c : Case) → ⦃ p : ℓ diaeresis ⦄ → Token
  with-accent-diaeresis : (ℓ : Letter) → (c : Case) → ℓ accent → ⦃ p : ℓ diaeresis ⦄ → Token
  with-accent-iota : (ℓ : Letter) → (c : Case) → ℓ accent → ⦃ p : ℓ iota-subscript ⦄ → Token
  with-breathing-iota : (ℓ : Letter) → (c : Case) → ℓ ⟦ c ⟧-breathing → ⦃ p : ℓ iota-subscript ⦄ → Token
  with-iota : (ℓ : Letter) → (c : Case) → ⦃ p : ℓ iota-subscript ⦄ → Token
  final : (ℓ : Letter) → (c : Case) → ⦃ p : ℓ ⟦ c ⟧-final ⦄ → Token

-- Constructions

-- Α α
instance α-vowel : α′ vowel
α-vowel = is-vowel here

¬α-always-short : ¬ α′ always-short
¬α-always-short (is-always-short (there (there ())))

instance α-long-vowel : α′ long-vowel
α-long-vowel = make-long-vowel ¬α-always-short

instance α-smooth : α′ ⟦ lower ⟧-smooth
α-smooth = add-smooth-lower-vowel

instance Α-smooth : α′ ⟦ upper ⟧-smooth
Α-smooth = add-smooth-upper-vowel-not-Υ (λ ())

instance α-rough : α′ with-rough
α-rough = add-rough-vowel

instance α-iota-subscript : α′ iota-subscript
α-iota-subscript = add-iota-subscript here

-- Ε ε
instance ε-vowel : ε′ vowel
ε-vowel = is-vowel (there here)

ε-always-short : ε′ always-short
ε-always-short = is-always-short here

instance ε-smooth : ε′ ⟦ lower ⟧-smooth
ε-smooth = add-smooth-lower-vowel

instance Ε-smooth : ε′ ⟦ upper ⟧-smooth
Ε-smooth = add-smooth-upper-vowel-not-Υ (λ ())

instance ε-rough : ε′ with-rough
ε-rough = add-rough-vowel

-- Η η
instance η-vowel : η′ vowel
η-vowel = is-vowel (there (there here))

¬η-always-short : ¬ η′ always-short
¬η-always-short (is-always-short (there (there ())))

instance η-long-vowel : η′ long-vowel
η-long-vowel = make-long-vowel ¬η-always-short

instance η-smooth : η′ ⟦ lower ⟧-smooth
η-smooth = add-smooth-lower-vowel

instance Η-smooth : η′ ⟦ upper ⟧-smooth
Η-smooth = add-smooth-upper-vowel-not-Υ (λ ())

instance η-rough : η′ with-rough
η-rough = add-rough-vowel

instance η-iota-subscript : η′ iota-subscript
η-iota-subscript = add-iota-subscript (there here)

-- Ι ι

instance ι-vowel : ι′ vowel
ι-vowel = is-vowel (there (there (there here)))

¬ι-always-short : ¬ ι′ always-short
¬ι-always-short (is-always-short (there (there ())))

instance ι-long-vowel : ι′ long-vowel
ι-long-vowel = make-long-vowel ¬ι-always-short

instance ι-smooth : ι′ ⟦ lower ⟧-smooth
ι-smooth = add-smooth-lower-vowel

instance Ι-smooth : ι′ ⟦ upper ⟧-smooth
Ι-smooth = add-smooth-upper-vowel-not-Υ (λ ())

instance ι-rough : ι′ with-rough
ι-rough = add-rough-vowel

instance ι-diaeresis : ι′ diaeresis
ι-diaeresis = add-diaeresis here

-- Ο ο
instance ο-vowel : ο′ vowel
ο-vowel = is-vowel (there (there (there (there here))))

ο-always-short : ο′ always-short
ο-always-short = is-always-short (there here)

instance ο-smooth : ο′ ⟦ lower ⟧-smooth
ο-smooth = add-smooth-lower-vowel

instance Ο-smooth : ο′ ⟦ upper ⟧-smooth
Ο-smooth = add-smooth-upper-vowel-not-Υ (λ ())

instance ο-rough : ο′ with-rough
ο-rough = add-rough-vowel

-- Ρ ρ
instance ρ-smooth : ρ′ ⟦ lower ⟧-smooth
ρ-smooth = add-smooth-ρ

instance ρ-rough : ρ′ with-rough
ρ-rough = add-rough-ρ

-- Σ σ
instance σ-final : σ′ ⟦ lower ⟧-final
σ-final = make-final

-- Υ υ
instance υ-vowel : υ′ vowel
υ-vowel = is-vowel (there (there (there (there (there here)))))

¬υ-always-short : ¬ υ′ always-short
¬υ-always-short (is-always-short (there (there ())))

instance υ-long-vowel : υ′ long-vowel
υ-long-vowel = make-long-vowel ¬υ-always-short

instance υ-smooth : υ′ ⟦ lower ⟧-smooth
υ-smooth = add-smooth-lower-vowel

instance υ-rough : υ′ with-rough
υ-rough = add-rough-vowel

instance υ-diaeresis : υ′ diaeresis
υ-diaeresis = add-diaeresis (there here)

-- Ω ω
instance ω-vowel : ω′ vowel
ω-vowel = is-vowel (there (there (there (there (there (there here))))))

¬ω-always-short : ¬ ω′ always-short
¬ω-always-short (is-always-short (there (there ())))

instance ω-long-vowel : ω′ long-vowel
ω-long-vowel = make-long-vowel ¬ω-always-short

instance ω-smooth : ω′ ⟦ lower ⟧-smooth
ω-smooth = add-smooth-lower-vowel

instance Ω-smooth : ω′ ⟦ upper ⟧-smooth
Ω-smooth = add-smooth-upper-vowel-not-Υ (λ ())

instance ω-rough : ω′ with-rough
ω-rough = add-rough-vowel

instance ω-iota-subscript : ω′ iota-subscript
ω-iota-subscript = add-iota-subscript (there (there here))


-- Mapping

get-letter : Token → Letter
get-letter (unmarked ℓ _) = ℓ
get-letter (with-accent ℓ _ _) = ℓ
get-letter (with-breathing ℓ _ _) = ℓ
get-letter (with-accent-breathing ℓ _ _ _) = ℓ
get-letter (with-accent-breathing-iota ℓ _ _ _) = ℓ
get-letter (with-diaeresis ℓ _) = ℓ
get-letter (with-accent-diaeresis ℓ _ _) = ℓ
get-letter (with-accent-iota ℓ _ _) = ℓ
get-letter (with-breathing-iota ℓ _ _) = ℓ
get-letter (with-iota ℓ _) = ℓ
get-letter (final ℓ _) = ℓ

get-case : Token → Case
get-case (unmarked _ c) = c
get-case (with-accent _ c _) = c
get-case (with-breathing _ c _) = c
get-case (with-accent-breathing _ c _ _) = c
get-case (with-accent-breathing-iota _ c _ _) = c
get-case (with-diaeresis _ c) = c
get-case (with-accent-diaeresis _ c _) = c
get-case (with-accent-iota _ c _) = c
get-case (with-breathing-iota _ c _) = c
get-case (with-iota _ c) = c
get-case (final _ c) = c

data Accent : Set where
  acute-mark grave-mark circumflex-mark : Accent

letter-to-accent : ∀ {v} → v accent → Accent
letter-to-accent acute = acute-mark
letter-to-accent grave = grave-mark
letter-to-accent circumflex = circumflex-mark

get-accent : Token → Maybe Accent
get-accent (with-accent _ _ a) = just (letter-to-accent a)
get-accent (with-accent-breathing _ _ a _) = just (letter-to-accent a)
get-accent (with-accent-breathing-iota _ _ a _) = just (letter-to-accent a)
get-accent (with-accent-diaeresis _ _ a) = just (letter-to-accent a)
get-accent (with-accent-iota _ _ a) = just (letter-to-accent a)
get-accent _ = nothing

data Breathing : Set where
  smooth-mark rough-mark : Breathing

letter-to-breathing : ∀ {ℓ c} → ℓ ⟦ c ⟧-breathing → Breathing
letter-to-breathing smooth = smooth-mark
letter-to-breathing rough = rough-mark

get-breathing : Token → Maybe Breathing
get-breathing (with-breathing _ _ x) = just (letter-to-breathing x)
get-breathing (with-accent-breathing _ _ _ x) = just (letter-to-breathing x)
get-breathing (with-accent-breathing-iota _ _ _ x) = just (letter-to-breathing x)
get-breathing (with-breathing-iota _ _ x) = just (letter-to-breathing x)
get-breathing _ = nothing

data IotaSubscript : Set where
  iota-subscript-mark : IotaSubscript

get-iota-subscript : Token → Maybe IotaSubscript
get-iota-subscript (with-accent-breathing-iota _ _ _ _) = just iota-subscript-mark
get-iota-subscript (with-accent-iota _ _ _) = just iota-subscript-mark
get-iota-subscript (with-breathing-iota _ _ _) = just iota-subscript-mark
get-iota-subscript _ = nothing

data Diaeresis : Set where
  diaeresis-mark : Diaeresis

get-diaeresis : Token → Maybe Diaeresis
get-diaeresis (with-diaeresis _ _) = just diaeresis-mark
get-diaeresis (with-accent-diaeresis _ _ _) = just diaeresis-mark
get-diaeresis _ = nothing

data FinalForm : Set where
  final-form : FinalForm

get-final-form : Token → Maybe FinalForm
get-final-form (final _ _) = just final-form
get-final-form _ = nothing
