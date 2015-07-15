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
  is-always-short : ∀ {v} → v vowel → v ∈ always-short-letters → v always-short

data _diaeresis : Letter → Set where
  add-diaeresis : ∀ {v} → v vowel → v ∈ diaeresis-letters → v diaeresis

data _⟦_⟧-final : Letter → Case → Set where
  make-final : σ′ ⟦ lower ⟧-final

data _⟦_⟧-smooth : Letter → Case → Set where
  add-smooth-lower-vowel : ∀ {v} → v vowel → v ⟦ lower ⟧-smooth
  add-smooth-ρ : ρ′ ⟦ lower ⟧-smooth
  add-smooth-upper-vowel-not-Υ : ∀ {v} → v vowel → v ≢ υ′ → v ⟦ upper ⟧-smooth

data _with-rough : Letter → Set where
  add-rough-vowel : ∀ {v} → v vowel → v with-rough
  add-rough-ρ : ρ′ with-rough

data _iota-subscript : Letter → Set where
  add-iota-subscript : ∀ {v} → v vowel → v ∈ iota-subscript-letters → v iota-subscript

data _⟦_⟧-breathing : Letter → Case → Set where
  smooth : ∀ {ℓ c} → ℓ ⟦ c ⟧-smooth → ℓ ⟦ c ⟧-breathing
  rough : ∀ {ℓ c} → ℓ with-rough → ℓ ⟦ c ⟧-breathing

data _long-vowel : Letter → Set where
  make-long-vowel : ∀ {v} → v vowel → ¬ v always-short → v long-vowel

data _accent : Letter → Set where
  acute : ∀ {ℓ} → ℓ vowel → ℓ accent
  grave : ∀ {ℓ} → ℓ vowel → ℓ accent
  circumflex : ∀ {ℓ} → ℓ long-vowel → ℓ accent

data Token : Set where
  unmarked : (ℓ : Letter) → (c : Case) → Token
  with-accent : (ℓ : Letter) → (c : Case) → ℓ accent → Token
  with-breathing : (ℓ : Letter) → (c : Case) → ℓ ⟦ c ⟧-breathing → Token
  with-accent-breathing : (ℓ : Letter) → (c : Case) → ℓ accent → ℓ ⟦ c ⟧-breathing → Token
  with-accent-breathing-iota : (ℓ : Letter) → (c : Case) → ℓ accent → ℓ ⟦ c ⟧-breathing → ℓ iota-subscript → Token
  with-diaeresis : (ℓ : Letter) → (c : Case) → ℓ diaeresis → Token
  with-accent-diaeresis : (ℓ : Letter) → (c : Case) → ℓ accent → ℓ diaeresis → Token
  with-accent-iota : (ℓ : Letter) → (c : Case) → ℓ accent → ℓ iota-subscript → Token
  with-breathing-iota : (ℓ : Letter) → (c : Case) → ℓ ⟦ c ⟧-breathing → ℓ iota-subscript → Token
  with-iota : (ℓ : Letter) → (c : Case) → ℓ iota-subscript → Token
  final : (ℓ : Letter) → (c : Case) → ℓ ⟦ c ⟧-final → Token

-- Constructions

-- Α α
α-vowel : α′ vowel
α-vowel = is-vowel here

¬α-always-short : ¬ α′ always-short
¬α-always-short (is-always-short x (there (there ())))

α-long-vowel : α′ long-vowel
α-long-vowel = make-long-vowel α-vowel ¬α-always-short

α-smooth : α′ ⟦ lower ⟧-smooth
α-smooth = add-smooth-lower-vowel (is-vowel here)

Α-smooth : α′ ⟦ upper ⟧-smooth
Α-smooth = add-smooth-upper-vowel-not-Υ (is-vowel here) (λ ())

α-rough : α′ with-rough
α-rough = add-rough-vowel (is-vowel here)

α-iota-subscript : α′ iota-subscript
α-iota-subscript = add-iota-subscript (is-vowel here) here

-- Ε ε
ε-vowel : ε′ vowel
ε-vowel = is-vowel (there here)

ε-always-short : ε′ always-short
ε-always-short = is-always-short (is-vowel (there here)) here

ε-smooth : ε′ ⟦ lower ⟧-smooth
ε-smooth = add-smooth-lower-vowel (is-vowel (there here))

Ε-smooth : ε′ ⟦ upper ⟧-smooth
Ε-smooth = add-smooth-upper-vowel-not-Υ (is-vowel (there here)) (λ ())

ε-rough : ε′ with-rough
ε-rough = add-rough-vowel (is-vowel (there here))

-- Η η
η-vowel : η′ vowel
η-vowel = is-vowel (there (there here))

¬η-always-short : ¬ η′ always-short
¬η-always-short (is-always-short x (there (there ())))

η-long-vowel : η′ long-vowel
η-long-vowel = make-long-vowel (is-vowel (there (there here))) ¬η-always-short

η-smooth : η′ ⟦ lower ⟧-smooth
η-smooth = add-smooth-lower-vowel (is-vowel (there (there here)))

Η-smooth : η′ ⟦ upper ⟧-smooth
Η-smooth = add-smooth-upper-vowel-not-Υ (is-vowel (there (there here))) (λ ())

η-rough : η′ with-rough
η-rough = add-rough-vowel (is-vowel (there (there here)))

η-iota-subscript : η′ iota-subscript
η-iota-subscript = add-iota-subscript (is-vowel (there (there here))) (there here)

-- Ι ι

ι-vowel : ι′ vowel
ι-vowel = is-vowel (there (there (there here)))

¬ι-always-short : ¬ ι′ always-short
¬ι-always-short (is-always-short x (there (there ())))

ι-long-vowel : ι′ long-vowel
ι-long-vowel = make-long-vowel (is-vowel (there (there (there here)))) ¬ι-always-short

ι-smooth : ι′ ⟦ lower ⟧-smooth
ι-smooth = add-smooth-lower-vowel (is-vowel (there (there (there here))))

Ι-smooth : ι′ ⟦ upper ⟧-smooth
Ι-smooth = add-smooth-upper-vowel-not-Υ
             (is-vowel (there (there (there here)))) (λ ())

ι-rough : ι′ with-rough
ι-rough = add-rough-vowel (is-vowel (there (there (there here))))

ι-diaeresis : ι′ diaeresis
ι-diaeresis = add-diaeresis (is-vowel (there (there (there here)))) here

-- Ο ο
ο-vowel : ο′ vowel
ο-vowel = is-vowel (there (there (there (there here))))

ο-always-short : ο′ always-short
ο-always-short = is-always-short (is-vowel (there (there (there (there here)))))
                   (there here)

ο-smooth : ο′ ⟦ lower ⟧-smooth
ο-smooth = add-smooth-lower-vowel
             (is-vowel (there (there (there (there here)))))

Ο-smooth : ο′ ⟦ upper ⟧-smooth
Ο-smooth = add-smooth-upper-vowel-not-Υ
             (is-vowel (there (there (there (there here))))) (λ ())

ο-rough : ο′ with-rough
ο-rough = add-rough-vowel (is-vowel (there (there (there (there here)))))

-- Ρ ρ
ρ-smooth : ρ′ ⟦ lower ⟧-smooth
ρ-smooth = add-smooth-ρ

ρ-rough : ρ′ with-rough
ρ-rough = add-rough-ρ

-- Σ σ
σ-final : σ′ ⟦ lower ⟧-final
σ-final = make-final

-- Υ υ
υ-vowel : υ′ vowel
υ-vowel = is-vowel (there (there (there (there (there here)))))

¬υ-always-short : ¬ υ′ always-short
¬υ-always-short (is-always-short x (there (there ())))

υ-long-vowel : υ′ long-vowel
υ-long-vowel = make-long-vowel υ-vowel ¬υ-always-short

υ-smooth : υ′ ⟦ lower ⟧-smooth
υ-smooth = add-smooth-lower-vowel
             (is-vowel (there (there (there (there (there here))))))

υ-rough : υ′ with-rough
υ-rough = add-rough-vowel
            (is-vowel (there (there (there (there (there here))))))

υ-diaeresis : υ′ diaeresis
υ-diaeresis = add-diaeresis
                (is-vowel (there (there (there (there (there here))))))
                (there here)

-- Ω ω
ω-vowel : ω′ vowel
ω-vowel = is-vowel (there (there (there (there (there (there here))))))

¬ω-always-short : ¬ ω′ always-short
¬ω-always-short (is-always-short x (there (there ())))

ω-long-vowel : ω′ long-vowel
ω-long-vowel = make-long-vowel ω-vowel ¬ω-always-short

ω-smooth : ω′ ⟦ lower ⟧-smooth
ω-smooth = add-smooth-lower-vowel
             (is-vowel (there (there (there (there (there (there here)))))))

Ω-smooth : ω′ ⟦ upper ⟧-smooth
Ω-smooth = add-smooth-upper-vowel-not-Υ
             (is-vowel (there (there (there (there (there (there here)))))))
             (λ ())

ω-rough : ω′ with-rough
ω-rough = add-rough-vowel
            (is-vowel (there (there (there (there (there (there here)))))))

ω-iota-subscript : ω′ iota-subscript
ω-iota-subscript = add-iota-subscript
                     (is-vowel (there (there (there (there (there (there here)))))))
                     (there (there here))


-- Mapping

get-letter : Token → Letter
get-letter (unmarked ℓ _) = ℓ
get-letter (with-accent ℓ _ _) = ℓ
get-letter (with-breathing ℓ _ _) = ℓ
get-letter (with-accent-breathing ℓ _ _ _) = ℓ
get-letter (with-accent-breathing-iota ℓ _ _ _ _) = ℓ
get-letter (with-diaeresis ℓ _ _) = ℓ
get-letter (with-accent-diaeresis ℓ _ _ _) = ℓ
get-letter (with-accent-iota ℓ _ _ _) = ℓ
get-letter (with-breathing-iota ℓ _ _ _) = ℓ
get-letter (with-iota ℓ _ _) = ℓ
get-letter (final ℓ _ _) = ℓ

get-case : Token → Case
get-case (unmarked _ c) = c
get-case (with-accent _ c _) = c
get-case (with-breathing _ c _) = c
get-case (with-accent-breathing _ c _ _) = c
get-case (with-accent-breathing-iota _ c _ _ _) = c
get-case (with-diaeresis _ c _) = c
get-case (with-accent-diaeresis _ c _ _) = c
get-case (with-accent-iota _ c _ _) = c
get-case (with-breathing-iota _ c _ _) = c
get-case (with-iota _ c _) = c
get-case (final _ c _) = c

data Accent : Set where
  acute-mark grave-mark circumflex-mark : Accent

letter-to-accent : ∀ {v} → v accent → Accent
letter-to-accent (acute x) = acute-mark
letter-to-accent (grave x) = grave-mark
letter-to-accent (circumflex x) = circumflex-mark

get-accent : Token → Maybe Accent
get-accent (with-accent _ _ a) = just (letter-to-accent a)
get-accent (with-accent-breathing _ _ a _) = just (letter-to-accent a)
get-accent (with-accent-breathing-iota _ _ a _ _) = just (letter-to-accent a)
get-accent (with-accent-diaeresis _ _ a _) = just (letter-to-accent a)
get-accent (with-accent-iota _ _ a _) = just (letter-to-accent a)
get-accent _ = nothing

data Breathing : Set where
  smooth-mark rough-mark : Breathing

letter-to-breathing : ∀ {ℓ c} → ℓ ⟦ c ⟧-breathing → Breathing
letter-to-breathing (smooth x) = smooth-mark
letter-to-breathing (rough x) = rough-mark

get-breathing : Token → Maybe Breathing
get-breathing (with-breathing _ _ x) = just (letter-to-breathing x)
get-breathing (with-accent-breathing _ _ _ x) = just (letter-to-breathing x)
get-breathing (with-accent-breathing-iota _ _ _ x _) = just (letter-to-breathing x)
get-breathing (with-breathing-iota _ _ x _) = just (letter-to-breathing x)
get-breathing _ = nothing

data IotaSubscript : Set where
  iota-subscript-mark : IotaSubscript

get-iota-subscript : Token → Maybe IotaSubscript
get-iota-subscript (with-accent-breathing-iota _ _ _ _ _) = just iota-subscript-mark
get-iota-subscript (with-accent-iota _ _ _ _) = just iota-subscript-mark
get-iota-subscript (with-breathing-iota _ _ _ _) = just iota-subscript-mark
get-iota-subscript _ = nothing

data Diaeresis : Set where
  diaeresis-mark : Diaeresis

get-diaeresis : Token → Maybe Diaeresis
get-diaeresis (with-diaeresis _ _ _) = just diaeresis-mark
get-diaeresis (with-accent-diaeresis _ _ _ _) = just diaeresis-mark
get-diaeresis _ = nothing

data FinalForm : Set where
  final-form : FinalForm

get-final-form : Token → Maybe FinalForm
get-final-form (final _ _ _) = just final-form
get-final-form _ = nothing
