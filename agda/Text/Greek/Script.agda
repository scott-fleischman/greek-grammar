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


-- Unicode
-- U+0390 - U+03CE
ΐ : Token ι′ lower -- U+0390 GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
ΐ = with-accent-diaeresis acute
Α : Token α′ upper -- U+0391 GREEK CAPITAL LETTER ALPHA
Α = unmarked
Β : Token β′ upper -- U+0392 GREEK CAPITAL LETTER BETA
Β = unmarked
Γ : Token γ′ upper -- U+0393 GREEK CAPITAL LETTER GAMMA
Γ = unmarked
Δ : Token δ′ upper -- U+0394 GREEK CAPITAL LETTER DELTA
Δ = unmarked
Ε : Token ε′ upper -- U+0395 GREEK CAPITAL LETTER EPSILON
Ε = unmarked
Ζ : Token ζ′ upper -- U+0396 GREEK CAPITAL LETTER ZETA
Ζ = unmarked
Η : Token η′ upper -- U+0397 GREEK CAPITAL LETTER ETA
Η = unmarked
Θ : Token θ′ upper -- U+0398 GREEK CAPITAL LETTER THETA
Θ = unmarked
Ι : Token ι′ upper -- U+0399 GREEK CAPITAL LETTER IOTA
Ι = unmarked
Κ : Token κ′ upper -- U+039A GREEK CAPITAL LETTER KAPPA
Κ = unmarked
Λ : Token λ′ upper -- U+039B GREEK CAPITAL LETTER LAMDA
Λ = unmarked
Μ : Token μ′ upper -- U+039C GREEK CAPITAL LETTER MU
Μ = unmarked
Ν : Token ν′ upper -- U+039D GREEK CAPITAL LETTER NU
Ν = unmarked
Ξ : Token ξ′ upper -- U+039E GREEK CAPITAL LETTER XI
Ξ = unmarked
Ο : Token ο′ upper -- U+039F GREEK CAPITAL LETTER OMICRON
Ο = unmarked
Π : Token π′ upper -- U+03A0 GREEK CAPITAL LETTER PI
Π = unmarked
Ρ : Token ρ′ upper -- U+03A1 GREEK CAPITAL LETTER RHO
Ρ = unmarked
                  -- U+03A2 <reserved>
Σ : Token σ′ upper -- U+03A3 GREEK CAPITAL LETTER SIGMA
Σ = unmarked
Τ : Token τ′ upper -- U+03A4 GREEK CAPITAL LETTER TAU
Τ = unmarked
Υ : Token υ′ upper -- U+03A5 GREEK CAPITAL LETTER UPSILON
Υ = unmarked
Φ : Token φ′ upper -- U+03A6 GREEK CAPITAL LETTER PHI
Φ = unmarked
Χ : Token χ′ upper -- U+03A7 GREEK CAPITAL LETTER CHI
Χ = unmarked
Ψ : Token ψ′ upper -- U+03A8 GREEK CAPITAL LETTER PSI
Ψ = unmarked
Ω : Token ω′ upper -- U+03A9 GREEK CAPITAL LETTER OMEGA
Ω = unmarked
Ϊ : Token ι′ upper -- U+03AA GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
Ϊ = with-diaeresis
Ϋ : Token υ′ upper -- U+03AB GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
Ϋ = with-diaeresis
ά : Token α′ lower -- U+03AC GREEK SMALL LETTER ALPHA WITH TONOS
ά = with-accent acute
έ : Token ε′ lower -- U+03AD GREEK SMALL LETTER EPSILON WITH TONOS
έ = with-accent acute
ή : Token η′ lower -- U+03AE GREEK SMALL LETTER ETA WITH TONOS
ή = with-accent acute
ί : Token ι′ lower -- U+03AF GREEK SMALL LETTER IOTA WITH TONOS
ί = with-accent acute
ΰ : Token υ′ lower -- U+03B0 GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
ΰ = with-accent-diaeresis acute
α : Token α′ lower -- U+03B1 GREEK SMALL LETTER ALPHA
α = unmarked
β : Token β′ lower -- U+03B2 GREEK SMALL LETTER BETA
β = unmarked
γ : Token γ′ lower -- U+03B3 GREEK SMALL LETTER GAMMA
γ = unmarked
δ : Token δ′ lower -- U+03B4 GREEK SMALL LETTER DELTA
δ = unmarked
ε : Token ε′ lower -- U+03B5 GREEK SMALL LETTER EPSILON
ε = unmarked
ζ : Token ζ′ lower -- U+03B6 GREEK SMALL LETTER ZETA
ζ = unmarked
η : Token η′ lower -- U+03B7 GREEK SMALL LETTER ETA
η = unmarked
θ : Token θ′ lower -- U+03B8 GREEK SMALL LETTER THETA
θ = unmarked
ι : Token ι′ lower -- U+03B9 GREEK SMALL LETTER IOTA
ι = unmarked
κ : Token κ′ lower -- U+03BA GREEK SMALL LETTER KAPPA
κ = unmarked
∙λ : Token λ′ lower -- U+03BB GREEK SMALL LETTER LAMDA
∙λ = unmarked
μ : Token μ′ lower -- U+03BC GREEK SMALL LETTER MU
μ = unmarked
ν : Token ν′ lower -- U+03BD GREEK SMALL LETTER NU
ν = unmarked
ξ : Token ξ′ lower -- U+03BE GREEK SMALL LETTER XI
ξ = unmarked
ο : Token ο′ lower -- U+03BF GREEK SMALL LETTER OMICRON
ο = unmarked
π : Token π′ lower -- U+03C0 GREEK SMALL LETTER PI
π = unmarked
ρ : Token ρ′ lower -- U+03C1 GREEK SMALL LETTER RHO
ρ = unmarked
ς : Token σ′ lower -- U+03C2 GREEK SMALL LETTER FINAL SIGMA
ς = final
σ : Token σ′ lower -- U+03C3 GREEK SMALL LETTER SIGMA
σ = unmarked
τ : Token τ′ lower -- U+03C4 GREEK SMALL LETTER TAU
τ = unmarked
υ : Token υ′ lower -- U+03C5 GREEK SMALL LETTER UPSILON
υ = unmarked
φ : Token φ′ lower -- U+03C6 GREEK SMALL LETTER PHI
φ = unmarked
χ : Token χ′ lower -- U+03C7 GREEK SMALL LETTER CHI
χ = unmarked
ψ : Token ψ′ lower -- U+03C8 GREEK SMALL LETTER PSI
ψ = unmarked
ω : Token ω′ lower -- U+03C9 GREEK SMALL LETTER OMEGA
ω = unmarked
ϊ : Token ι′ lower -- U+03CA GREEK SMALL LETTER IOTA WITH DIALYTIKA
ϊ = with-diaeresis
ϋ : Token υ′ lower -- U+03CB GREEK SMALL LETTER UPSILON WITH DIALYTIKA
ϋ = with-diaeresis
ό : Token ο′ lower -- U+03CC GREEK SMALL LETTER OMICRON WITH TONOS
ό = with-accent acute
ύ : Token υ′ lower -- U+03CD GREEK SMALL LETTER UPSILON WITH TONOS
ύ = with-accent acute
ώ : Token ω′ lower -- U+03CE GREEK SMALL LETTER OMEGA WITH TONOS
ώ = with-accent acute


ἀ : Token α′ lower
ἀ = with-breathing smooth
ἁ : Token α′ lower
ἁ = with-breathing rough
ἂ : Token α′ lower
ἂ = with-accent-breathing grave smooth
ἃ : Token α′ lower
ἃ = with-accent-breathing grave rough
ἄ : Token α′ lower
ἄ = with-accent-breathing acute smooth
ἅ : Token α′ lower
ἅ = with-accent-breathing acute rough
ἆ : Token α′ lower
ἆ = with-accent-breathing circumflex smooth
ἇ : Token α′ lower
ἇ = with-accent-breathing circumflex rough
Ἀ : Token α′ upper
Ἀ = with-breathing smooth
Ἁ : Token α′ upper
Ἁ = with-breathing rough
Ἂ : Token α′ upper
Ἂ = with-accent-breathing grave smooth
Ἃ : Token α′ upper
Ἃ = with-accent-breathing grave rough
Ἄ : Token α′ upper
Ἄ = with-accent-breathing acute smooth
Ἅ : Token α′ upper
Ἅ = with-accent-breathing acute rough
Ἆ : Token α′ upper
Ἆ = with-accent-breathing circumflex smooth
Ἇ : Token α′ upper
Ἇ = with-accent-breathing circumflex rough

-- U+1F7x
ὰ : Token α′ lower
ὰ = with-accent grave
ά : Token α′ lower
ά = with-accent acute

-- U+1F8x
ᾀ : Token α′ lower
ᾀ = with-breathing-iota smooth
ᾁ : Token α′ lower
ᾁ = with-breathing-iota rough
ᾂ : Token α′ lower
ᾂ = with-accent-breathing-iota grave smooth
ᾃ : Token α′ lower
ᾃ = with-accent-breathing-iota grave rough
ᾄ : Token α′ lower
ᾄ = with-accent-breathing-iota acute smooth
ᾅ : Token α′ lower
ᾅ = with-accent-breathing-iota acute rough
ᾆ : Token α′ lower
ᾆ = with-accent-breathing-iota circumflex smooth
ᾇ : Token α′ lower
ᾇ = with-accent-breathing-iota circumflex rough
ᾈ : Token α′ upper
ᾈ = with-breathing-iota smooth
ᾉ : Token α′ upper
ᾉ = with-breathing-iota rough
ᾊ : Token α′ upper
ᾊ = with-accent-breathing-iota grave smooth
ᾋ : Token α′ upper
ᾋ = with-accent-breathing-iota grave rough
ᾌ : Token α′ upper
ᾌ = with-accent-breathing-iota acute smooth
ᾍ : Token α′ upper
ᾍ = with-accent-breathing-iota acute rough
ᾎ : Token α′ upper
ᾎ = with-accent-breathing-iota circumflex smooth
ᾏ : Token α′ upper
ᾏ = with-accent-breathing-iota circumflex rough

-- U+1FBx
-- ᾰ
-- ᾱ
ᾲ : Token α′ lower
ᾲ = with-accent-iota grave
ᾳ : Token α′ lower
ᾳ = with-iota
ᾴ : Token α′ lower
ᾴ = with-accent-iota acute
ᾶ : Token α′ lower
ᾶ = with-accent circumflex
ᾷ : Token α′ lower
ᾷ = with-accent-iota circumflex
-- Ᾰ
-- Ᾱ
Ὰ : Token α′ upper
Ὰ = with-accent grave
Ά : Token α′ upper
Ά = with-accent acute
ᾼ : Token α′ upper
ᾼ = with-iota

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
