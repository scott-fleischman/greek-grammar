module Text.Greek.Smyth where

open import Data.Char
open import Data.Maybe
open import Relation.Nullary using (¬_)

-- Smyth §1
data Letter : Set where
  α β γ δ ε ζ η θ ι κ λ′ μ ν ξ ο π ρ σ τ υ φ χ ψ ω : Letter

data LetterCase : Set where
  capital small : LetterCase

form : Letter → LetterCase → Char
form α capital = 'Α'
form α small = 'α'
form β capital = 'Β'
form β small = 'β'
form γ capital = 'Γ'
form γ small = 'γ'
form δ capital = 'Δ'
form δ small = 'δ'
form ε capital = 'Ε'
form ε small = 'ε'
form ζ capital = 'Ζ'
form ζ small = 'ζ'
form η capital = 'Η'
form η small = 'η'
form θ capital = 'Θ'
form θ small = 'θ'
form ι capital = 'Ι'
form ι small = 'ι'
form κ capital = 'Κ'
form κ small = 'κ'
form λ′ capital = 'Λ'
form λ′ small = 'λ'
form μ capital = 'Μ'
form μ small = 'μ'
form ν capital = 'Ν'
form ν small = 'ν'
form ξ capital = 'Ξ'
form ξ small = 'ξ'
form ο capital = 'Ο'
form ο small = 'ο'
form π capital = 'Π'
form π small = 'π'
form ρ capital = 'Ρ'
form ρ small = 'ρ'
form σ capital = 'Σ'
form σ small = 'σ'
form τ capital = 'Τ'
form τ small = 'τ'
form υ capital = 'Υ'
form υ small = 'υ'
form φ capital = 'Φ'
form φ small = 'φ'
form χ capital = 'Χ'
form χ small = 'χ'
form ψ capital = 'Ψ'
form ψ small = 'ψ'
form ω capital = 'Ω'
form ω small = 'ω'

-- Smyth §1 a
data PositionInWord : Set where
  start-of-word : PositionInWord
  middle-of-word : PositionInWord
  end-of-word : PositionInWord

data FinalForm : Set where
  final-form not-final-form : FinalForm

get-final-form : Letter → LetterCase → PositionInWord → FinalForm
get-final-form σ′ small end-of-word = final-form
get-final-form _ _ _ = not-final-form

-- Smyth §3
data OlderLetter : Set where
  Ϝ′ : OlderLetter
  Ϙ′ : OlderLetter
  ϡ′ : OlderLetter

-- Smyth §4
data Vowel : Letter → Set where
  α : Vowel α
  ε : Vowel ε
  η : Vowel η
  ι : Vowel ι
  ο : Vowel ο
  υ : Vowel υ
  ω : Vowel ω

data AlwaysShort : Letter → Set where
  ε : AlwaysShort ε
  ο : AlwaysShort ο

data AlwaysLong : Letter → Set where
  η : AlwaysLong η
  ω : AlwaysLong ω

data VowelLength : Set where
  short long : VowelLength

data VowelWithLength : ∀ {v} → Vowel v → VowelLength → Set where
  always-short : ∀ {ℓ} → (v : Vowel ℓ) → AlwaysShort ℓ → VowelWithLength v short
  always-long : ∀ {ℓ} → (v : Vowel ℓ) → AlwaysLong ℓ → VowelWithLength v long
  α-with-length : (vl : VowelLength) → VowelWithLength α vl
  ι-with-length : (vl : VowelLength) → VowelWithLength ι vl
  υ-with-length : (vl : VowelLength) → VowelWithLength υ vl

alwaysShort-Vowel : ∀ {ℓ} → AlwaysShort ℓ → Vowel ℓ
alwaysShort-Vowel ε = ε
alwaysShort-Vowel ο = ο

alwaysLong-Vowel : ∀ {ℓ} → AlwaysLong ℓ → Vowel ℓ
alwaysLong-Vowel η = η
alwaysLong-Vowel ω = ω

-- Smyth §5
data Diphthong : Letter → Letter → Set where
  αι : Diphthong α ι
  ει : Diphthong ε ι
  οι : Diphthong ο ι
  ᾱͅ : Diphthong α ι
  ῃ : Diphthong η ι
  ῳ : Diphthong ω ι
  αυ : Diphthong α υ
  ευ : Diphthong ε υ
  ου : Diphthong ο υ
  ηυ : Diphthong η υ
  υι : Diphthong υ ι

-- Smyth §6
data SpuriousDiphthong : ∀ {v₁ v₂} → Diphthong v₁ v₂ → Set where
  ει-sp : SpuriousDiphthong ει
  ου-sp : SpuriousDiphthong ου

-- Smyth §7
data TonguePosition : Set where
  closed closed-medium open-medium open′ closing opening : TonguePosition

tongue-position-vowel : ∀ {ℓ} → Vowel ℓ → TonguePosition
tongue-position-vowel α = open′
tongue-position-vowel ε = closed-medium
tongue-position-vowel η = open-medium
tongue-position-vowel ι = closed
tongue-position-vowel ο = closed-medium
tongue-position-vowel υ = closed
tongue-position-vowel ω = open-medium

tongue-position-diphthong : ∀ {ℓ₁ ℓ₂} → Diphthong ℓ₁ ℓ₂ → TonguePosition
tongue-position-diphthong d = closing

data RoundedLips : Set where
  rounded-lips not-rounded-lips : RoundedLips

rounded-lips-vowel : ∀ {ℓ} → Vowel ℓ → RoundedLips
rounded-lips-vowel ο = rounded-lips
rounded-lips-vowel υ = rounded-lips
rounded-lips-vowel ω = rounded-lips
rounded-lips-vowel _ = not-rounded-lips

rounded-lips-diphthong : ∀ {ℓ₁ ℓ₂} → Diphthong ℓ₁ ℓ₂ → RoundedLips
rounded-lips-diphthong ου = rounded-lips
rounded-lips-diphthong _ = not-rounded-lips

-- Smyth §9
data Breathing : Set where
  ῾ ᾿ : Breathing

-- Smyth §15
data Consonant : Letter → Set where
  β : Consonant β
  γ : Consonant γ
  δ : Consonant δ
  ζ : Consonant ζ
  θ : Consonant θ
  κ : Consonant κ
  λ′ : Consonant λ′
  μ : Consonant μ
  ν : Consonant ν
  ξ : Consonant ξ
  π : Consonant π
  ρ : Consonant ρ
  σ : Consonant σ
  τ : Consonant τ
  φ : Consonant φ
  χ : Consonant χ
  ψ : Consonant ψ

data LetterCategory : Letter → Set where
  vowel : ∀ {ℓ} → Vowel ℓ → LetterCategory ℓ
  consonant : ∀ {ℓ} → Consonant ℓ → LetterCategory ℓ

Letter-to-LetterCategory : (ℓ : Letter) → LetterCategory ℓ
Letter-to-LetterCategory α = vowel α
Letter-to-LetterCategory β = consonant β
Letter-to-LetterCategory γ = consonant γ
Letter-to-LetterCategory δ = consonant δ
Letter-to-LetterCategory ε = vowel ε
Letter-to-LetterCategory ζ = consonant ζ
Letter-to-LetterCategory η = vowel η
Letter-to-LetterCategory θ = consonant θ
Letter-to-LetterCategory ι = vowel ι
Letter-to-LetterCategory κ = consonant κ
Letter-to-LetterCategory λ′ = consonant λ′
Letter-to-LetterCategory μ = consonant μ
Letter-to-LetterCategory ν = consonant ν
Letter-to-LetterCategory ξ = consonant ξ
Letter-to-LetterCategory ο = vowel ο
Letter-to-LetterCategory π = consonant π
Letter-to-LetterCategory ρ = consonant ρ
Letter-to-LetterCategory σ = consonant σ
Letter-to-LetterCategory τ = consonant τ
Letter-to-LetterCategory υ = vowel υ
Letter-to-LetterCategory φ = consonant φ
Letter-to-LetterCategory χ = consonant χ
Letter-to-LetterCategory ψ = consonant ψ
Letter-to-LetterCategory ω = vowel ω

LetterCategory-to-Letter : ∀ {ℓ} → LetterCategory ℓ → Letter
LetterCategory-to-Letter {ℓ} _ = ℓ

data ConsonantSound : Letter → Set where
  β : ConsonantSound β
  γ : ConsonantSound γ
  δ : ConsonantSound δ
  ζ : ConsonantSound ζ
  θ : ConsonantSound θ
  κ : ConsonantSound κ
  λ′ : ConsonantSound λ′
  μ : ConsonantSound μ
  ν : ConsonantSound ν
  ξ : ConsonantSound ξ
  π : ConsonantSound π
  ρ : ConsonantSound ρ
  σ : ConsonantSound σ
  τ : ConsonantSound τ
  φ : ConsonantSound φ
  χ : ConsonantSound χ
  ψ : ConsonantSound ψ
  γ-nasal : ConsonantSound γ
  ῥ : ConsonantSound ρ

data VocalChords : Set where
  voiced voiceless : VocalChords

-- Smyth §15 a
data Voiced : ∀ {ℓ} → ConsonantSound ℓ → Set where
  β : Voiced β
  δ : Voiced δ
  γ : Voiced γ
  λ′ : Voiced λ′
  ρ : Voiced ρ
  μ : Voiced μ
  ν : Voiced ν
  γ-nasal : Voiced γ-nasal
  ζ : Voiced ζ

-- Smyth §15 a, b
data Voiceless : ∀ {ℓ} → ConsonantSound ℓ → Set where
  ῥ : Voiceless ῥ
  π : Voiceless π
  τ : Voiceless τ
  κ : Voiceless κ
  φ : Voiceless φ
  θ : Voiceless θ
  χ : Voiceless χ
  σ : Voiceless σ
  ψ : Voiceless ψ
  ξ : Voiceless ξ



-- TODO (needs a unified model)

-- Accent
-- Smyth §4 All vowels with the circumflex (149) are long.

-- Diaeresis
-- Smyth §8 Diaeresis.—A double dot, the mark of diaeresis, may be written over ι or υ when these do not form a diphthong with the preceding vowel

-- Breathing
-- Smyth §9. Every initial vowel or diphthong has either the rough (῾) or the smooth (᾿) breathing.
-- Smyth §10. Initial υ (ῠ and ῡ) always has the rough breathing.
-- Smyth §11. Diphthongs take the breathing, as the accent (152), over the second vowel: αἱρέω hairéo I seize, αἴρω aíro I lift.
-- Smyth §11. But ᾳ, ῃ, ῳ take both the breathing and the accent on the first vowel, even when ι is written in the line (5): ᾄδω = Ἄιδω I sing… The writing ἀίδηλος (Ἀίδηλος) destroying shows that αι does not here form a diphthong
-- Smyth §12. In compound words (as in προορᾶν to foresee, from πρό + ὁρᾶν) the rough breathing is not written, though it must often have been pronounced
-- Smyth §13. Every initial ρ has the rough breathing
-- Smyth §13. Medial ρρ is written ῤῥ in some texts

-- Consonants
-- Smyth §15 c. ι̯ υ̯

