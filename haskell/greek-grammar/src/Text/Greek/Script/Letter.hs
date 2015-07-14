{-# LANGUAGE DeriveDataTypeable #-}

module Text.Greek.Script.Letter where

import Data.Data

data UnicodeLetter
  = U_α | U_β | U_γ | U_δ | U_ε | U_ζ | U_η | U_θ | U_ι | U_κ | U_λ | U_μ
  | U_ν | U_ξ | U_ο | U_π | U_ρ | U_σ | U_ς | U_τ | U_υ | U_φ | U_χ | U_ψ | U_ω
  | U_Α | U_Β | U_Γ | U_Δ | U_Ε | U_Ζ | U_Η | U_Θ | U_Ι | U_Κ | U_Λ | U_Μ
  | U_Ν | U_Ξ | U_Ο | U_Π | U_Ρ | U_Σ | U_Τ | U_Υ | U_Φ | U_Χ | U_Ψ | U_Ω

data FinalForm = FinalForm
data Letter
  = L_α | L_β | L_γ | L_δ | L_ε | L_ζ | L_η | L_θ | L_ι | L_κ | L_λ | L_μ
  | L_ν | L_ξ | L_ο | L_π | L_ρ | L_σ (Maybe FinalForm) | L_τ | L_υ | L_φ | L_χ | L_ψ | L_ω
data LetterCase = Lowercase | Uppercase

toUnicodeLetter :: UnicodeLetter -> (Letter, LetterCase)
toUnicodeLetter U_α = (L_α, Lowercase)
toUnicodeLetter U_β = (L_β, Lowercase)
toUnicodeLetter U_γ = (L_γ, Lowercase)
toUnicodeLetter U_δ = (L_δ, Lowercase)
toUnicodeLetter U_ε = (L_ε, Lowercase)
toUnicodeLetter U_ζ = (L_ζ, Lowercase)
toUnicodeLetter U_η = (L_η, Lowercase)
toUnicodeLetter U_θ = (L_θ, Lowercase)
toUnicodeLetter U_ι = (L_ι, Lowercase)
toUnicodeLetter U_κ = (L_κ, Lowercase)
toUnicodeLetter U_λ = (L_λ, Lowercase)
toUnicodeLetter U_μ = (L_μ, Lowercase)
toUnicodeLetter U_ν = (L_ν, Lowercase)
toUnicodeLetter U_ξ = (L_ξ, Lowercase)
toUnicodeLetter U_ο = (L_ο, Lowercase)
toUnicodeLetter U_π = (L_π, Lowercase)
toUnicodeLetter U_ρ = (L_ρ, Lowercase)
toUnicodeLetter U_σ = (L_σ Nothing, Lowercase)
toUnicodeLetter U_ς = (L_σ (Just FinalForm), Lowercase)
toUnicodeLetter U_τ = (L_τ, Lowercase)
toUnicodeLetter U_υ = (L_υ, Lowercase)
toUnicodeLetter U_φ = (L_φ, Lowercase)
toUnicodeLetter U_χ = (L_χ, Lowercase)
toUnicodeLetter U_ψ = (L_ψ, Lowercase)
toUnicodeLetter U_ω = (L_ω, Lowercase)

toUnicodeLetter U_Α = (L_α, Uppercase)
toUnicodeLetter U_Β = (L_β, Uppercase)
toUnicodeLetter U_Γ = (L_γ, Uppercase)
toUnicodeLetter U_Δ = (L_δ, Uppercase)
toUnicodeLetter U_Ε = (L_ε, Uppercase)
toUnicodeLetter U_Ζ = (L_ζ, Uppercase)
toUnicodeLetter U_Η = (L_η, Uppercase)
toUnicodeLetter U_Θ = (L_θ, Uppercase)
toUnicodeLetter U_Ι = (L_ι, Uppercase)
toUnicodeLetter U_Κ = (L_κ, Uppercase)
toUnicodeLetter U_Λ = (L_λ, Uppercase)
toUnicodeLetter U_Μ = (L_μ, Uppercase)
toUnicodeLetter U_Ν = (L_ν, Uppercase)
toUnicodeLetter U_Ξ = (L_ξ, Uppercase)
toUnicodeLetter U_Ο = (L_ο, Uppercase)
toUnicodeLetter U_Π = (L_π, Uppercase)
toUnicodeLetter U_Ρ = (L_ρ, Uppercase)
toUnicodeLetter U_Σ = (L_σ Nothing, Uppercase)
toUnicodeLetter U_Τ = (L_τ, Uppercase)
toUnicodeLetter U_Υ = (L_υ, Uppercase)
toUnicodeLetter U_Φ = (L_φ, Uppercase)
toUnicodeLetter U_Χ = (L_χ, Uppercase)
toUnicodeLetter U_Ψ = (L_ψ, Uppercase)
toUnicodeLetter U_Ω = (L_ω, Uppercase)

data Vowel = V_α | V_ε | V_η | V_ι | V_ο | V_υ | V_ω
data Consonant
  = C_β | C_γ | C_δ | C_ζ | C_θ | C_κ | C_λ | C_μ | C_ν
  | C_ξ | C_π | C_ρ | C_σ (Maybe FinalForm) | C_τ | C_φ | C_χ | C_ψ

toVowelConsonant :: Letter -> Either Vowel Consonant
toVowelConsonant L_α = Left V_α
toVowelConsonant L_ε = Left V_ε
toVowelConsonant L_η = Left V_η
toVowelConsonant L_ι = Left V_ι
toVowelConsonant L_ο = Left V_ο
toVowelConsonant L_υ = Left V_υ
toVowelConsonant L_ω = Left V_ω
toVowelConsonant L_β = Right C_β
toVowelConsonant L_γ = Right C_γ
toVowelConsonant L_δ = Right C_δ
toVowelConsonant L_ζ = Right C_ζ
toVowelConsonant L_θ = Right C_θ
toVowelConsonant L_κ = Right C_κ
toVowelConsonant L_λ = Right C_λ
toVowelConsonant L_μ = Right C_μ
toVowelConsonant L_ν = Right C_ν
toVowelConsonant L_ξ = Right C_ξ
toVowelConsonant L_π = Right C_π
toVowelConsonant L_ρ = Right C_ρ
toVowelConsonant (L_σ ff) = Right (C_σ ff)
toVowelConsonant L_τ = Right C_τ
toVowelConsonant L_φ = Right C_φ
toVowelConsonant L_χ = Right C_χ
toVowelConsonant L_ψ = Right C_ψ


data UnicodeMark = AcuteMark | GraveMark | CircumflexMark | SmoothMark | RoughMark | IotaSubscriptMark | DiaeresisMark

data Accent = AcuteAccent | GraveAccent | CircumflexAccent
data Breathing = SmoothBreathing | RoughBreathing
data IotaSubscript = IotaSubscript
data Diaeresis = Diaeresis

data OneOf4 a b c d
  = OneOf4_0 a
  | OneOf4_1 b
  | OneOf4_2 c
  | OneOf4_3 d

toMark :: UnicodeMark -> OneOf4 Accent Breathing IotaSubscript Diaeresis
toMark AcuteMark = OneOf4_0 AcuteAccent
toMark GraveMark = OneOf4_0 GraveAccent
toMark CircumflexMark = OneOf4_0 CircumflexAccent
toMark SmoothMark = OneOf4_1 SmoothBreathing
toMark RoughMark = OneOf4_1 RoughBreathing
toMark IotaSubscriptMark = OneOf4_2 IotaSubscript
toMark DiaeresisMark = OneOf4_3 Diaeresis

{-

data MarkedLetter = MarkedLetter
  { letter :: Letter
  , letterCase :: LetterCase
  , accent :: Maybe Accent
  , breathing :: Maybe Breathing
  , iotaSubscript :: Maybe IotaSubscript
  , diaeresis :: Maybe Diaeresis
  , finalForm :: Maybe FinalForm
  }


data RoughBreathing = RoughBreathing
  deriving (Eq, Show, Ord, Data, Typeable)
data Vowel = V_α | V_ε | V_η | V_ι | V_ο | V_υ | V_ω
  deriving (Eq, Show, Ord, Data, Typeable)
data Diphthong = D_αι | D_ει | D_οι | D_αυ | D_ευ | D_ου | D_ηυ | D_ωυ | D_υι
  deriving (Eq, Show, Ord, Data, Typeable)
data ImproperDiphthong = I_ᾳ | I_ῃ | I_ῳ
  deriving (Eq, Show, Ord, Data, Typeable)

data SingleVocalicSound
  = VowelSound Vowel
  | ImproperDiphthongSound ImproperDiphthong

data VocalicSoundUnit
  = SingleVocalicSound 
  | DiphthongSound Diphthong

data ConsonantalSound
  = ConsonantSound Consonant
  | RoughBreathingSound RoughBreathing

data SoundUnit
  = ConsonantalSoundUnit ConsonantalSound
  | VocalicSoundUnit VocalicSound (Maybe Accent)

makeConsonant :: Letter -> Maybe Consonant
makeConsonant L_β = Just C_β
makeConsonant L_γ = Just C_γ
makeConsonant L_δ = Just C_δ
makeConsonant L_ζ = Just C_ζ
makeConsonant L_θ = Just C_θ
makeConsonant L_κ = Just C_κ
makeConsonant L_λ = Just C_λ
makeConsonant L_μ = Just C_μ
makeConsonant L_ν = Just C_ν
makeConsonant L_ξ = Just C_ξ
makeConsonant L_π = Just C_π
makeConsonant L_ρ = Just C_ρ
makeConsonant L_σ = Just C_σ
makeConsonant L_τ = Just C_τ
makeConsonant L_φ = Just C_φ
makeConsonant L_χ = Just C_χ
makeConsonant L_ψ = Just C_ψ
makeConsonant _ = Nothing

makeVowel :: Letter -> Maybe Vowel
makeVowel L_α = Just V_α
makeVowel L_ε = Just V_ε
makeVowel L_η = Just V_η
makeVowel L_ι = Just V_ι
makeVowel L_ο = Just V_ο
makeVowel L_υ = Just V_υ
makeVowel L_ω = Just V_ω
makeVowel _ = Nothing

makeDiphthong :: Vowel -> Vowel -> Maybe Diphthong
makeDiphthong V_α V_ι = Just D_αι
makeDiphthong V_ε V_ι = Just D_ει
makeDiphthong V_ο V_ι = Just D_οι
makeDiphthong V_α V_υ = Just D_αυ
makeDiphthong V_ε V_υ = Just D_ευ
makeDiphthong V_ο V_υ = Just D_ου
makeDiphthong V_η V_υ = Just D_ηυ
makeDiphthong V_ω V_υ = Just D_ωυ
makeDiphthong V_υ V_ι = Just D_υι
makeDiphthong _ _ = Nothing

makeImproperDiphthong :: Vowel -> Subscript_ι -> Maybe ImproperDiphthong
makeImproperDiphthong V_α Subscript_ι = Just I_ᾳ
makeImproperDiphthong V_η Subscript_ι = Just I_ῃ
makeImproperDiphthong V_ω Subscript_ι = Just I_ῳ
makeImproperDiphthong _ _ = Nothing

lettersToSounds :: [MarkedLetter] -> [SoundUnit]
lettersToSounds [] = []
lettersToSounds (ml1 : ml2 : mls)
  | Just v1 <- makeVowel (letter ml1)
  , Just v2 <- makeVowel (letter ml2)
  , Nothing <- diaeresis ml2
  , Just d <- makeDiphthong v1 v2
  , a <- accent ml2
  = VocalicSoundUnit (DiphthongSound d) a : lettersToSounds mls

lettersToSounds (ml : mls)
  | Just c <- makeConsonant (letter ml)
  = ConsonantSoundUnit c : lettersToSounds mls

  | Just v <- makeVowel (letter ml)
  , Just s <- subscript_ι ml
  , Just i <- makeImproperDiphthong v s
  , a <- accent ml
  = VocalicSoundUnit (ImproperDiphthongSound i) a : lettersToSounds mls

  | Just v <- makeVowel (letter ml)
  , a <- accent ml
  = VocalicSoundUnit (VowelSound v) a : lettersToSounds mls

  | True
  = lettersToSounds mls
-}
