module Text.Greek.Script.Letter where

import Text.Greek.Script.Unicode

data Letter
  = L_α | L_β | L_γ | L_δ | L_ε | L_ζ | L_η | L_θ | L_ι | L_κ | L_λ | L_μ
  | L_ν | L_ξ | L_ο | L_π | L_ρ | L_σ | L_τ | L_υ | L_φ | L_χ | L_ψ | L_ω
  deriving (Eq, Ord, Show)
data LetterCase = Lowercase | Uppercase deriving (Eq, Ord, Show)
data Final = Final | NotFinal deriving (Eq, Ord, Show)

data LetterInfo
  = LetterInfoCase LetterCase Letter
  | LetterInfoFinalSigma
  deriving (Eq, Ord, Show)

toLetterInfo :: UnicodeLetter -> LetterInfo
toLetterInfo U_Α = LetterInfoCase Uppercase L_α
toLetterInfo U_Β = LetterInfoCase Uppercase L_β
toLetterInfo U_Γ = LetterInfoCase Uppercase L_γ
toLetterInfo U_Δ = LetterInfoCase Uppercase L_δ
toLetterInfo U_Ε = LetterInfoCase Uppercase L_ε
toLetterInfo U_Ζ = LetterInfoCase Uppercase L_ζ
toLetterInfo U_Η = LetterInfoCase Uppercase L_η
toLetterInfo U_Θ = LetterInfoCase Uppercase L_θ
toLetterInfo U_Ι = LetterInfoCase Uppercase L_ι
toLetterInfo U_Κ = LetterInfoCase Uppercase L_κ
toLetterInfo U_Λ = LetterInfoCase Uppercase L_λ
toLetterInfo U_Μ = LetterInfoCase Uppercase L_μ
toLetterInfo U_Ν = LetterInfoCase Uppercase L_ν
toLetterInfo U_Ξ = LetterInfoCase Uppercase L_ξ
toLetterInfo U_Ο = LetterInfoCase Uppercase L_ο
toLetterInfo U_Π = LetterInfoCase Uppercase L_π
toLetterInfo U_Ρ = LetterInfoCase Uppercase L_ρ
toLetterInfo U_Σ = LetterInfoCase Uppercase L_σ
toLetterInfo U_Τ = LetterInfoCase Uppercase L_τ
toLetterInfo U_Υ = LetterInfoCase Uppercase L_υ
toLetterInfo U_Φ = LetterInfoCase Uppercase L_φ
toLetterInfo U_Χ = LetterInfoCase Uppercase L_χ
toLetterInfo U_Ψ = LetterInfoCase Uppercase L_ψ
toLetterInfo U_Ω = LetterInfoCase Uppercase L_ω
toLetterInfo U_α = LetterInfoCase Lowercase L_α
toLetterInfo U_β = LetterInfoCase Lowercase L_β
toLetterInfo U_γ = LetterInfoCase Lowercase L_γ
toLetterInfo U_δ = LetterInfoCase Lowercase L_δ
toLetterInfo U_ε = LetterInfoCase Lowercase L_ε
toLetterInfo U_ζ = LetterInfoCase Lowercase L_ζ
toLetterInfo U_η = LetterInfoCase Lowercase L_η
toLetterInfo U_θ = LetterInfoCase Lowercase L_θ
toLetterInfo U_ι = LetterInfoCase Lowercase L_ι
toLetterInfo U_κ = LetterInfoCase Lowercase L_κ
toLetterInfo U_λ = LetterInfoCase Lowercase L_λ
toLetterInfo U_μ = LetterInfoCase Lowercase L_μ
toLetterInfo U_ν = LetterInfoCase Lowercase L_ν
toLetterInfo U_ξ = LetterInfoCase Lowercase L_ξ
toLetterInfo U_ο = LetterInfoCase Lowercase L_ο
toLetterInfo U_π = LetterInfoCase Lowercase L_π
toLetterInfo U_ρ = LetterInfoCase Lowercase L_ρ
toLetterInfo U_σ = LetterInfoCase Lowercase L_σ
toLetterInfo U_ς = LetterInfoFinalSigma
toLetterInfo U_τ = LetterInfoCase Lowercase L_τ
toLetterInfo U_υ = LetterInfoCase Lowercase L_υ
toLetterInfo U_φ = LetterInfoCase Lowercase L_φ
toLetterInfo U_χ = LetterInfoCase Lowercase L_χ
toLetterInfo U_ψ = LetterInfoCase Lowercase L_ψ
toLetterInfo U_ω = LetterInfoCase Lowercase L_ω

data Vowel = V_α | V_ε | V_η | V_ι | V_ο | V_υ | V_ω deriving (Eq, Show, Ord)
data Consonant = C_β | C_γ | C_δ | C_ζ | C_θ | C_κ | C_λ | C_μ | C_ν | C_ξ | C_π | C_ρ | C_σ | C_τ | C_φ | C_χ | C_ψ deriving (Eq, Show, Ord)

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
toVowelConsonant L_σ = Right C_σ
toVowelConsonant L_τ = Right C_τ
toVowelConsonant L_φ = Right C_φ
toVowelConsonant L_χ = Right C_χ
toVowelConsonant L_ψ = Right C_ψ



data Accent = AcuteAccent | GraveAccent | CircumflexAccent
data Breathing = SmoothBreathing | RoughBreathing
data IotaSubscript = IotaSubscript
data Diaeresis = Diaeresis

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
