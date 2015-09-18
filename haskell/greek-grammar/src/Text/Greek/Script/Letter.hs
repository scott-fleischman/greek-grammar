module Text.Greek.Script.Letter where

import Text.Greek.Script.Unicode
import Text.Greek.Script.Unit

data Letter
  = L_α | L_β | L_γ | L_δ | L_ε | L_ζ | L_η | L_θ | L_ι | L_κ | L_λ | L_μ
  | L_ν | L_ξ | L_ο | L_π | L_ρ | L_σ | L_τ | L_υ | L_φ | L_χ | L_ψ | L_ω
  deriving (Eq, Ord, Show)
data LetterCase = Lowercase | Uppercase deriving (Eq, Ord, Show)
data Final = Final | NotFinal deriving (Eq, Ord, Show)

data LetterInfo
  = LetterInfoCase Letter LetterCase
  | LetterInfoFinalSigma
  deriving (Eq, Ord, Show)

toLetterInfo :: UnicodeLetter -> LetterInfo
toLetterInfo U_Α = LetterInfoCase L_α Uppercase
toLetterInfo U_Β = LetterInfoCase L_β Uppercase
toLetterInfo U_Γ = LetterInfoCase L_γ Uppercase
toLetterInfo U_Δ = LetterInfoCase L_δ Uppercase
toLetterInfo U_Ε = LetterInfoCase L_ε Uppercase
toLetterInfo U_Ζ = LetterInfoCase L_ζ Uppercase
toLetterInfo U_Η = LetterInfoCase L_η Uppercase
toLetterInfo U_Θ = LetterInfoCase L_θ Uppercase
toLetterInfo U_Ι = LetterInfoCase L_ι Uppercase
toLetterInfo U_Κ = LetterInfoCase L_κ Uppercase
toLetterInfo U_Λ = LetterInfoCase L_λ Uppercase
toLetterInfo U_Μ = LetterInfoCase L_μ Uppercase
toLetterInfo U_Ν = LetterInfoCase L_ν Uppercase
toLetterInfo U_Ξ = LetterInfoCase L_ξ Uppercase
toLetterInfo U_Ο = LetterInfoCase L_ο Uppercase
toLetterInfo U_Π = LetterInfoCase L_π Uppercase
toLetterInfo U_Ρ = LetterInfoCase L_ρ Uppercase
toLetterInfo U_Σ = LetterInfoCase L_σ Uppercase
toLetterInfo U_Τ = LetterInfoCase L_τ Uppercase
toLetterInfo U_Υ = LetterInfoCase L_υ Uppercase
toLetterInfo U_Φ = LetterInfoCase L_φ Uppercase
toLetterInfo U_Χ = LetterInfoCase L_χ Uppercase
toLetterInfo U_Ψ = LetterInfoCase L_ψ Uppercase
toLetterInfo U_Ω = LetterInfoCase L_ω Uppercase
toLetterInfo U_α = LetterInfoCase L_α Lowercase
toLetterInfo U_β = LetterInfoCase L_β Lowercase
toLetterInfo U_γ = LetterInfoCase L_γ Lowercase
toLetterInfo U_δ = LetterInfoCase L_δ Lowercase
toLetterInfo U_ε = LetterInfoCase L_ε Lowercase
toLetterInfo U_ζ = LetterInfoCase L_ζ Lowercase
toLetterInfo U_η = LetterInfoCase L_η Lowercase
toLetterInfo U_θ = LetterInfoCase L_θ Lowercase
toLetterInfo U_ι = LetterInfoCase L_ι Lowercase
toLetterInfo U_κ = LetterInfoCase L_κ Lowercase
toLetterInfo U_λ = LetterInfoCase L_λ Lowercase
toLetterInfo U_μ = LetterInfoCase L_μ Lowercase
toLetterInfo U_ν = LetterInfoCase L_ν Lowercase
toLetterInfo U_ξ = LetterInfoCase L_ξ Lowercase
toLetterInfo U_ο = LetterInfoCase L_ο Lowercase
toLetterInfo U_π = LetterInfoCase L_π Lowercase
toLetterInfo U_ρ = LetterInfoCase L_ρ Lowercase
toLetterInfo U_σ = LetterInfoCase L_σ Lowercase
toLetterInfo U_ς = LetterInfoFinalSigma
toLetterInfo U_τ = LetterInfoCase L_τ Lowercase
toLetterInfo U_υ = LetterInfoCase L_υ Lowercase
toLetterInfo U_φ = LetterInfoCase L_φ Lowercase
toLetterInfo U_χ = LetterInfoCase L_χ Lowercase
toLetterInfo U_ψ = LetterInfoCase L_ψ Lowercase
toLetterInfo U_ω = LetterInfoCase L_ω Lowercase

letterToLetterChar :: Letter -> LetterChar
letterToLetterChar L_α = LetterChar 'α'
letterToLetterChar L_β = LetterChar 'β'
letterToLetterChar L_γ = LetterChar 'γ'
letterToLetterChar L_δ = LetterChar 'δ'
letterToLetterChar L_ε = LetterChar 'ε'
letterToLetterChar L_ζ = LetterChar 'ζ'
letterToLetterChar L_η = LetterChar 'η'
letterToLetterChar L_θ = LetterChar 'θ'
letterToLetterChar L_ι = LetterChar 'ι'
letterToLetterChar L_κ = LetterChar 'κ'
letterToLetterChar L_λ = LetterChar 'λ'
letterToLetterChar L_μ = LetterChar 'μ'
letterToLetterChar L_ν = LetterChar 'ν'
letterToLetterChar L_ξ = LetterChar 'ξ'
letterToLetterChar L_ο = LetterChar 'ο'
letterToLetterChar L_π = LetterChar 'π'
letterToLetterChar L_ρ = LetterChar 'ρ'
letterToLetterChar L_σ = LetterChar 'σ'
letterToLetterChar L_τ = LetterChar 'τ'
letterToLetterChar L_υ = LetterChar 'υ'
letterToLetterChar L_φ = LetterChar 'φ'
letterToLetterChar L_χ = LetterChar 'χ'
letterToLetterChar L_ψ = LetterChar 'ψ'
letterToLetterChar L_ω = LetterChar 'ω'

letterInfoToLetterChar :: LetterInfo -> LetterChar
letterInfoToLetterChar (LetterInfoCase l _) = letterToLetterChar l
letterInfoToLetterChar LetterInfoFinalSigma = LetterChar 'ς'

letterInfoToLetterCase :: LetterInfo -> LetterCase
letterInfoToLetterCase (LetterInfoCase _ c) = c
letterInfoToLetterCase LetterInfoFinalSigma = Lowercase


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
