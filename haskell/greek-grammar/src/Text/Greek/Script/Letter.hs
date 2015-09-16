module Text.Greek.Script.Letter where

import qualified Text.Greek.Script.Unit as U

data UnicodeLetter
  = U_Α | U_Β | U_Γ | U_Δ | U_Ε | U_Ζ | U_Η | U_Θ | U_Ι | U_Κ | U_Λ | U_Μ | U_Ν | U_Ξ | U_Ο | U_Π | U_Ρ | U_Σ       | U_Τ | U_Υ | U_Φ | U_Χ | U_Ψ | U_Ω
  | U_α | U_β | U_γ | U_δ | U_ε | U_ζ | U_η | U_θ | U_ι | U_κ | U_λ | U_μ | U_ν | U_ξ | U_ο | U_π | U_ρ | U_σ | U_ς | U_τ | U_υ | U_φ | U_χ | U_ψ | U_ω
  deriving (Eq, Ord, Show)

toUnicodeLetter :: U.LetterChar -> Maybe UnicodeLetter
toUnicodeLetter (U.LetterChar 'Α') = Just U_Α
toUnicodeLetter (U.LetterChar 'Β') = Just U_Β
toUnicodeLetter (U.LetterChar 'Γ') = Just U_Γ
toUnicodeLetter (U.LetterChar 'Δ') = Just U_Δ
toUnicodeLetter (U.LetterChar 'Ε') = Just U_Ε
toUnicodeLetter (U.LetterChar 'Ζ') = Just U_Ζ
toUnicodeLetter (U.LetterChar 'Η') = Just U_Η
toUnicodeLetter (U.LetterChar 'Θ') = Just U_Θ
toUnicodeLetter (U.LetterChar 'Ι') = Just U_Ι
toUnicodeLetter (U.LetterChar 'Κ') = Just U_Κ
toUnicodeLetter (U.LetterChar 'Λ') = Just U_Λ
toUnicodeLetter (U.LetterChar 'Μ') = Just U_Μ
toUnicodeLetter (U.LetterChar 'Ν') = Just U_Ν
toUnicodeLetter (U.LetterChar 'Ξ') = Just U_Ξ
toUnicodeLetter (U.LetterChar 'Ο') = Just U_Ο
toUnicodeLetter (U.LetterChar 'Π') = Just U_Π
toUnicodeLetter (U.LetterChar 'Ρ') = Just U_Ρ
toUnicodeLetter (U.LetterChar 'Σ') = Just U_Σ
toUnicodeLetter (U.LetterChar 'Τ') = Just U_Τ
toUnicodeLetter (U.LetterChar 'Υ') = Just U_Υ
toUnicodeLetter (U.LetterChar 'Φ') = Just U_Φ
toUnicodeLetter (U.LetterChar 'Χ') = Just U_Χ
toUnicodeLetter (U.LetterChar 'Ψ') = Just U_Ψ
toUnicodeLetter (U.LetterChar 'Ω') = Just U_Ω
toUnicodeLetter (U.LetterChar 'α') = Just U_α
toUnicodeLetter (U.LetterChar 'β') = Just U_β
toUnicodeLetter (U.LetterChar 'γ') = Just U_γ
toUnicodeLetter (U.LetterChar 'δ') = Just U_δ
toUnicodeLetter (U.LetterChar 'ε') = Just U_ε
toUnicodeLetter (U.LetterChar 'ζ') = Just U_ζ
toUnicodeLetter (U.LetterChar 'η') = Just U_η
toUnicodeLetter (U.LetterChar 'θ') = Just U_θ
toUnicodeLetter (U.LetterChar 'ι') = Just U_ι
toUnicodeLetter (U.LetterChar 'κ') = Just U_κ
toUnicodeLetter (U.LetterChar 'λ') = Just U_λ
toUnicodeLetter (U.LetterChar 'μ') = Just U_μ
toUnicodeLetter (U.LetterChar 'ν') = Just U_ν
toUnicodeLetter (U.LetterChar 'ξ') = Just U_ξ
toUnicodeLetter (U.LetterChar 'ο') = Just U_ο
toUnicodeLetter (U.LetterChar 'π') = Just U_π
toUnicodeLetter (U.LetterChar 'ρ') = Just U_ρ
toUnicodeLetter (U.LetterChar 'ς') = Just U_ς
toUnicodeLetter (U.LetterChar 'σ') = Just U_σ
toUnicodeLetter (U.LetterChar 'τ') = Just U_τ
toUnicodeLetter (U.LetterChar 'υ') = Just U_υ
toUnicodeLetter (U.LetterChar 'φ') = Just U_φ
toUnicodeLetter (U.LetterChar 'χ') = Just U_χ
toUnicodeLetter (U.LetterChar 'ψ') = Just U_ψ
toUnicodeLetter (U.LetterChar 'ω') = Just U_ω
toUnicodeLetter _ = Nothing

data RightQuote = RightQuote deriving (Eq, Ord, Show) -- 

data Letter
  = L_α | L_β | L_γ | L_δ | L_ε | L_ζ | L_η | L_θ | L_ι | L_κ | L_λ | L_μ
  | L_ν | L_ξ | L_ο | L_π | L_ρ | L_σ | L_τ | L_υ | L_φ | L_χ | L_ψ | L_ω
data LetterCase = Lowercase | Uppercase deriving (Eq, Ord, Show)
data Final = Final | NotFinal deriving (Eq, Ord, Show)

data LetterInfo = LetterInfo
  { letterInfoLetter :: Letter
  , letterInfoCase :: LetterCase
  , letterInfoFinal :: Final
  }

upper :: Letter -> LetterInfo
upper l = LetterInfo l Uppercase NotFinal

lower :: Letter -> LetterInfo
lower l = LetterInfo l Lowercase NotFinal

toLetterInfo :: UnicodeLetter -> LetterInfo
toLetterInfo U_Α = upper L_α
toLetterInfo U_Β = upper L_β
toLetterInfo U_Γ = upper L_γ
toLetterInfo U_Δ = upper L_δ
toLetterInfo U_Ε = upper L_ε
toLetterInfo U_Ζ = upper L_ζ
toLetterInfo U_Η = upper L_η
toLetterInfo U_Θ = upper L_θ
toLetterInfo U_Ι = upper L_ι
toLetterInfo U_Κ = upper L_κ
toLetterInfo U_Λ = upper L_λ
toLetterInfo U_Μ = upper L_μ
toLetterInfo U_Ν = upper L_ν
toLetterInfo U_Ξ = upper L_ξ
toLetterInfo U_Ο = upper L_ο
toLetterInfo U_Π = upper L_π
toLetterInfo U_Ρ = upper L_ρ
toLetterInfo U_Σ = upper L_σ
toLetterInfo U_Τ = upper L_τ
toLetterInfo U_Υ = upper L_υ
toLetterInfo U_Φ = upper L_φ
toLetterInfo U_Χ = upper L_χ
toLetterInfo U_Ψ = upper L_ψ
toLetterInfo U_Ω = upper L_ω
toLetterInfo U_α = lower L_α
toLetterInfo U_β = lower L_β
toLetterInfo U_γ = lower L_γ
toLetterInfo U_δ = lower L_δ
toLetterInfo U_ε = lower L_ε
toLetterInfo U_ζ = lower L_ζ
toLetterInfo U_η = lower L_η
toLetterInfo U_θ = lower L_θ
toLetterInfo U_ι = lower L_ι
toLetterInfo U_κ = lower L_κ
toLetterInfo U_λ = lower L_λ
toLetterInfo U_μ = lower L_μ
toLetterInfo U_ν = lower L_ν
toLetterInfo U_ξ = lower L_ξ
toLetterInfo U_ο = lower L_ο
toLetterInfo U_π = lower L_π
toLetterInfo U_ρ = lower L_ρ
toLetterInfo U_σ = lower L_σ
toLetterInfo U_ς = LetterInfo L_σ Lowercase Final
toLetterInfo U_τ = lower L_τ
toLetterInfo U_υ = lower L_υ
toLetterInfo U_φ = lower L_φ
toLetterInfo U_χ = lower L_χ
toLetterInfo U_ψ = lower L_ψ
toLetterInfo U_ω = lower L_ω

data Vowel = V_α | V_ε | V_η | V_ι | V_ο | V_υ | V_ω
data Consonant = C_β | C_γ | C_δ | C_ζ | C_θ | C_κ | C_λ | C_μ | C_ν | C_ξ | C_π | C_ρ | C_σ | C_τ | C_φ | C_χ | C_ψ

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


data UnicodeMark = AcuteMark | GraveMark | CircumflexMark | SmoothMark | RoughMark | IotaSubscriptMark | DiaeresisMark

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
