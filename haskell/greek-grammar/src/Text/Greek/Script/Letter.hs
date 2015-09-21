{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Greek.Script.Letter where

import Control.Lens
import Text.Greek.FileReference
import Text.Greek.Parse.Utility
import Text.Greek.Script.Unicode
import Text.Greek.Script.Unit
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim
import Text.Parsec.Combinator

data Letter
  = L_α | L_β | L_γ | L_δ | L_ε | L_ζ | L_η | L_θ | L_ι | L_κ | L_λ | L_μ
  | L_ν | L_ξ | L_ο | L_π | L_ρ | L_σ | L_τ | L_υ | L_φ | L_χ | L_ψ | L_ω
  deriving (Eq, Ord, Show)
data LetterCase = Lowercase | Uppercase deriving (Eq, Ord, Show)
data IsFinal = Final | NotFinal deriving (Eq, Ord, Show)
data CanBeFinal = CanBeFinal | CannotBeFinal deriving (Eq, Ord, Show)

data LetterInfo = LetterInfo
  { _letterInfoLetter :: Letter
  , _letterInfoCase :: LetterCase
  } deriving (Eq, Ord, Show)
makeLenses ''LetterInfo

data LetterInfoFinal
  = LetterInfoFinal LetterInfo
  | LetterInfoFinalSigma
  deriving (Eq, Ord, Show)

upper :: Letter -> LetterInfoFinal
upper l = LetterInfoFinal (LetterInfo l Uppercase)

lower :: Letter -> LetterInfoFinal
lower l = LetterInfoFinal (LetterInfo l Lowercase)

toLetterInfoFinal :: UnicodeLetter -> LetterInfoFinal
toLetterInfoFinal U_Α = upper L_α
toLetterInfoFinal U_Β = upper L_β
toLetterInfoFinal U_Γ = upper L_γ
toLetterInfoFinal U_Δ = upper L_δ
toLetterInfoFinal U_Ε = upper L_ε
toLetterInfoFinal U_Ζ = upper L_ζ
toLetterInfoFinal U_Η = upper L_η
toLetterInfoFinal U_Θ = upper L_θ
toLetterInfoFinal U_Ι = upper L_ι
toLetterInfoFinal U_Κ = upper L_κ
toLetterInfoFinal U_Λ = upper L_λ
toLetterInfoFinal U_Μ = upper L_μ
toLetterInfoFinal U_Ν = upper L_ν
toLetterInfoFinal U_Ξ = upper L_ξ
toLetterInfoFinal U_Ο = upper L_ο
toLetterInfoFinal U_Π = upper L_π
toLetterInfoFinal U_Ρ = upper L_ρ
toLetterInfoFinal U_Σ = upper L_σ
toLetterInfoFinal U_Τ = upper L_τ
toLetterInfoFinal U_Υ = upper L_υ
toLetterInfoFinal U_Φ = upper L_φ
toLetterInfoFinal U_Χ = upper L_χ
toLetterInfoFinal U_Ψ = upper L_ψ
toLetterInfoFinal U_Ω = upper L_ω
toLetterInfoFinal U_α = lower L_α
toLetterInfoFinal U_β = lower L_β
toLetterInfoFinal U_γ = lower L_γ
toLetterInfoFinal U_δ = lower L_δ
toLetterInfoFinal U_ε = lower L_ε
toLetterInfoFinal U_ζ = lower L_ζ
toLetterInfoFinal U_η = lower L_η
toLetterInfoFinal U_θ = lower L_θ
toLetterInfoFinal U_ι = lower L_ι
toLetterInfoFinal U_κ = lower L_κ
toLetterInfoFinal U_λ = lower L_λ
toLetterInfoFinal U_μ = lower L_μ
toLetterInfoFinal U_ν = lower L_ν
toLetterInfoFinal U_ξ = lower L_ξ
toLetterInfoFinal U_ο = lower L_ο
toLetterInfoFinal U_π = lower L_π
toLetterInfoFinal U_ρ = lower L_ρ
toLetterInfoFinal U_σ = lower L_σ
toLetterInfoFinal U_ς = LetterInfoFinalSigma
toLetterInfoFinal U_τ = lower L_τ
toLetterInfoFinal U_υ = lower L_υ
toLetterInfoFinal U_φ = lower L_φ
toLetterInfoFinal U_χ = lower L_χ
toLetterInfoFinal U_ψ = lower L_ψ
toLetterInfoFinal U_ω = lower L_ω

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
letterInfoToLetterChar = letterToLetterChar . view letterInfoLetter

forgetFinal :: LetterInfoFinal -> LetterInfo
forgetFinal (LetterInfoFinal i) = i
forgetFinal LetterInfoFinalSigma = LetterInfo L_σ Lowercase

letterInfoFinalToLetterChar :: LetterInfoFinal -> LetterChar
letterInfoFinalToLetterChar (LetterInfoFinal i) = letterToLetterChar $ view letterInfoLetter i
letterInfoFinalToLetterChar LetterInfoFinalSigma = LetterChar 'ς'

letterInfoFinalToLetterCase :: LetterInfoFinal -> LetterCase
letterInfoFinalToLetterCase = view letterInfoCase . forgetFinal

isFinal :: LetterInfoFinal -> IsFinal
isFinal LetterInfoFinalSigma = Final
isFinal _ = NotFinal

letterCanBeFinal :: Letter -> CanBeFinal
letterCanBeFinal L_σ = CanBeFinal
letterCanBeFinal _ = CannotBeFinal

letterInfoFinalCanBeFinal :: LetterInfoFinal -> CanBeFinal
letterInfoFinalCanBeFinal = letterCanBeFinal . view letterInfoLetter . forgetFinal


parseFinalPrim :: Show s => (s -> LineReference) -> (s -> LetterInfoFinal) -> (LetterInfoFinal -> Bool) -> Parser [s] s
parseFinalPrim f g h = primBool f (h . g)

nonFinalLetterParser :: Show s => (s -> LineReference) -> (s -> LetterInfoFinal) -> Parser [s] s
nonFinalLetterParser f g = parseFinalPrim f g check
  where
    check i | NotFinal <- isFinal i = True
    check _ = False

finalLetterParser :: Show s => (s -> LineReference) -> (s -> LetterInfoFinal) -> Parser [s] s
finalLetterParser f g = parseFinalPrim f g check
  where
    check i | CannotBeFinal <- letterInfoFinalCanBeFinal i, NotFinal <- isFinal i = True
    check i | CanBeFinal    <- letterInfoFinalCanBeFinal i, Final    <- isFinal i = True
    check _ = False

finalParser :: Show s => (s -> LineReference) -> (s -> LetterInfoFinal) -> Parser [s] [s]
finalParser f g = tryManyEnd (nonFinalLetterParser f g) (finalLetterParser f g <* eof)

parseFinals :: Show s => (s -> LineReference) -> (s -> LetterInfoFinal) -> [s] -> Either ParseError [s]
parseFinals f g = parse (finalParser f g) ""

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
