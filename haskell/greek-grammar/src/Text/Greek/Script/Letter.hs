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
data Case = Lowercase | Uppercase deriving (Eq, Ord, Show)
data Position = NotLast | Last deriving (Eq, Ord, Show)

data Info = Info
  { _infoCase :: Case
  , _infoLetter :: Letter
  , _infoPosition :: Position
  } deriving (Eq, Ord, Show)
makeLenses ''Info

data InfoFinal
  = InfoNotFinal Case Letter
  | InfoFinalSigma
  deriving (Eq, Ord, Show)

upper :: Letter -> InfoFinal
upper = InfoNotFinal Uppercase

lower :: Letter -> InfoFinal
lower = InfoNotFinal Lowercase

toInfoFinal :: UnicodeLetter -> InfoFinal
toInfoFinal U_Α = upper L_α
toInfoFinal U_Β = upper L_β
toInfoFinal U_Γ = upper L_γ
toInfoFinal U_Δ = upper L_δ
toInfoFinal U_Ε = upper L_ε
toInfoFinal U_Ζ = upper L_ζ
toInfoFinal U_Η = upper L_η
toInfoFinal U_Θ = upper L_θ
toInfoFinal U_Ι = upper L_ι
toInfoFinal U_Κ = upper L_κ
toInfoFinal U_Λ = upper L_λ
toInfoFinal U_Μ = upper L_μ
toInfoFinal U_Ν = upper L_ν
toInfoFinal U_Ξ = upper L_ξ
toInfoFinal U_Ο = upper L_ο
toInfoFinal U_Π = upper L_π
toInfoFinal U_Ρ = upper L_ρ
toInfoFinal U_Σ = upper L_σ
toInfoFinal U_Τ = upper L_τ
toInfoFinal U_Υ = upper L_υ
toInfoFinal U_Φ = upper L_φ
toInfoFinal U_Χ = upper L_χ
toInfoFinal U_Ψ = upper L_ψ
toInfoFinal U_Ω = upper L_ω
toInfoFinal U_α = lower L_α
toInfoFinal U_β = lower L_β
toInfoFinal U_γ = lower L_γ
toInfoFinal U_δ = lower L_δ
toInfoFinal U_ε = lower L_ε
toInfoFinal U_ζ = lower L_ζ
toInfoFinal U_η = lower L_η
toInfoFinal U_θ = lower L_θ
toInfoFinal U_ι = lower L_ι
toInfoFinal U_κ = lower L_κ
toInfoFinal U_λ = lower L_λ
toInfoFinal U_μ = lower L_μ
toInfoFinal U_ν = lower L_ν
toInfoFinal U_ξ = lower L_ξ
toInfoFinal U_ο = lower L_ο
toInfoFinal U_π = lower L_π
toInfoFinal U_ρ = lower L_ρ
toInfoFinal U_σ = lower L_σ
toInfoFinal U_ς = InfoFinalSigma
toInfoFinal U_τ = lower L_τ
toInfoFinal U_υ = lower L_υ
toInfoFinal U_φ = lower L_φ
toInfoFinal U_χ = lower L_χ
toInfoFinal U_ψ = lower L_ψ
toInfoFinal U_ω = lower L_ω

toLetterChar :: Letter -> LetterChar
toLetterChar L_α = LetterChar 'α'
toLetterChar L_β = LetterChar 'β'
toLetterChar L_γ = LetterChar 'γ'
toLetterChar L_δ = LetterChar 'δ'
toLetterChar L_ε = LetterChar 'ε'
toLetterChar L_ζ = LetterChar 'ζ'
toLetterChar L_η = LetterChar 'η'
toLetterChar L_θ = LetterChar 'θ'
toLetterChar L_ι = LetterChar 'ι'
toLetterChar L_κ = LetterChar 'κ'
toLetterChar L_λ = LetterChar 'λ'
toLetterChar L_μ = LetterChar 'μ'
toLetterChar L_ν = LetterChar 'ν'
toLetterChar L_ξ = LetterChar 'ξ'
toLetterChar L_ο = LetterChar 'ο'
toLetterChar L_π = LetterChar 'π'
toLetterChar L_ρ = LetterChar 'ρ'
toLetterChar L_σ = LetterChar 'σ'
toLetterChar L_τ = LetterChar 'τ'
toLetterChar L_υ = LetterChar 'υ'
toLetterChar L_φ = LetterChar 'φ'
toLetterChar L_χ = LetterChar 'χ'
toLetterChar L_ψ = LetterChar 'ψ'
toLetterChar L_ω = LetterChar 'ω'

finalToPair :: InfoFinal -> (Case, LetterChar)
finalToPair (InfoNotFinal c l) = (c, toLetterChar l)
finalToPair InfoFinalSigma = (Lowercase, LetterChar 'ς')

parseFinalPrim :: Show s => (s -> LineReference) -> LensLike Maybe s t a b -> (a -> Maybe b) -> Parser [s] t
parseFinalPrim f g h = primMaybe f (g h)

nonFinalLetterParser :: Show s => (s -> LineReference) -> Lens s t InfoFinal Info -> Parser [s] t
nonFinalLetterParser f g = parseFinalPrim f g apply
  where
    apply (InfoNotFinal c l) = Just $ Info c l NotLast
    apply _ = Nothing

finalLetterParser :: Show s => (s -> LineReference) -> Lens s t InfoFinal Info -> Parser [s] t
finalLetterParser f g = parseFinalPrim f g apply
  where
    apply (InfoNotFinal _ l) | L_σ <- l = Nothing
    apply (InfoNotFinal c l) = Just $ Info c l Last
    apply InfoFinalSigma = Just $ Info Lowercase L_σ Last

finalParser :: Show s => (s -> LineReference) -> Lens s t InfoFinal Info -> Parser [s] [t]
finalParser f g = tryManyEnd (nonFinalLetterParser f g) (finalLetterParser f g <* eof)

parseFinals :: Show s => (s -> LineReference) -> Lens s t InfoFinal Info -> [s] -> Either ParseError [t]
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
