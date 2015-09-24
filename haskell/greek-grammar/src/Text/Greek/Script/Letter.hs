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
import qualified Text.Greek.Script.Word as Word

data Letter
  = L_α | L_β | L_γ | L_δ | L_ε | L_ζ | L_η | L_θ | L_ι | L_κ | L_λ | L_μ
  | L_ν | L_ξ | L_ο | L_π | L_ρ | L_σ | L_τ | L_υ | L_φ | L_χ | L_ψ | L_ω
  deriving (Eq, Ord, Show)
data Case = Lowercase | Uppercase deriving (Eq, Ord, Show)
data IsLast = IsNotLast | IsLast deriving (Eq, Ord, Show)

data IsFinal = IsFinal | IsNotFinal deriving (Eq, Ord, Show)
data LetterFinal
  = LF_α | LF_β | LF_γ | LF_δ | LF_ε | LF_ζ | LF_η | LF_θ | LF_ι | LF_κ | LF_λ | LF_μ
  | LF_ν | LF_ξ | LF_ο | LF_π | LF_ρ | LF_σ IsFinal | LF_τ | LF_υ | LF_φ | LF_χ | LF_ψ | LF_ω
  deriving (Eq, Ord, Show)

toCaseLetterFinal :: UnicodeLetter -> (Case, LetterFinal)
toCaseLetterFinal U_Α = (Uppercase, LF_α)
toCaseLetterFinal U_Β = (Uppercase, LF_β)
toCaseLetterFinal U_Γ = (Uppercase, LF_γ)
toCaseLetterFinal U_Δ = (Uppercase, LF_δ)
toCaseLetterFinal U_Ε = (Uppercase, LF_ε)
toCaseLetterFinal U_Ζ = (Uppercase, LF_ζ)
toCaseLetterFinal U_Η = (Uppercase, LF_η)
toCaseLetterFinal U_Θ = (Uppercase, LF_θ)
toCaseLetterFinal U_Ι = (Uppercase, LF_ι)
toCaseLetterFinal U_Κ = (Uppercase, LF_κ)
toCaseLetterFinal U_Λ = (Uppercase, LF_λ)
toCaseLetterFinal U_Μ = (Uppercase, LF_μ)
toCaseLetterFinal U_Ν = (Uppercase, LF_ν)
toCaseLetterFinal U_Ξ = (Uppercase, LF_ξ)
toCaseLetterFinal U_Ο = (Uppercase, LF_ο)
toCaseLetterFinal U_Π = (Uppercase, LF_π)
toCaseLetterFinal U_Ρ = (Uppercase, LF_ρ)
toCaseLetterFinal U_Σ = (Uppercase, LF_σ IsNotFinal)
toCaseLetterFinal U_Τ = (Uppercase, LF_τ)
toCaseLetterFinal U_Υ = (Uppercase, LF_υ)
toCaseLetterFinal U_Φ = (Uppercase, LF_φ)
toCaseLetterFinal U_Χ = (Uppercase, LF_χ)
toCaseLetterFinal U_Ψ = (Uppercase, LF_ψ)
toCaseLetterFinal U_Ω = (Uppercase, LF_ω)
toCaseLetterFinal U_α = (Lowercase, LF_α)
toCaseLetterFinal U_β = (Lowercase, LF_β)
toCaseLetterFinal U_γ = (Lowercase, LF_γ)
toCaseLetterFinal U_δ = (Lowercase, LF_δ)
toCaseLetterFinal U_ε = (Lowercase, LF_ε)
toCaseLetterFinal U_ζ = (Lowercase, LF_ζ)
toCaseLetterFinal U_η = (Lowercase, LF_η)
toCaseLetterFinal U_θ = (Lowercase, LF_θ)
toCaseLetterFinal U_ι = (Lowercase, LF_ι)
toCaseLetterFinal U_κ = (Lowercase, LF_κ)
toCaseLetterFinal U_λ = (Lowercase, LF_λ)
toCaseLetterFinal U_μ = (Lowercase, LF_μ)
toCaseLetterFinal U_ν = (Lowercase, LF_ν)
toCaseLetterFinal U_ξ = (Lowercase, LF_ξ)
toCaseLetterFinal U_ο = (Lowercase, LF_ο)
toCaseLetterFinal U_π = (Lowercase, LF_π)
toCaseLetterFinal U_ρ = (Lowercase, LF_ρ)
toCaseLetterFinal U_σ = (Lowercase, LF_σ IsNotFinal)
toCaseLetterFinal U_ς = (Lowercase, LF_σ IsFinal)
toCaseLetterFinal U_τ = (Lowercase, LF_τ)
toCaseLetterFinal U_υ = (Lowercase, LF_υ)
toCaseLetterFinal U_φ = (Lowercase, LF_φ)
toCaseLetterFinal U_χ = (Lowercase, LF_χ)
toCaseLetterFinal U_ψ = (Lowercase, LF_ψ)
toCaseLetterFinal U_ω = (Lowercase, LF_ω)

letterFinalToLetter :: LetterFinal -> Letter
letterFinalToLetter LF_α = L_α
letterFinalToLetter LF_β = L_β
letterFinalToLetter LF_γ = L_γ
letterFinalToLetter LF_δ = L_δ
letterFinalToLetter LF_ε = L_ε
letterFinalToLetter LF_ζ = L_ζ
letterFinalToLetter LF_η = L_η
letterFinalToLetter LF_θ = L_θ
letterFinalToLetter LF_ι = L_ι
letterFinalToLetter LF_κ = L_κ
letterFinalToLetter LF_λ = L_λ
letterFinalToLetter LF_μ = L_μ
letterFinalToLetter LF_ν = L_ν
letterFinalToLetter LF_ξ = L_ξ
letterFinalToLetter LF_ο = L_ο
letterFinalToLetter LF_π = L_π
letterFinalToLetter LF_ρ = L_ρ
letterFinalToLetter (LF_σ _) = L_σ
letterFinalToLetter LF_τ = L_τ
letterFinalToLetter LF_υ = L_υ
letterFinalToLetter LF_φ = L_φ
letterFinalToLetter LF_χ = L_χ
letterFinalToLetter LF_ψ = L_ψ
letterFinalToLetter LF_ω = L_ω


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

letterFinalToLetterChar :: LetterFinal -> LetterChar
letterFinalToLetterChar (LF_σ IsFinal) = LetterChar 'ς'
letterFinalToLetterChar x = toLetterChar $ letterFinalToLetter x

primLensMaybe :: Show s => (s -> LineReference) -> LensLike Maybe s t a b -> (a -> Maybe b) -> Parser [s] t
primLensMaybe f g h = primMaybe f (g h)

nonFinalLetterParser :: Show s => (s -> LineReference) -> Lens s t LetterFinal (Letter, IsLast) -> Parser [s] t
nonFinalLetterParser f g = primLensMaybe f g apply
  where
    apply (LF_σ IsFinal) = Nothing
    apply x = Just (letterFinalToLetter x, IsNotLast)

finalLetterParser :: Show s => (s -> LineReference) -> Lens s t LetterFinal (Letter, IsLast) -> Parser [s] t
finalLetterParser f g = primLensMaybe f g apply
  where
    apply (LF_σ IsNotFinal) = Nothing
    apply x = Just (letterFinalToLetter x, IsLast)

finalParser :: Show s => (s -> LineReference) -> Lens s t LetterFinal (Letter, IsLast) -> Parser [s] [t]
finalParser f g = tryManyEndEof (nonFinalLetterParser f g) (finalLetterParser f g)

parseFinals :: Show s => (s -> LineReference) -> Lens s t LetterFinal (Letter, IsLast) -> [s] -> Either ParseError [t]
parseFinals f g = parse (finalParser f g) ""


lowercaseParser :: Show s => (s -> LineReference) -> Lens s t Case () -> Parser [s] t
lowercaseParser f g = primLensMaybe f g apply
  where
    apply Lowercase = Just ()
    apply Uppercase = Nothing

uppercaseParser :: Show s => (s -> LineReference) -> Lens s t Case () -> Parser [s] t
uppercaseParser f g = primLensMaybe f g apply
  where
    apply Lowercase = Nothing
    apply Uppercase = Just ()

capitalizedParser :: Show s => (s -> LineReference) -> Lens s t Case () -> Parser [s] (Word.IsCapitalized, [t])
capitalizedParser f g = do
  first <- uppercaseParser f g
  remaining <- many (lowercaseParser f g)
  _ <- eof
  return (Word.IsCapitalized, first : remaining)

uncapitalizedParser :: Show s => (s -> LineReference) -> Lens s t Case () -> Parser [s] (Word.IsCapitalized, [t])
uncapitalizedParser f g = do
  result <- many1 (lowercaseParser f g)
  _ <- eof
  return (Word.IsNotCapitalized, result)

parseCase :: Show s => (s -> LineReference) -> Lens s t Case () -> [s] -> Either ParseError (Word.IsCapitalized, [t])
parseCase f g = parse (capitalizedParser f g <|> uncapitalizedParser f g) ""


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
