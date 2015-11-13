{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Greek.Script.Abstract where

import qualified Text.Greek.Script.Concrete as Concrete
import qualified Text.Greek.Script.Unicode as Unicode

data Letter
  = L_α | L_β | L_γ | L_δ | L_ε | L_ζ | L_η | L_θ | L_ι | L_κ | L_λ | L_μ
  | L_ν | L_ξ | L_ο | L_π | L_ρ | L_σ | L_τ | L_υ | L_φ | L_χ | L_ψ | L_ω
  deriving (Eq, Ord, Show)
data Case = Lowercase | Uppercase deriving (Eq, Ord, Show)

data Final = FinalNotSupported | IsFinal | IsNotFinal deriving (Eq, Ord, Show)

newtype LetterIndex = LetterIndex { getLetterIndex :: Int } deriving (Eq, Show, Ord)
newtype CaseIndex = CaseIndex { getCaseIndex :: Int } deriving (Eq, Show, Ord)
newtype FinalReverseIndex = FinalReverseIndex { getFinalReverseIndex :: Int } deriving (Eq, Show, Ord)

toLetterCaseFinal :: Concrete.Letter -> (Letter, Case, Final)
toLetterCaseFinal Concrete.C_Α = (L_α, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Β = (L_β, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Γ = (L_γ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Δ = (L_δ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Ε = (L_ε, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Ζ = (L_ζ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Η = (L_η, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Θ = (L_θ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Ι = (L_ι, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Κ = (L_κ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Λ = (L_λ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Μ = (L_μ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Ν = (L_ν, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Ξ = (L_ξ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Ο = (L_ο, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Π = (L_π, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Ρ = (L_ρ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Σ = (L_σ, Uppercase, IsNotFinal)
toLetterCaseFinal Concrete.C_Τ = (L_τ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Υ = (L_υ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Φ = (L_φ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Χ = (L_χ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Ψ = (L_ψ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Ω = (L_ω, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_α = (L_α, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_β = (L_β, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_γ = (L_γ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_δ = (L_δ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_ε = (L_ε, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_ζ = (L_ζ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_η = (L_η, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_θ = (L_θ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_ι = (L_ι, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_κ = (L_κ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_λ = (L_λ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_μ = (L_μ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_ν = (L_ν, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_ξ = (L_ξ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_ο = (L_ο, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_π = (L_π, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_ρ = (L_ρ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_σ = (L_σ, Lowercase, IsNotFinal)
toLetterCaseFinal Concrete.C_ς = (L_σ, Lowercase, IsFinal)
toLetterCaseFinal Concrete.C_τ = (L_τ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_υ = (L_υ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_φ = (L_φ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_χ = (L_χ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_ψ = (L_ψ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_ω = (L_ω, Lowercase, FinalNotSupported)

letterToUnicode :: Letter -> Unicode.Letter
letterToUnicode L_α = Unicode.Letter 'α'
letterToUnicode L_β = Unicode.Letter 'β'
letterToUnicode L_γ = Unicode.Letter 'γ'
letterToUnicode L_δ = Unicode.Letter 'δ'
letterToUnicode L_ε = Unicode.Letter 'ε'
letterToUnicode L_ζ = Unicode.Letter 'ζ'
letterToUnicode L_η = Unicode.Letter 'η'
letterToUnicode L_θ = Unicode.Letter 'θ'
letterToUnicode L_ι = Unicode.Letter 'ι'
letterToUnicode L_κ = Unicode.Letter 'κ'
letterToUnicode L_λ = Unicode.Letter 'λ'
letterToUnicode L_μ = Unicode.Letter 'μ'
letterToUnicode L_ν = Unicode.Letter 'ν'
letterToUnicode L_ξ = Unicode.Letter 'ξ'
letterToUnicode L_ο = Unicode.Letter 'ο'
letterToUnicode L_π = Unicode.Letter 'π'
letterToUnicode L_ρ = Unicode.Letter 'ρ'
letterToUnicode L_σ = Unicode.Letter 'σ'
letterToUnicode L_τ = Unicode.Letter 'τ'
letterToUnicode L_υ = Unicode.Letter 'υ'
letterToUnicode L_φ = Unicode.Letter 'φ'
letterToUnicode L_χ = Unicode.Letter 'χ'
letterToUnicode L_ψ = Unicode.Letter 'ψ'
letterToUnicode L_ω = Unicode.Letter 'ω'



--letterFinalToLetterChar :: LetterFinal -> LetterChar
--letterFinalToLetterChar (LF_σ IsFinal) = LetterChar 'ς'
--letterFinalToLetterChar x = toLetterChar $ letterFinalToLetter x

--nonFinalLetterParser :: Show s => (s -> LineReference) -> Lens s t LetterFinal Letter -> Parser [s] t
--nonFinalLetterParser f g = primLensMaybe "Non-final letter" f g apply
--  where
--    apply (LF_σ IsFinal) = Nothing
--    apply x = Just (letterFinalToLetter x)

--finalLetterParser :: Show s => (s -> LineReference) -> Lens s t LetterFinal Letter -> Parser [s] t
--finalLetterParser f g = primLensMaybe "Final letter" f g apply
--  where
--    apply (LF_σ IsNotFinal) = Nothing
--    apply x = Just (letterFinalToLetter x)

--finalParser :: Show s => (s -> LineReference) -> Lens s t LetterFinal Letter -> Parser [s] [t]
--finalParser f g = tryManyEndEof (nonFinalLetterParser f g) (finalLetterParser f g)

--parseFinals :: Show s => (s -> LineReference) -> Lens s t LetterFinal Letter -> [s] -> Either ParseError [t]
--parseFinals f g = parse (finalParser f g) ""


--lowercaseParser :: Show s => (s -> LineReference) -> Lens s t Case () -> Parser [s] t
--lowercaseParser f g = primLensMaybe "Lowercase" f g apply
--  where
--    apply Lowercase = Just ()
--    apply Uppercase = Nothing

--uppercaseParser :: Show s => (s -> LineReference) -> Lens s t Case () -> Parser [s] t
--uppercaseParser f g = primLensMaybe "Uppercase" f g apply
--  where
--    apply Lowercase = Nothing
--    apply Uppercase = Just ()

--capitalizedParser :: Show s => (s -> LineReference) -> Lens s t Case () -> Parser [s] (Word.IsCapitalized, [t])
--capitalizedParser f g = do
--  first <- uppercaseParser f g
--  remaining <- many (lowercaseParser f g)
--  _ <- eof
--  return (Word.IsCapitalized, first : remaining)

--uncapitalizedParser :: Show s => (s -> LineReference) -> Lens s t Case () -> Parser [s] (Word.IsCapitalized, [t])
--uncapitalizedParser f g = do
--  result <- many1 (lowercaseParser f g)
--  _ <- eof
--  return (Word.IsNotCapitalized, result)

--parseCase :: Show s => (s -> LineReference) -> Lens s t Case () -> [s] -> Either ParseError (Word.IsCapitalized, [t])
--parseCase f g = parse (try (capitalizedParser f g) <|> uncapitalizedParser f g) ""


data Vowel = V_α | V_ε | V_η | V_ι | V_ο | V_υ | V_ω deriving (Eq, Show, Ord)
data Consonant = C_β | C_γ | C_δ | C_ζ | C_θ | C_κ | C_λ | C_μ | C_ν | C_ξ | C_π | C_ρ | C_σ | C_τ | C_φ | C_χ | C_ψ deriving (Eq, Show, Ord)

type VowelConsonant = Either Vowel Consonant

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

vowelToLetter :: Vowel -> Letter
vowelToLetter V_α = L_α
vowelToLetter V_ε = L_ε
vowelToLetter V_η = L_η
vowelToLetter V_ι = L_ι
vowelToLetter V_ο = L_ο
vowelToLetter V_υ = L_υ
vowelToLetter V_ω = L_ω

consonantToLetter :: Consonant -> Letter
consonantToLetter C_β = L_β
consonantToLetter C_γ = L_γ
consonantToLetter C_δ = L_δ
consonantToLetter C_ζ = L_ζ
consonantToLetter C_θ = L_θ
consonantToLetter C_κ = L_κ
consonantToLetter C_λ = L_λ
consonantToLetter C_μ = L_μ
consonantToLetter C_ν = L_ν
consonantToLetter C_ξ = L_ξ
consonantToLetter C_π = L_π
consonantToLetter C_ρ = L_ρ
consonantToLetter C_σ = L_σ
consonantToLetter C_τ = L_τ
consonantToLetter C_φ = L_φ
consonantToLetter C_χ = L_χ
consonantToLetter C_ψ = L_ψ

type VowelCluster = [Vowel]
type ConsonantCluster = [Consonant]
type VowelConsonantCluster = Either VowelCluster ConsonantCluster

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
