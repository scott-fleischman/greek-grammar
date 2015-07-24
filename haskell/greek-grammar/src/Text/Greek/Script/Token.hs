{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Script.Token where

import Control.Lens
import Data.Data
import Data.List
import Data.Maybe

data Letter
  = L_α | L_β | L_γ | L_δ | L_ε | L_ζ | L_η | L_θ | L_ι | L_κ | L_λ | L_μ
  | L_ν | L_ξ | L_ο | L_π | L_ρ | L_σ | L_τ | L_υ | L_φ | L_χ | L_ψ | L_ω
  deriving (Eq, Show, Ord, Data, Typeable)
data LetterCase = Lowercase | Uppercase deriving (Eq, Show, Ord, Data, Typeable)
data Accent = Acute | Grave | Circumflex deriving (Eq, Show, Ord, Data, Typeable)
data Breathing = Smooth | Rough deriving (Eq, Show, Ord, Data, Typeable)
data IotaSubscript = IotaSubscript deriving (Eq, Show, Ord, Data, Typeable)
data Diaeresis = Diaeresis deriving (Eq, Show, Ord, Data, Typeable)
data FinalForm = FinalForm deriving (Eq, Show, Ord, Data, Typeable)

data Token = Token
  { _letter :: Letter
  , _letterCase :: LetterCase
  , _accent :: Maybe Accent
  , _breathing :: Maybe Breathing
  , _iotaSubscript :: Maybe IotaSubscript
  , _diaeresis :: Maybe Diaeresis
  , _finalForm :: Maybe FinalForm
  }
  deriving (Eq, Show, Ord, Data, Typeable)
makeLenses ''Token

data Consonant = C_β | C_γ | C_δ | C_ζ | C_θ | C_κ | C_λ | C_μ | C_ν | C_ξ | C_π | C_ρ | C_σ | C_τ | C_φ | C_χ | C_ψ
  deriving (Eq, Show, Ord, Data, Typeable)
data Vowel = V_α | V_ε | V_η | V_ι | V_ο | V_υ | V_ω
  deriving (Eq, Show, Ord, Data, Typeable)

divideLetter :: Letter -> Either Consonant Vowel
divideLetter L_β = Left C_β
divideLetter L_γ = Left C_γ
divideLetter L_δ = Left C_δ
divideLetter L_ζ = Left C_ζ
divideLetter L_θ = Left C_θ
divideLetter L_κ = Left C_κ
divideLetter L_λ = Left C_λ
divideLetter L_μ = Left C_μ
divideLetter L_ν = Left C_ν
divideLetter L_ξ = Left C_ξ
divideLetter L_π = Left C_π
divideLetter L_ρ = Left C_ρ
divideLetter L_σ = Left C_σ
divideLetter L_τ = Left C_τ
divideLetter L_φ = Left C_φ
divideLetter L_χ = Left C_χ
divideLetter L_ψ = Left C_ψ
divideLetter L_α = Right V_α
divideLetter L_ε = Right V_ε
divideLetter L_η = Right V_η
divideLetter L_ι = Right V_ι
divideLetter L_ο = Right V_ο
divideLetter L_υ = Right V_υ
divideLetter L_ω = Right V_ω

unmarkedLetter :: Letter -> LetterCase -> Token
unmarkedLetter el c = Token el c Nothing Nothing Nothing Nothing Nothing

vowels :: [Letter]
vowels = [L_α, L_ε, L_η, L_ι, L_ο, L_υ, L_ω]

iotaSubscriptVowels :: [Letter]
iotaSubscriptVowels = [L_α, L_η, L_ω]

alwaysShortVowels :: [Letter]
alwaysShortVowels = [L_ε, L_ο]

alwaysLongVowels :: [Letter]
alwaysLongVowels = [L_η, L_ω]

diphthongs :: [(Letter, Letter)]
diphthongs =
  [ (L_α, L_ι)
  , (L_ε, L_ι)
  , (L_ο, L_ι)
  , (L_υ, L_ι)
  , (L_α, L_υ)
  , (L_ε, L_υ)
  , (L_ο, L_υ)
  , (L_η, L_υ)
  ]

diphthongSecondVowels :: [Letter]
diphthongSecondVowels = nub . fmap snd $ diphthongs

isValidAccent :: Letter -> Accent -> Bool
isValidAccent el Acute = el `elem` vowels
isValidAccent el Grave = el `elem` vowels
isValidAccent el Circumflex = el `elem` vowels && not (el `elem` alwaysShortVowels)

isValidBreathing :: Letter -> LetterCase -> Breathing -> Bool
isValidBreathing el Lowercase Smooth = el `elem` vowels || el == L_ρ
isValidBreathing el Uppercase Smooth = el `elem` vowels && el /= L_υ
isValidBreathing el _ Rough = el `elem` vowels || el == L_ρ

isValidIotaSubscript :: Letter -> IotaSubscript -> Bool
isValidIotaSubscript el _ = el `elem` iotaSubscriptVowels

isValidDiaeresis :: Letter -> Diaeresis -> Bool
isValidDiaeresis el _ = el `elem` diphthongSecondVowels

isValidFinalForm :: Letter -> LetterCase -> FinalForm -> Bool
isValidFinalForm L_σ Lowercase _ = True
isValidFinalForm _ _ _ = False

data ValidationError =
    AccentError | BreathingError
  | IotaSubscriptError | DiaeresisError | FinalFormError
  deriving (Eq, Show)

validateItem :: (a -> Bool) -> Maybe a -> ValidationError -> Maybe ValidationError
validateItem _ Nothing _ = Nothing
validateItem isValid (Just v) e = case isValid v of
  True -> Nothing
  False -> Just e

validateToken :: Token -> [ValidationError]
validateToken (Token el c a b is d f) = concatMap maybeToList $
  [ validateItem (isValidAccent el) a AccentError
  , validateItem (isValidBreathing el c) b BreathingError
  , validateItem (isValidIotaSubscript el) is IotaSubscriptError
  , validateItem (isValidDiaeresis el) d DiaeresisError
  , validateItem (isValidFinalForm el c) f FinalFormError
  ]

removeSmoothBreathing :: Maybe Breathing -> Maybe Breathing
removeSmoothBreathing (Just Smooth) = Nothing
removeSmoothBreathing x = x
