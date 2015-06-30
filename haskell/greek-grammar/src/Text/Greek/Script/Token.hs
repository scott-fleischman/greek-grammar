{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Script.Token where

import Prelude (Bool(..), Eq(..), Show(..), Ord, not, ($), (.), (||), (&&), fmap, snd)
import Control.Lens (makeLenses)
import Data.Data (Data, Typeable)
import Data.Foldable (concatMap)
import Data.List (elem, nub)
import Data.Maybe (Maybe(..), maybeToList)

data Letter =
    Alpha | Beta | Gamma | Delta | Epsilon | Digamma | Zeta | Eta | Theta | Iota | Kappa | Lambda
  | Mu | Nu | Xi | Omicron | Pi | Rho | Sigma | Tau | Upsilon | Phi | Chi | Psi | Omega
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

data TokenContext a = TokenContext
  { _token :: Token
  , _context :: a
  }
  deriving (Eq, Show, Data, Typeable)
makeLenses ''TokenContext

unmarkedLetter :: Letter -> LetterCase -> Token
unmarkedLetter el c = Token el c Nothing Nothing Nothing Nothing Nothing

vowels :: [Letter]
vowels = [Alpha, Epsilon, Eta, Iota, Omicron, Upsilon, Omega]

iotaSubscriptVowels :: [Letter]
iotaSubscriptVowels = [Alpha, Eta, Omega]

alwaysShortVowels :: [Letter]
alwaysShortVowels = [Epsilon, Omicron]

alwaysLongVowels :: [Letter]
alwaysLongVowels = [Eta, Omega]

diphthongs :: [(Letter, Letter)]
diphthongs =
  [ (Alpha, Iota)
  , (Epsilon, Iota)
  , (Omicron, Iota)
  , (Upsilon, Iota)
  , (Alpha, Upsilon)
  , (Epsilon, Upsilon)
  , (Omicron, Upsilon)
  , (Eta, Upsilon)
  ]

diphthongSecondVowels :: [Letter]
diphthongSecondVowels = nub . fmap snd $ diphthongs

isValidAccent :: Letter -> Accent -> Bool
isValidAccent el Acute = el `elem` vowels
isValidAccent el Grave = el `elem` vowels
isValidAccent el Circumflex = el `elem` vowels && not (el `elem` alwaysShortVowels)

isValidBreathing :: Letter -> LetterCase -> Breathing -> Bool
isValidBreathing el Lowercase Smooth = el `elem` vowels || el == Rho
isValidBreathing el Uppercase Smooth = el `elem` vowels && el /= Upsilon
isValidBreathing el _ Rough = el `elem` vowels || el == Rho

isValidIotaSubscript :: Letter -> IotaSubscript -> Bool
isValidIotaSubscript el _ = el `elem` iotaSubscriptVowels

isValidDiaeresis :: Letter -> Diaeresis -> Bool
isValidDiaeresis el _ = el `elem` diphthongSecondVowels

isValidFinalForm :: Letter -> LetterCase -> FinalForm -> Bool
isValidFinalForm Sigma Lowercase _ = True
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
