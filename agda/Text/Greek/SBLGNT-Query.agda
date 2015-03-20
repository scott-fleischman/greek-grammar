module Text.Greek.SBLGNT-Query where

open import Data.Bool
open import Data.Nat hiding (_≟_)
open import Data.List
open import Data.Maybe
open import Data.Product hiding (map)
open import Function
open import Relation.Binary.PropositionalEquality
open import Text.Greek.Bible
open import Text.Greek.Script
open import Text.Greek.SBLGNT.1John

texts = ΙΩΑΝΝΟΥ-Α ∷ []

wordCount = length ΙΩΑΝΝΟΥ-Α
--tokenCount = length (concat ΙΩΑΝΝΟΥ-Α)

getLetters : Word → List Token
getLetters (word x _) = x

tokens = concatMap getLetters ΙΩΑΝΝΟΥ-Α

adjacent-accent : (Token → Maybe Accent) → List Token → List (Σ Token (const Token))
adjacent-accent f [] = []
adjacent-accent f (_ ∷ []) = []
adjacent-accent f (t₁ ∷ t₂ ∷ ts) with (f t₁) , (f t₂) , adjacent-accent f (t₂ ∷ ts)
… | (just circumflex-mark , just circumflex-mark , ts′)  = (t₁ , t₂) ∷ ts′
… | (_ , _ , ts′) = ts′

adj = adjacent-accent get-accent tokens
