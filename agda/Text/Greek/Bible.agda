module Text.Greek.Bible where

open import Data.Nat
open import Data.List
open import Data.String
open import Text.Greek.Script

data Word : Set where
  word : (List Token) → String → Word
