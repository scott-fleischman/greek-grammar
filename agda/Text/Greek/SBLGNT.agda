module Text.Greek.SBLGNT where

open import Data.Nat
open import Data.List
open import Text.Greek.SBLGNT.FirstJohn
open import Text.Greek.SBLGNT.Luke

wordCount = length ΚΑΤΑ-ΛΟΥΚΑΝ
tokenCount = length (concat ΚΑΤΑ-ΛΟΥΚΑΝ)
