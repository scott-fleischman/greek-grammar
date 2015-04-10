module Text.Greek.Phonology.Euphony where

import Text.Greek.Phonology.Contractions
import Text.Greek.Phonology.Phoneme

applyAllContractions :: [Phoneme] -> [[Phoneme]]
applyAllContractions [] = []
applyAllContractions (p1 : p2 : ps)
  | MkVowel v1 <- p1
  , MkVowel v2 <- p2
  , cs <- getContractions v1 v2
  = fmap (p1 :) $ applyAllContractions (p2 : ps)

  | True
  = fmap (p1 :) $ applyAllContractions (p2 : ps)
applyAllContractions (p : []) = [[p]]
