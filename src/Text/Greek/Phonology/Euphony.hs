module Text.Greek.Phonology.Euphony where

import Data.List (nub)
import Text.Greek.Phonology.Contractions
import Text.Greek.Phonology.Phoneme

-- smyth 55 - apply euphony rules greedily from the right

contract :: Phoneme -> [Phoneme] -> [[Phoneme]]
contract n [] = [[n]]
contract n (p : ps)
  | MkVowel v1 <- n
  , MkVowel v2 <- p
  , cs@(_ : _) <- getContractions v1 v2
  = fmap (: ps) . fmap MkVowel $ cs

  | True
  = [n : p : ps]

contractAll :: Phoneme -> [[Phoneme]] -> [[Phoneme]]
contractAll n ps = nub $ concatMap (\x -> contract n x) ps

applyAllContractions :: [Phoneme] -> [[Phoneme]]
applyAllContractions = foldr contractAll [[]]
