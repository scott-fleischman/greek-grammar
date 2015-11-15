module Text.Greek.Phonology.Consonant where

import qualified Text.Greek.Script.Abstract as Abstract
import qualified Text.Greek.Script.Mark as Mark

data RR_ῥ = RR_ῥ deriving (Eq, Ord, Show)

type PlusRhoRough = Either Abstract.Consonant RR_ῥ

reifyBreathing :: (Abstract.Consonant, Maybe Mark.Breathing) -> Maybe PlusRhoRough
reifyBreathing (Abstract.C_ρ, Just Mark.BreathingRough) = Just . Right $ RR_ῥ
reifyBreathing (c, Nothing) = Just . Left $ c
reifyBreathing _ = Nothing
