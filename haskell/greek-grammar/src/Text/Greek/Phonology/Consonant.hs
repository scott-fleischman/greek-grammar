module Text.Greek.Phonology.Consonant where

import qualified Text.Greek.Script.Abstract as Abstract
import qualified Text.Greek.Script.Mark as Mark

data PlusRoughRho = Rh_β | Rh_γ | Rh_δ | Rh_ζ | Rh_θ | Rh_κ | Rh_λ | Rh_μ | Rh_ν | Rh_ξ | Rh_π | Rh_ρ | Rh_ῥ | Rh_σ | Rh_τ | Rh_φ | Rh_χ | Rh_ψ
  deriving (Eq, Show, Ord)

reifyBreathing :: (Abstract.Consonant, Maybe Mark.Breathing) -> Maybe PlusRoughRho
reifyBreathing (Abstract.C_β, Nothing) = Just Rh_β
reifyBreathing (Abstract.C_γ, Nothing) = Just Rh_γ
reifyBreathing (Abstract.C_δ, Nothing) = Just Rh_δ
reifyBreathing (Abstract.C_ζ, Nothing) = Just Rh_ζ
reifyBreathing (Abstract.C_θ, Nothing) = Just Rh_θ
reifyBreathing (Abstract.C_κ, Nothing) = Just Rh_κ
reifyBreathing (Abstract.C_λ, Nothing) = Just Rh_λ
reifyBreathing (Abstract.C_μ, Nothing) = Just Rh_μ
reifyBreathing (Abstract.C_ν, Nothing) = Just Rh_ν
reifyBreathing (Abstract.C_ξ, Nothing) = Just Rh_ξ
reifyBreathing (Abstract.C_π, Nothing) = Just Rh_π
reifyBreathing (Abstract.C_ρ, Nothing) = Just Rh_ρ
reifyBreathing (Abstract.C_ρ, Just Mark.BreathingRough) = Just Rh_ῥ
reifyBreathing (Abstract.C_σ, Nothing) = Just Rh_σ
reifyBreathing (Abstract.C_τ, Nothing) = Just Rh_τ
reifyBreathing (Abstract.C_φ, Nothing) = Just Rh_φ
reifyBreathing (Abstract.C_χ, Nothing) = Just Rh_χ
reifyBreathing (Abstract.C_ψ, Nothing) = Just Rh_ψ
reifyBreathing _ = Nothing
