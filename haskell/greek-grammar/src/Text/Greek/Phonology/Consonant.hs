module Text.Greek.Phonology.Consonant where

import qualified Text.Greek.Script.Abstract as Abstract
import qualified Text.Greek.Script.Mark as Mark

data PlusRhoRough = RR_β | RR_γ | RR_δ | RR_ζ | RR_θ | RR_κ | RR_λ | RR_μ | RR_ν | RR_ξ | RR_π | RR_ρ | RR_ῥ | RR_σ | RR_τ | RR_φ | RR_χ | RR_ψ
  deriving (Eq, Show, Ord)

reifyBreathing :: (Abstract.Consonant, Maybe Mark.Breathing) -> Maybe PlusRhoRough
reifyBreathing (Abstract.C_β, Nothing) = Just RR_β
reifyBreathing (Abstract.C_γ, Nothing) = Just RR_γ
reifyBreathing (Abstract.C_δ, Nothing) = Just RR_δ
reifyBreathing (Abstract.C_ζ, Nothing) = Just RR_ζ
reifyBreathing (Abstract.C_θ, Nothing) = Just RR_θ
reifyBreathing (Abstract.C_κ, Nothing) = Just RR_κ
reifyBreathing (Abstract.C_λ, Nothing) = Just RR_λ
reifyBreathing (Abstract.C_μ, Nothing) = Just RR_μ
reifyBreathing (Abstract.C_ν, Nothing) = Just RR_ν
reifyBreathing (Abstract.C_ξ, Nothing) = Just RR_ξ
reifyBreathing (Abstract.C_π, Nothing) = Just RR_π
reifyBreathing (Abstract.C_ρ, Nothing) = Just RR_ρ
reifyBreathing (Abstract.C_ρ, Just Mark.BreathingRough) = Just RR_ῥ
reifyBreathing (Abstract.C_σ, Nothing) = Just RR_σ
reifyBreathing (Abstract.C_τ, Nothing) = Just RR_τ
reifyBreathing (Abstract.C_φ, Nothing) = Just RR_φ
reifyBreathing (Abstract.C_χ, Nothing) = Just RR_χ
reifyBreathing (Abstract.C_ψ, Nothing) = Just RR_ψ
reifyBreathing _ = Nothing
