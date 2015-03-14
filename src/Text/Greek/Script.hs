module Text.Greek.Script where

{-
letters: α β γ δ ε ζ η θ ι κ λ μ ν ξ ο π ρ σ τ υ φ χ ψ ω
vowels: α ε η ι ο υ ω
consonants: letters except vowels

always have case: letters

can have iota subscript: α η ω
can have diaeresis : vowels
can have breathing : vowels ρ

can have acute accent : vowels
can have grave accent : vowels
can have circumflex : vowels

can have final form : σ
-}

data BasicConsonant = Beta | Gamma | Delta | Zeta | Theta | Kappa | Lambda | Mu | Nu | Xi | Pi | Tau | Phi | Chi | Psi
data Rho
data Sigma
data BasicVowel = Epsilon | Iota | Omicron | Upsilon
data IotaSubscriptVowel = Alpha | Eta | Omega
data Case = Lowercase | Uppercase
data Accent = Acute | Grave | Circumflex
data Breathing = Smooth | Rough
data IotaSubscript
data Diaeresis
data Final

data Letter =
    LetterBasicConsonant BasicConsonant
  | LetterRho Rho
  | LetterSigma Sigma
  | LetterBasicVowel BasicVowel
  | LetterIotaSubscriptVowel IotaSubscriptVowel

data VowelMarks = VowelMarks
  { accent :: Maybe Accent
  , breathing :: Maybe Breathing
  , diaeresis :: Maybe Diaeresis
  }

data MarkedLetter =
    BasicConsonant
  | MarkedRho Rho (Maybe Breathing)
  | MarkedSigma Sigma (Maybe Final)
  | BasicVowel BasicVowel VowelMarks
  | IotaSubscriptVowel IotaSubscriptVowel (Maybe IotaSubscript) VowelMarks

data CasedLetter a = CasedLetter
  { letterMarked :: MarkedLetter
  , letterCase :: Case
  , letterSource :: a
  }
