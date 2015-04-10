module Text.Greek.Phonology.Vowels where

data Vowel = Alpha | Epsilon | Eta | Iota | Omicron | Upsilon | Omega

data VowelLength = Short | Long

data RoundedLip = Round | Unround

data TonguePosition = TongueClosed | TongueClosedMedium | TongueOpen | TongueOpenMedium | TongueClosing | TongueOpening


data VowelPhoneme =
    VowelPhoneme Vowel VowelLength
  | Diphthong Vowel Vowel
  | ImproperDiphthong Vowel
  | SpuriousDiphthong Vowel Vowel

alpha :: VowelLength -> VowelPhoneme
alpha len = VowelPhoneme Alpha len

omicron :: VowelPhoneme
omicron = VowelPhoneme Omicron Short

epsilon :: VowelPhoneme
epsilon = VowelPhoneme Epsilon Short

eta :: VowelPhoneme
eta = VowelPhoneme Eta Long

iota :: VowelLength -> VowelPhoneme
iota len = VowelPhoneme Iota len

upsilon :: VowelLength -> VowelPhoneme
upsilon len = VowelPhoneme Upsilon len

omega :: VowelPhoneme
omega = VowelPhoneme Omega Long

omicronUpsilon :: VowelPhoneme
omicronUpsilon = Diphthong Omicron Upsilon

alphaUpsilon :: VowelPhoneme 
alphaUpsilon = Diphthong Alpha Upsilon

epsilonUpsilon :: VowelPhoneme
epsilonUpsilon = Diphthong Epsilon Upsilon

etaUpsilon :: VowelPhoneme
etaUpsilon = Diphthong Eta Upsilon



















alphaIota :: VowelPhoneme
alphaIota = Diphthong Alpha Iota

epsilonIota :: VowelPhoneme
epsilonIota = Diphthong Epsilon Iota

spuriousEI :: VowelPhoneme
spuriousEI = SpuriousDiphthong Epsilon Iota

omicronIota :: VowelPhoneme
omicronIota = Diphthong Omicron Iota

improperAlpha :: VowelPhoneme
improperAlpha = ImproperDiphthong Alpha

improperEta :: VowelPhoneme
improperEta = ImproperDiphthong Eta

improperOmega :: VowelPhoneme
improperOmega = ImproperDiphthong Omega

data Contraction = Contraction
  { target :: VowelPhoneme
  , first :: VowelPhoneme
  , second :: VowelPhoneme
  }

contractions :: [Contraction]
contractions =
  [ Contraction (alpha Long) (alpha Short) (alpha Short)
  , Contraction (alpha Long) (alpha Long) (alpha Short)
  , Contraction (alpha Long) (alpha Short) (alpha Long)
  , Contraction (Diphthong Alpha Iota) (alpha Short) (Diphthong Alpha Iota)
  , Contraction (ImproperDiphthong Alpha) (alpha Short) (ImproperDiphthong Alpha)
  , Contraction (alpha Long) (alpha Short) epsilon
  , Contraction (ImproperDiphthong Alpha) (alpha Short) (Diphthong Epsilon Iota)
  , Contraction (alpha Long) (alpha Short) (SpuriousDiphthong Epsilon Iota)
  , Contraction (alpha Long) (alpha Short) eta
  --, Contraction (ImproperDiphthong Alpha) 
  , Contraction (Diphthong Omicron Upsilon) omicron omicron
  ]

properties :: [(VowelPhoneme, RoundedLip, TonguePosition)]
properties =
  [ (alpha Long, Unround, TongueOpen)
  , (alpha Short, Unround, TongueOpen)
  , (epsilon, Unround, TongueClosedMedium)
  , (eta, Unround, TongueOpenMedium)
  , (iota Short, Unround, TongueClosed)
  , (iota Long, Unround, TongueClosed)
  , (omicron, Round, TongueClosedMedium)
  , (upsilon Short, Round, TongueClosed)
  , (upsilon Long, Round, TongueClosed)
  , (omega, Round, TongueOpenMedium)
  , (omicronUpsilon, Round, TongueClosed)
  , (alphaUpsilon, Unround, TongueClosing)
  ]
