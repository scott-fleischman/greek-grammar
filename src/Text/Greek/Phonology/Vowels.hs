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

spuriousOU :: VowelPhoneme
spuriousOU = SpuriousDiphthong Omicron Upsilon 

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
  , Contraction alphaIota (alpha Short) alphaIota
  , Contraction improperAlpha (alpha Short) improperAlpha
  , Contraction (alpha Long) (alpha Short) epsilon
  , Contraction improperAlpha (alpha Short) epsilonIota
  , Contraction (alpha Long) (alpha Short) spuriousEI
  , Contraction (alpha Long) (alpha Short) eta
  , Contraction improperAlpha (alpha Short) improperEta
  , Contraction alphaIota (alpha Short) (iota Short)
  , Contraction improperAlpha (alpha Long) (iota Short)
  , Contraction omega (alpha Short) omicron
  , Contraction improperOmega (alpha Short) omicronIota
  , Contraction omega (alpha Short) spuriousOU
  , Contraction omega (alpha Short) omega
  , Contraction eta epsilon (alpha Short)
  , Contraction (alpha Long) epsilon (alpha Short)
  , Contraction eta epsilon (alpha Long)
  , Contraction eta eta eta 
  , Contraction improperEta eta improperEta
  , Contraction improperOmega eta omicronIota
  , Contraction improperEta eta (iota Short)
  , Contraction (iota Long) (iota Short) (iota Short)
  , Contraction omega omicron (alpha Short)
  , Contraction (alpha Long) omicron (alpha Short)
  , Contraction (Diphthong Omicron Upsilon) omicron omicron
  ]

tonguePosition :: VowelPhoneme -> TonguePosition
tonguePosition (VowelPhoneme Alpha _) = TongueOpen
tonguePosition (VowelPhoneme Epsilon _) = TongueClosedMedium
tonguePosition (VowelPhoneme Eta _) = TongueOpenMedium
tonguePosition (VowelPhoneme Iota _) = TongueClosed
tonguePosition (VowelPhoneme Omicron _) = TongueClosedMedium
tonguePosition (VowelPhoneme Upsilon _) = TongueClosed
tonguePosition (VowelPhoneme Omega _) = TongueOpenMedium
tonguePosition (Diphthong _ _) = TongueClosing
tonguePosition _ = TongueOpen

roundedLip :: VowelPhoneme -> RoundedLip
roundedLip (VowelPhoneme Omicron _) = Round
roundedLip (VowelPhoneme Upsilon _) = Round
roundedLip (VowelPhoneme Omega _) = Round
roundedLip (Diphthong Omicron Upsilon) = Round
roundedLip _ = Unround
