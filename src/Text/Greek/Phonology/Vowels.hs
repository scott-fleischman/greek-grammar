module Text.Greek.Phonology.Vowels where

data Vowel = Alpha | Epsilon | Eta | Iota | Omicron | Upsilon | Omega
  deriving (Eq, Ord, Show)

data VowelLength = Short | Long
  deriving (Eq, Ord, Show)

data RoundedLip = Round | Unround

data TonguePosition = TongueClosed | TongueClosedMedium | TongueOpen | TongueOpenMedium | TongueClosing | TongueOpening


data VowelPhoneme =
    VowelPhoneme Vowel VowelLength
  | Diphthong Vowel Vowel 
  | ImproperDiphthong Vowel
  | SpuriousDiphthong Vowel Vowel
  deriving (Eq, Ord, Show)

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
