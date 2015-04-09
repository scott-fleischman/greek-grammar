module Text.Greek.Phonology.Vowels where

data Vowel = Alpha | Epsilon | Eta | Iota | Omicron | Upsilon | Omega

data Diphthong = Genuine Vowel Vowel | Spurious Vowel Vowel | Improper Vowel  

data VowelLength = Short | Long

data RoundedLip = LipClosed | LipClosedMedium | LipOpen | LipOpenMedium

data TonguePosition = TongueClosed | TongueClosedMedium | TongueOpen | TongueOpenMedium  


data VowelPhoneme =
    VowelPhoneme Vowel VowelLength
  | DiphthongPhoneme Diphthong

alphaShort :: VowelPhoneme
alphaShort = VowelPhoneme Alpha Short

alphaLong :: VowelPhoneme
alphaLong = VowelPhoneme Alpha Long

ouGenuine :: VowelPhoneme
ouGenuine = DiphthongPhoneme (Genuine Omicron Upsilon)

ouSpurious :: VowelPhoneme
ouSpurious = DiphthongPhoneme (Spurious Omicron Omicron)

data Contraction = Contraction
  { target :: VowelPhoneme
  , first :: VowelPhoneme
  , second :: VowelPhoneme
  }

contractions :: [Contraction]
contractions =
  [ Contraction alphaLong alphaShort alphaShort
  , Contraction alphaLong alphaLong alphaShort
  , Contraction alphaLong alphaShort alphaLong
  ]
