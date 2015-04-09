
data Vowel = Alpha | Epsilon | Eta | Iota | Omicron | Upsilon | Omega | IotaSubscript 

data DiphthongQuality = Genuine Vowel Vowel | Spurious Vowel Vowel | Improper Vowel  

data VowelLength = Short | Long 

data RoundedLip = LipClosed | LipClosedMedium | LipOpen | LipOpenMedium

data TonguePosition = TongueClosed | TongueClosedMedium | TongueOpen | TongueOpenMedium  

data Breathing = Rough | Smooth 

data Diacritic = Circumflex | Diaeresis


diphthongs :: [(Vowel, Vowel)]
diphthongs = [ (Alpha, Iota)
  , (Epsilon, Iota)
  , (Omicron, Iota)
  , (Upsilon, Iota)
  , (Alpha, Upsilon)
  , (Epsilon, Upsilon)
  , (Omicron, Upsilon)
  , (Eta, Upsilon)
  , (Alpha, IotaSubscript)
  , (Eta, IotaSubscript)
  , (Omega, IotaSubscript)
  ]

allvowelphoneme :: [VowelPhoneme]
allVowelPhoneme = [Alpha, Epsilon, Eta, Iota, Omicron, Upsilon, Omega, IotaSubscript, (Epsilon, Iota), (Alpha, Iota) 
, (Omicron, Iota), (Alpha, Upsilon), (Epsilon, Upsilon), (Omicron, Upsilon), (Eta, Upsilon), (Alpha, IotaSubscript) 
, (Eta, IotaSubscript), (Omega, IotaSubscript)
 ]

data VowelPhonemeClassification = VowelPhonemeClassification
  { diphthongQuality :: DiphthongQualityClassification
  , vowellength :: VowelLengthClassification
  , roundedlip :: RoundedLipClassification
  , toungueposition :: TounguePositionClassification
  , breathing :: BreathingClassification
  , diacritic :: DiacriticClassification
  }

data DiphthongQualityClassification = DiphthongQualityClassification
  { genuine :: }

data VocalChordsClassification = VocalChordsClassification
  { voiceless :: Cited [ConsonantPhoneme]
  , voiced :: Cited [ConsonantPhoneme]
  }

data PartOfMouthClassification = PartOfMouthClassification
  { labial :: Cited [ConsonantPhoneme]
  , lingual :: Cited [ConsonantPhoneme]
  , velar :: Cited [ConsonantPhoneme]
  , dental :: Cited [ConsonantPhoneme]
  , gutteral :: Cited [ConsonantPhoneme]
  }

data AirFlowClassification = AirFlowClassification
  { stop :: Cited [ConsonantPhoneme]
  , fricative :: Cited [ConsonantPhoneme]
  , affricate :: Cited [ConsonantPhoneme]
  , aspirate :: Cited [ConsonantPhoneme]
  , liquid :: Cited [ConsonantPhoneme]
  , nasal :: Cited [ConsonantPhoneme]
  , sibilant :: Cited [ConsonantPhoneme]
  }
  smythdiphthongquality :: 



diphthong a short vowel followed by Iota | Upsilon smyth § "5"
initial Upsilon always has Rough smyth § "10"
every intitla Rho has rough Breathing
medial Rho Rho is written Rho rough Rho smooth

shortvowel shortvowel = long
short short = diphthong

______________________________
data DiacriticClassification = DiacriticClassification 
	{Circumflex :: Cited [VowelPhoneme]
	, Diaeresis :: Cited [VowelPhoneme]
	}

data BreathingClassification = BreathingClassification
	{Rough :: Cited [VowelPhoneme]
	,Smooth :: Cited [VowelPhoneme]
	}

