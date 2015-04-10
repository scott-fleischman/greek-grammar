module Text.Greek.Morphology.Noun where

import Text.Greek.Phonology.Phoneme
import Text.Greek.Phonology.Shorthand

data NounInflection = NounInflection Case Gender Number

data Case = Nominative | Genitive | Dative | Accusative | Vocative

data Gender = Feminine | Masculine | Neuter

data Number = Singular | Plural | Dual

data Noun =
    FirstDeclension
  | SecondDeclension Gender
  | ThirdDeclension


data OptionNomSg = OptionNomSg_s | OptionNomSg_null
data OptionGenSg = OptionGenSg_s | OptionGenSg_io
data OptionDatPl = OptionDatPl_is | OptionDatPl_isi

-- smyth 210
vowelDeclension_Nom_Sg_MascFem :: OptionNomSg -> [Phoneme]
vowelDeclension_Nom_Sg_MascFem OptionNomSg_s = [σ]
vowelDeclension_Nom_Sg_MascFem OptionNomSg_null = []

vowelDeclension_Nom_Sg_Neut :: [Phoneme]
vowelDeclension_Nom_Sg_Neut = [ν]

vowelDeclension_Gen_Sg :: OptionGenSg -> [Phoneme]
vowelDeclension_Gen_Sg OptionGenSg_s = [σ]
vowelDeclension_Gen_Sg OptionGenSg_io = [ι', ο]

vowelDeclension_Dat_Sg :: [Phoneme]
vowelDeclension_Dat_Sg = [ῐ]

vowelDeclension_Acc_Sg :: [Phoneme]
vowelDeclension_Acc_Sg = [ν]

vowelDeclension_Voc_Sg_MascFem :: [Phoneme]
vowelDeclension_Voc_Sg_MascFem = []


secondDeclensionThematicVowel = ο
secondDeclensionThematicVowel' = ε

secondDeclension :: Gender -> Number -> Case -> [Phoneme]
secondDeclension Masculine Singular Nominative  = secondDeclensionThematicVowel : vowelDeclension_Nom_Sg_MascFem OptionNomSg_s
secondDeclension Masculine Singular Genitive    = secondDeclensionThematicVowel : vowelDeclension_Gen_Sg OptionGenSg_io
secondDeclension Masculine Singular Dative      = secondDeclensionThematicVowel : vowelDeclension_Dat_Sg
secondDeclension Masculine Singular Accusative  = secondDeclensionThematicVowel : vowelDeclension_Acc_Sg
secondDeclension _ _ _ = []
