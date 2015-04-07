{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (readFile)
import Data.Char
import Data.Default (def)
import Data.Either
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple
import Numeric
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit
import Text.Greek.Conversions
import Text.Greek.Corpus.Bible
import Text.Greek.NewTestament.SBL
import Text.Greek.Paths
import Text.Greek.Script
import Text.Greek.Script.Sound
import Text.XML (readFile)

case_valid_tokens = do
  mapM_ (\p -> assertEqual (showString "'\\x" . showHex (ord . fst $ p) $ "'") [] (validateToken . snd $ p)) unicodeTokenPairs

case_load_sblgnt = do
  sblgnt <- readFile def sblgntOsisPath
  isRight (loadOsis sblgnt) @?= True

case_sound_a_vowel = True @=? case textToSounds "α" of { VowelSound (Vowel _) : [] -> True ; _ -> False }
case_sound_b_consonant = True @=? case textToSounds "β" of { ConsonantSound (Consonant _) : [] -> True ; _ -> False }
case_sound_ai_diphthong = True @=? case textToSounds "αι" of { VowelSound (Diphthong _ _) : [] -> True ; _ -> False }
case_sound_ai_diaeresis = True @=? case textToSounds "αϊ" of { VowelSound (Vowel _) : VowelSound (Vowel _) : [] -> True ; _ -> False }
case_sound_ai_iotaSubscript = True @=? case textToSounds "ᾳ" of { VowelSound (ImproperDiphthong _) : [] -> True ; _ -> False }
case_sound_a_rough = True @=? case textToSounds "ἁ" of { ConsonantSound (RoughBreathing _) : VowelSound (Vowel _) : [] -> True ; _ -> False }
case_sound_ai_iotaSubscript_rough = True @=? case textToSounds "ᾁ" of { ConsonantSound (RoughBreathing _) : VowelSound (ImproperDiphthong _) : [] -> True ; _ -> False }
case_sound_ai_diphthong_rough = True @=? case textToSounds "αἱ" of { ConsonantSound (RoughBreathing _) : VowelSound (Diphthong _ _) : [] -> True ; _ -> False }

main :: IO ()
main = $(defaultMainGenerator)
