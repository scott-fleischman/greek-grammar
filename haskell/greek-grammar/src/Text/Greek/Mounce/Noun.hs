{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Mounce.Noun where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text, append)
import Text.Greek.Grammar
import Text.Greek.Mounce.Morphology
import Text.Greek.Mounce.NounFirstDeclension
import Text.Greek.Mounce.NounSecondDeclension
import Text.Greek.Mounce.NounThirdDeclension
import Text.Greek.Script.Sound

allNounFormsMap :: Map [Sound] [NounForm]
allNounFormsMap = M.fromListWith (++) formPairs
  where
    formPairs :: [([Sound], [NounForm])]
    formPairs = makeFormPair <$> allNounFormsList

    makeFormPair :: NounForm -> ([Sound], [NounForm])
    makeFormPair nf = (fmap stripEnding $ nf ^. nounFormSounds, pure nf)

allNounFormsList :: [NounForm]
allNounFormsList =
     listToNounForms "1st declension : " firstDeclensionNouns
  ++ listToNounForms "2nd declension : " secondDeclensionNouns
  ++ listToNounForms "3rd declension : " thirdDeclensionNouns

listToNounForms :: Text -> [Cited NounCategory] -> [NounForm]
listToNounForms namePrefix = fmap (& nounFormCategoryName %~ (append namePrefix)) . concat . fmap nounCategoryToAllForms . fmap (^. item)
