{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Mounce.Noun where

import Control.Lens
import Data.Text (Text, append)
import Text.Greek.Grammar
import Text.Greek.Mounce.Morphology
import Text.Greek.Mounce.NounFirstDeclension
import Text.Greek.Mounce.NounSecondDeclension
import Text.Greek.Mounce.NounThirdDeclension

allNounFormsList :: [NounForm]
allNounFormsList =
     listToNounForms "1st declension : " firstDeclensionNouns
  ++ listToNounForms "2nd declension : " secondDeclensionNouns
  ++ listToNounForms "3rd declension : " thirdDeclensionNouns

listToNounForms :: Text -> [Cited NounCategory] -> [NounForm]
listToNounForms namePrefix = fmap (& nounFormCategoryName %~ (append namePrefix)) . concat . fmap nounCategoryToAllForms . fmap (^. item)
