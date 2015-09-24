{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Source.All where

import Prelude hiding (Word)
import Control.Lens
import Data.Foldable
import Data.Text (Text)
import Text.Greek.Paths
import Text.Greek.Xml.Common
import Text.Greek.Xml.Parse
import qualified Text.Greek.Source.Sblgnt as SBL
import qualified Text.Greek.Script.Word as Word

data WorkSource = Sblgnt deriving Show

type WorkText = Work [Word.BasicText]
data Work a = Work
  { _workSource :: WorkSource
  , _workTitle :: Text
  , _workContent :: a
  } deriving Show
makeLenses ''Work

loadAll :: IO (Either [XmlError] [WorkText])
loadAll = loadSblgnt

loadSblgnt :: IO (Either [XmlError] [WorkText])
loadSblgnt = (fmap . fmap) sblgntToWorks $ readParseEvents SBL.sblgntParser sblgntXmlPath

sblgntToWorks :: SBL.Sblgnt -> [WorkText]
sblgntToWorks (SBL.Sblgnt _ _ bs) = fmap sblgntBookToWork bs

sblgntBookToWork :: SBL.Book -> WorkText
sblgntBookToWork (SBL.Book _ t ps) = Work Sblgnt t (sblgntParagraphsToWords ps)

sblgntParagraphsToWords :: Foldable t => t SBL.BookParagraph -> [Word.BasicText]
sblgntParagraphsToWords = fmap sblWordToWord . concatMap (toListOf SBL._ItemWord) . concat . concatMap (toListOf SBL._BookParagraphContent)

sblWordToWord :: SBL.Word -> Word.BasicText
sblWordToWord (SBL.Word s e _ _) = Word.Basic s e

concatSurface :: (Foldable tw, Foldable tc) => Work (tw (Word.Basic (tc a))) -> [a]
concatSurface = concatMap (toList . view Word.basicSurface) . (view workContent)

globalConcatSurface :: (Foldable twork, Foldable tword, Foldable tc) => twork (Work (tword (Word.Basic (tc a)))) -> [a]
globalConcatSurface = concatMap concatSurface
