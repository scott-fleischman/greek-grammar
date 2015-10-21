{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

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

data WorkSource = Sblgnt deriving (Eq, Ord, Show)

newtype WorkTitle = WorkTitle Text deriving (Eq, Ord, Show)

type WorkText = Work [Word.BasicText]
data Work a = Work
  { _workSource :: WorkSource
  , _workTitle :: WorkTitle
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
sblgntBookToWork (SBL.Book _ t ps) = Work Sblgnt (WorkTitle t) (sblgntParagraphsToWords ps)

sblgntParagraphsToWords :: Foldable t => t SBL.BookParagraph -> [Word.BasicText]
sblgntParagraphsToWords = fmap sblWordToWord . concatMap (toListOf SBL._ItemWord) . concat . concatMap (toListOf SBL._BookParagraphContent)

sblWordToWord :: SBL.Word -> Word.BasicText
sblWordToWord (SBL.Word s e _ _) = Word.Basic s e

concatSurface :: (Foldable tw, Foldable tc) => Getter s (tc a) -> Work (tw s) -> [a]
concatSurface l = concatMap (toList . view l) . (view workContent)

globalConcatSurface :: (Foldable twork, Foldable tword, Foldable tc) => Getter s (tc a) -> twork (Work (tword s)) -> [a]
globalConcatSurface = concatMap . concatSurface
