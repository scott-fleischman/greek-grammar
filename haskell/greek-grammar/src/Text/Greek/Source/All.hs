{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Source.All where

import Prelude hiding (Word)
import Control.Lens
import Data.Text (Text)
import Text.Greek.FileReference
import Text.Greek.Paths
import Text.Greek.Script.Elision
import Text.Greek.Xml.Common
import Text.Greek.Xml.Parse
import qualified Text.Greek.Source.Sblgnt as SBL

type BasicWordText = BasicWord (Text, FileReference)
data BasicWord a = BasicWord
  { wordSurface :: a
  , wordElision :: Maybe (ElisionChar, FileCharReference)
  } deriving Show

data WorkSource = Sblgnt deriving Show

type WorkText = Work [BasicWordText]
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

sblgntParagraphsToWords :: Foldable t => t SBL.BookParagraph -> [BasicWordText]
sblgntParagraphsToWords = fmap sblWordToWord . concatMap (toListOf SBL._ItemWord) . concat . concatMap (toListOf SBL._BookParagraphContent)

sblWordToWord :: SBL.Word -> BasicWordText
sblWordToWord (SBL.Word s e _ _) = BasicWord s e
