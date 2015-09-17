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

type BasicTextWord = BasicWord (Text, FileReference)
data BasicWord a = BasicWord
  { wordSurface :: a
  , wordElision :: Maybe (ElisionChar, FileCharReference)
  } deriving Show

type TextWork = Work [BasicTextWord]
data Work a = Work
  { workSource :: WorkSource
  , workTitle :: Text
  , workContent :: a
  } deriving Show

data WorkSource = Sblgnt deriving Show

loadAll :: IO (Either [XmlError] [TextWork])
loadAll = loadSblgnt

loadSblgnt :: IO (Either [XmlError] [TextWork])
loadSblgnt = (fmap . fmap) sblgntToWorks $ readParseEvents SBL.sblgntParser sblgntXmlPath

sblgntToWorks :: SBL.Sblgnt -> [TextWork]
sblgntToWorks (SBL.Sblgnt _ _ bs) = fmap sblgntBookToWork bs

sblgntBookToWork :: SBL.Book -> TextWork
sblgntBookToWork (SBL.Book _ t ps) = Work Sblgnt t (sblgntParagraphsToWords ps)

sblgntParagraphsToWords :: Foldable t => t SBL.BookParagraph -> [BasicTextWord]
sblgntParagraphsToWords = fmap sblWordToWord . concatMap (toListOf SBL._ItemWord) . concat . concatMap (toListOf SBL._BookParagraphContent)

sblWordToWord :: SBL.Word -> BasicTextWord
sblWordToWord (SBL.Word s e _ _) = BasicWord s e
