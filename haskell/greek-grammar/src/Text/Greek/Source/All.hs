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

data Word = Word
  { wordSurface :: Text
  , wordFileReference :: FileReference
  , wordElision :: Maybe (ElisionChar, FileCharReference)
  } deriving Show

data Work = Work
  { workSource :: WorkSource
  , workTitle :: Text
  , workWords :: [Word]
  } deriving Show

data WorkSource = Sblgnt deriving Show

loadAll :: IO (Either [XmlError] [Work])
loadAll = loadSblgnt

loadSblgnt :: IO (Either [XmlError] [Work])
loadSblgnt = (fmap . fmap) sblgntToWorks $ readParseEvents SBL.sblgntParser sblgntXmlPath

sblgntToWorks :: SBL.Sblgnt -> [Work]
sblgntToWorks (SBL.Sblgnt _ _ bs) = fmap sblgntBookToWork bs

sblgntBookToWork :: SBL.Book -> Work
sblgntBookToWork (SBL.Book _ t ps) = Work Sblgnt t (sblgntParagraphsToWords ps)

sblgntParagraphsToWords :: Foldable t => t SBL.BookParagraph -> [Word]
sblgntParagraphsToWords = fmap sblWordToWord . concatMap (toListOf SBL._ItemWord) . concat . concatMap (toListOf SBL._BookParagraphContent)

sblWordToWord :: SBL.Word -> Word
sblWordToWord (SBL.Word s r e _ _) = Word s r e
