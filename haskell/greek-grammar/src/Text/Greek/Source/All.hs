{-# LANGUAGE RankNTypes #-}

module Text.Greek.Source.All where

import Prelude hiding (Word)
import Control.Lens
import Data.Foldable
import Text.Greek.Paths
import Text.Greek.Xml.Common
import Text.Greek.Xml.Parse
import qualified Text.Greek.Script.Word as Word
import qualified Text.Greek.Source.Sblgnt as SBL
import qualified Text.Greek.Source.Work as Work

loadAll :: IO (Either [XmlError] [Work.Indexed [Word.BasicText]])
loadAll = do
  sblgntResult <- loadSblgnt
  return $ do
    sblgntWords <- sblgntResult
    return $ Work.indexBasic sblgntWords

loadSblgnt :: IO (Either [XmlError] [Work.Basic [Word.BasicText]])
loadSblgnt = (fmap . fmap) sblgntToWorks $ readParseEvents SBL.sblgntParser sblgntXmlPath

sblgntToWorks :: SBL.Sblgnt -> [Work.Basic [Word.BasicText]]
sblgntToWorks (SBL.Sblgnt _ _ bs) = fmap sblgntBookToWork bs

sblgntBookToWork :: SBL.Book -> Work.Basic [Word.BasicText]
sblgntBookToWork (SBL.Book _ t ps) = Work.Work info (concatMap (\(i, x) -> sblgntParagraphToWords i x) . addIndex $ ps)
  where
    info = (Work.SourceSblgnt, (Work.Title t))
    addIndex = zip (fmap Word.ParagraphIndex [0..])

sblgntParagraphToWords :: Word.ParagraphIndex -> SBL.BookParagraph -> [Word.BasicText]
sblgntParagraphToWords i = fmap (sblWordToWord i) . concatMap (toListOf SBL._ItemWord) . concat . toListOf SBL._BookParagraphContent

sblWordToWord :: Word.ParagraphIndex -> SBL.Word -> Word.BasicText
sblWordToWord i (SBL.Word s e _ _) = Word.Word (e, i) s

concatSurface :: (Foldable tw, Foldable tc) => Getter s (tc a) -> Work.Basic (tw s) -> [a]
concatSurface l = concatMap (toList . view l) . (view Work.content)

globalConcatSurface :: (Foldable twork, Foldable tword, Foldable tc) => Getter s (tc a) -> twork (Work.Basic (tword s)) -> [a]
globalConcatSurface = concatMap . concatSurface
