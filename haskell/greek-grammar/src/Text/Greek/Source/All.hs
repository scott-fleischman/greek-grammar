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

loadAll :: IO (Either [XmlError] [Work.WorkText])
loadAll = loadSblgnt

loadSblgnt :: IO (Either [XmlError] [Work.WorkText])
loadSblgnt = (fmap . fmap) sblgntToWorks $ readParseEvents SBL.sblgntParser sblgntXmlPath

sblgntToWorks :: SBL.Sblgnt -> [Work.WorkText]
sblgntToWorks (SBL.Sblgnt _ _ bs) = fmap sblgntBookToWork bs

sblgntBookToWork :: SBL.Book -> Work.WorkText
sblgntBookToWork (SBL.Book _ t ps) = Work.Work info (concatMap (\(i, x) -> sblgntParagraphToWords i x) . addIndex $ ps)
  where
    info = (Work.SourceSblgnt, (Work.Title t))
    addIndex = zip [0..]

sblgntParagraphToWords :: Int -> SBL.BookParagraph -> [Word.BasicText]
sblgntParagraphToWords i = fmap (sblWordToWord i) . concatMap (toListOf SBL._ItemWord) . concat . toListOf SBL._BookParagraphContent

sblWordToWord :: Int -> SBL.Word -> Word.BasicText
sblWordToWord i (SBL.Word s e _ _) = Word.Basic s e i

concatSurface :: (Foldable tw, Foldable tc) => Getter s (tc a) -> Work.Basic (tw s) -> [a]
concatSurface l = concatMap (toList . view l) . (view Work.workContent)

globalConcatSurface :: (Foldable twork, Foldable tword, Foldable tc) => Getter s (tc a) -> twork (Work.Basic (tword s)) -> [a]
globalConcatSurface = concatMap . concatSurface
