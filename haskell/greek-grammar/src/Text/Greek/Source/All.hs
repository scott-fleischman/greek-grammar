{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Greek.Source.All where

import Prelude hiding (Word, words)
import Text.Greek.Xml.Common
import Text.Greek.Xml.Parse
import qualified Control.Lens as Lens
import qualified Control.Monad.State.Lazy as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Text.Greek.IO.Paths as Paths
import qualified Text.Greek.Script.Word as Word
import qualified Text.Greek.Source.Sblgnt as SBL
import qualified Text.Greek.Source.Work as Work

loadAll :: IO (Either [XmlError] [Work.Indexed [Word.Word Word.Basic Word.SourceInfo]])
loadAll = do
  _ <- putStrLn "Loading SBLGNT"
  sblgntResult <- loadSblgnt
  return $ do
    sblgntWorks <- sblgntResult
    let allWorks = sblgntWorks
    let indexedWorks = Work.indexBasic allWorks
    let indexedWorksWords = Lens.over (Lens.each . Work.content) Word.addIndex indexedWorks
    return indexedWorksWords

loadSblgnt :: IO (Either [XmlError] [Work.Basic [Word.Word Word.BasicInfo Word.SourceInfo]])
loadSblgnt = (fmap . fmap) sblgntToWorks $ readParseEvents SBL.sblgntParser Paths.sblgntXmlPath

sblgntToWorks :: SBL.Sblgnt -> [Work.Basic [Word.Word Word.BasicInfo Word.SourceInfo]]
sblgntToWorks (SBL.Sblgnt _ _ bs) = fmap sblgntBookToWork bs

sblgntBookToWork :: SBL.Book [SBL.Item] -> Work.Basic [Word.Word Word.BasicInfo Word.SourceInfo]
sblgntBookToWork (SBL.Book _ t ps) = Work.Work info words
  where
    info = (Work.SourceSblgnt, (Work.Title t))
    words = concatMap (\(i, xs) -> fmap (sblWordToWord i) xs) . applyVerseAllParagraphs $ ps


applyVerseAllParagraphs
  :: [SBL.BookParagraph [SBL.Item]]
  -> [(Word.ParagraphIndex, [(Word.Verse, SBL.Word)])]
applyVerseAllParagraphs ps = zip (fmap Word.ParagraphIndex [0..]) . reverse $ ps'
  where
    ps' = State.evalState topLevelState (Word.Verse 0 "No Verse")
    topLevelState = Foldable.foldlM (flip go) [] ps

    go p wss = do
      v <- State.get
      let (ws, v') = State.runState (applyVerseParagraph p) v
      _ <- State.put v'
      return $ ws : wss

applyVerseParagraph
  :: SBL.BookParagraph [SBL.Item]
  -> State.State Word.Verse [(Word.Verse, SBL.Word)]
applyVerseParagraph p = do
  v <- State.get
  let
    (v', ws)
      = Lens.over Lens._2 reverse
      . List.foldl' (flip go) (v, [])
      . Lens.toListOf (SBL._BookParagraphContent . traverse)
      $ p
  _ <- State.put v'
  return ws
  where
    go (SBL.ItemVerse (SBL.Verse vt _)) (Word.Verse vn _, ws) = (Word.Verse (vn + 1) vt, ws)
    go (SBL.ItemWord w) (v', ws) = (v', (v', w) : ws)

sblWordToWord :: Word.ParagraphIndex -> (Word.Verse, SBL.Word) -> Word.Word Word.BasicInfo Word.SourceInfo
sblWordToWord i (v, SBL.Word (t, f) p s) =
  Word.Word
  ((unifiedPrefix, unifiedSuffix), i, v)
  (Word.SourceInfo (Word.Source t) f)
  where
    unifiedPrefix = p >>= Word.makePrefix . SBL.stripSigla
    unifiedSuffix = Word.makeSuffix . SBL.stripSigla $ s
