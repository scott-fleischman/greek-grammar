{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Greek.Source.All where

import Prelude hiding (Word, words)
import Text.Greek.Xml.Parse
import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Lazy as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Format as Format
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Unicode.Decompose as Decompose
import qualified Text.Greek.IO.Paths as Paths
import qualified Text.Greek.Script.Word as Word
import qualified Text.Greek.Source.Sblgnt as SBL
import qualified Text.Greek.Source.Morphgnt as Morphgnt
import qualified Text.Greek.Source.Work as Work

loadAll :: Except.ExceptT String IO [Work.Indexed [Word.Word Word.Basic Word.SourceInfo]]
loadAll = loadAll'

loadAll' :: Except.ExceptT String IO [Work.Indexed [Word.Word Word.Basic Word.SourceInfo]]
loadAll' = do
  _ <- Except.liftIO $ putStrLn "Loading morphgnt"
  morphgnt <- Morphgnt.load
  _ <- Except.liftIO $ putStrLn . show . length . Morphgnt.morphgntBooks $ morphgnt

  _ <- Except.liftIO $ putStrLn "Loading SBLGNT"
  sblgntWorks <- loadSblgnt
  _ <- Except.liftIO $ putStrLn . show . length $ sblgntWorks

  let allWorks = alignMorphgnt morphgnt sblgntWorks
  validateMorphgntAlignment allWorks
  let indexedWorks = Work.indexBasic allWorks
  let indexedWorksWords = Lens.over (traverse . Work.content) Word.addIndex indexedWorks
  return indexedWorksWords

alignMorphgnt :: Morphgnt.Morphgnt
  -> [Work.Basic [Word.Word (Word.Affix, Word.ParagraphIndex, Word.Verse) Word.SourceInfo]]
  -> [Work.Basic [Word.Word (Word.Affix, Word.ParagraphIndex, Word.Verse, Morphgnt.Word) Word.SourceInfo]]
alignMorphgnt morphgnt sblgntWorks = alignedWords
  where
    alignedWorks ::
      [
        ( Morphgnt.Book
        , Work.Basic
          [ Word.Word
            (Word.Affix, Word.ParagraphIndex, Word.Verse)
            Word.SourceInfo
          ]
        )
      ]
    alignedWorks = zip (Morphgnt.morphgntBooks morphgnt) sblgntWorks

    alignedWords ::
      [ Work.Basic
        [ Word.Word
          (Word.Affix, Word.ParagraphIndex, Word.Verse, Morphgnt.Word)
          Word.SourceInfo
        ]
      ]
    alignedWords = fmap (\(m, w) -> alignWords (Morphgnt.bookWords m) w) alignedWorks

    alignWords
      :: [ Morphgnt.Word ]
      -> Work.Basic [Word.Word (Word.Affix, Word.ParagraphIndex, Word.Verse) Word.SourceInfo]
      -> Work.Basic [Word.Word (Word.Affix, Word.ParagraphIndex, Word.Verse, Morphgnt.Word) Word.SourceInfo]
    alignWords ms w = Lens.over Work.content (\c -> fmap combineMorphgntWord . zip ms $ c) w

combineMorphgntWord ::
    ( Morphgnt.Word
    , Word.Word (Word.Affix, Word.ParagraphIndex, Word.Verse) Word.SourceInfo
    )
  -> Word.Word (Word.Affix, Word.ParagraphIndex, Word.Verse, Morphgnt.Word) Word.SourceInfo
combineMorphgntWord (m, w) = Lens.over Word.info (\(a,b,c) -> (a,b,c,m)) w

validateMorphgntAlignment :: [Work.Basic [Word.Word (Word.Affix, Word.ParagraphIndex, Word.Verse, Morphgnt.Word) Word.SourceInfo]]
  -> Except.ExceptT String IO ()
validateMorphgntAlignment alignedWords = do
  let
    filtered
      = filter (\(x, y, _) -> x /= y)
      . fmap
        (\x ->
          ( Text.pack . Decompose.decompose . Morphgnt.wordStripped . Lens.view (Word.info . Lens._4) $ x
          , Text.pack . Decompose.decompose . Text.unpack . Word.getSource . Word.getSourceInfoWord . Word.getSurface $ x
          , Word.getSourceInfoFile . Word.getSurface $ x
          )
        )
      . concatMap (\x -> Work.getContent x)
      $ alignedWords
  _ <- Except.liftIO $
    mapM_ (\(m, s, l) -> Text.putStrLn . Lazy.toStrict . Format.format "{} {} {}" $ (Format.Shown l, m, s))
    filtered
  Monad.when (length filtered /= 0) (Except.throwError "morphgnt and sblgnt don't align")

loadSblgnt :: Except.ExceptT String IO [Work.Basic [Word.Word (Word.Affix, Word.ParagraphIndex, Word.Verse) Word.SourceInfo]]
loadSblgnt = do
  sbl <- Except.liftIO $ readParseEvents SBL.sblgntParser Paths.sblgntXmlPath
  case sbl of
    Left e -> Except.throwError . show $ e
    Right s -> return $ sblgntToWorks s

sblgntToWorks :: SBL.Sblgnt -> [Work.Basic [Word.Word (Word.Affix, Word.ParagraphIndex, Word.Verse) Word.SourceInfo]]
sblgntToWorks (SBL.Sblgnt _ _ bs) = fmap sblgntBookToWork bs

sblgntBookToWork :: SBL.Book [SBL.Item] -> Work.Basic [Word.Word (Word.Affix, Word.ParagraphIndex, Word.Verse) Word.SourceInfo]
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

sblWordToWord :: Word.ParagraphIndex -> (Word.Verse, SBL.Word) -> Word.Word (Word.Affix, Word.ParagraphIndex, Word.Verse) Word.SourceInfo
sblWordToWord i (v, SBL.Word (t, f) p s) =
  Word.Word
  ((unifiedPrefix, unifiedSuffix), i, v)
  (Word.SourceInfo (Word.Source t) f)
  where
    unifiedPrefix = p >>= Word.makePrefix . SBL.stripSigla
    unifiedSuffix = Word.makeSuffix . SBL.stripSigla $ s
