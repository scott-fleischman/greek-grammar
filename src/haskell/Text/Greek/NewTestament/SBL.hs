{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.NewTestament.SBL where

import Prelude (($), (.), Eq((==)), Show, otherwise, (||), Bool(..))
import Control.Applicative ((<$>), (<*>))
import Control.Lens ((&), (^?), (^..), (^.), (%~), (.~), Traversal', makeLenses)
import Control.Lens.Tuple (_1)
import Control.Monad (return)
import Control.Monad.State.Lazy (State, evalState, state, runState)
import Data.Char (isPunctuation, isSpace)
import Data.Either (Either(Left, Right))
import Data.Foldable (all, concat)
import Data.Functor (fmap)
import Data.List (elem)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Text (Text, unpack)
import Data.Traversable (sequence)
import Text.Greek.NewTestament.Bible (Bible(Bible), Book(Book), Segment(..), Milestone(Start,End), Paragraph(..), Chapter(..), Verse(..))
import Text.XML (elementName, Node (NodeElement, NodeContent), Name (Name), Document, Element)
import Text.XML.Lens ((./), attr, el, root, text, attributeIs, nodes)

data SBLError =
    ChapterWithinVerse MilestoneState Element
  | ContentOutsideOfMilestone MilestoneState Node
  | MismatchedEndChapter MilestoneState Element
  | MismatchedEndVerse MilestoneState Element
  | MissingBookOsisID
  | MissingBookTitle
  | MissingOsisIDWork
  | MissingOsisTitle
  | NestedChapter MilestoneState Element
  | NestedParagraph MilestoneState Element
  | NestedVerse MilestoneState Element
  | UnexpectedContentInVerse MilestoneState Node
  | UnexpectedNode MilestoneState Node
  | UnmatchedEndChapter MilestoneState Element
  | UnmatchedEndVerse MilestoneState Element
  | VerseOutsideChapter MilestoneState Element
  | WordOutsideMilestone MilestoneState Element
  deriving (Eq, Show)

osisNamespace :: Text
osisNamespace = "http://www.bibletechnologies.net/2003/OSIS/namespace"

osisName :: Text -> Name
osisName n = Name n (Just osisNamespace) Nothing

elo :: Text -> Traversal' Element Element
elo n = el (osisName n)

osisText :: Traversal' Document Element
osisText = root . elo "osis" ./ elo "osisText"

whenNothing :: a -> Maybe b -> Either a b
whenNothing _ (Just b) = Right b
whenNothing a Nothing = Left a

data MilestoneState = MilestoneState
  { _paragraphState :: Maybe Paragraph
  , _chapterState :: Maybe Chapter
  , _verseState :: Maybe Verse
  }
  deriving (Eq, Show)
makeLenses ''MilestoneState

emptyMilestoneState :: MilestoneState
emptyMilestoneState = MilestoneState Nothing Nothing Nothing

makeSegment :: Node -> State MilestoneState (Either SBLError [Segment])
makeSegment (NodeElement e)
  | eName "p" = state $ \s ->
    case s ^. paragraphState of
      Nothing ->
        ( concat <$> sequence
          [ Right $ [SegmentParagraph $ Start Paragraph]
          , es
          , Right [SegmentParagraph $ End Paragraph]
          ]
        , s' & paragraphState .~ Nothing
        )
        where (es, s') = runMakeSegments e (s & paragraphState .~ Just Paragraph)
      Just _ -> (Left $ NestedParagraph s e, s)

  | eName "title" = return $ Right [SectionTitle $ e ^. text]

  | eName "chapter"
  , Just sid <- e ^? attr "sID"
  = state $ \s ->
    case (s ^. chapterState, s ^. verseState) of
      (Nothing, Nothing) -> (Right [SegmentChapter $ Start $ Chapter sid], s & chapterState .~ (Just $ Chapter sid))
      (Nothing, Just _) -> (Left $ ChapterWithinVerse s e, s)
      (Just _, _) -> (Left $ NestedChapter s e, s)
  | eName "chapter"
  , Just eid <- e ^? attr "eID"
  = state $ \s ->
    case s ^. chapterState of
      Nothing -> (Left $ UnmatchedEndChapter s e, s)
      Just (Chapter c) -> case c == eid of
        True -> (Right [SegmentChapter $ End $ Chapter eid], s & chapterState .~ Nothing)
        False -> (Left $ MismatchedEndChapter s e, s)

  | eName "verse"
  , Just sid <- e ^? attr "sID"
  = state $ \s ->
    case (s ^. chapterState, s ^. verseState) of
      (Nothing, _) -> (Left $ VerseOutsideChapter s e, s)
      (Just _, Nothing) -> (Right [SegmentVerse $ Start $ Verse sid], s & verseState .~ (Just $ Verse sid))
      (Just _, Just _) -> (Left $ NestedVerse s e, s)
  | eName "verse"
  , Just eid <- e ^? attr "eID"
  = state $ \s ->
    case (s ^. chapterState, s ^. verseState) of
      (Nothing, _) -> (Left $ VerseOutsideChapter s e, s)
      (Just _, Nothing) -> (Left $ UnmatchedEndVerse s e, s)
      (Just _, Just (Verse v)) -> case v == eid of
        True -> (Right [SegmentVerse $ End $ Verse eid], s & verseState .~ Nothing)
        False -> (Left $ MismatchedEndVerse s e, s)

  | eName "w" = state $ \s ->
    case s of
      MilestoneState (Just _) (Just _) (Just _) -> (Right [Word $ e ^. text], s)
      MilestoneState _ _ _ -> (Left $ WordOutsideMilestone s e, s)

  | otherwise = return $ Right []
  where
    eName t = elementName e == osisName t
makeSegment n@(NodeContent c) = state $ \s ->
  case s of
    MilestoneState (Just _) (Just _) (Just _) ->
      case all (\x -> isSpace x || isPunctuation x || elem x "12") $ unpack c of
        True -> (Right [Separator c], s)
        False -> (Left $ UnexpectedContentInVerse s n, s)
    MilestoneState _ _ _ ->
      case all isSpace $ unpack c of
        True -> (Right [], s)
        False -> (Left $ ContentOutsideOfMilestone s n, s)
makeSegment n = state $ \s -> (Left $ UnexpectedNode s n, s)

runMakeSegments :: Element -> MilestoneState -> (Either SBLError [Segment], MilestoneState)
runMakeSegments e s = result & _1 %~ (fmap concat . sequence)
  where
    result = runState (sequence $ makeSegment <$> e ^. nodes) s

evalMakeSegments :: Element -> MilestoneState -> Either SBLError [Segment]
evalMakeSegments e s = fmap concat $ sequence $ evalState (sequence $ makeSegment <$> e ^. nodes) s

makeBook :: Element -> Either SBLError Book
makeBook e = Book <$> id <*> title <*> segments
  where
    id = whenNothing MissingBookOsisID $ e ^? attr "osisID"
    title = whenNothing MissingBookTitle $ e ^? ($) ./ elo "title" . text
    segments = evalMakeSegments e emptyMilestoneState

loadOsis :: Document -> Either SBLError Bible
loadOsis doc = Bible <$> id <*> title <*> books
  where
    id = whenNothing MissingOsisIDWork $ doc ^? osisText . attr "osisIDWork"
    title = whenNothing MissingOsisTitle $ doc ^? osisText ./ elo "header" ./ elo "work" ./ elo "title" . text
    books = sequence $ makeBook <$> doc ^.. osisText ./ elo "div" . attributeIs "type" "book"
