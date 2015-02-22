{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Text.Greek.NewTestament.SBL where

import Prelude (($), (.), Eq((==)), Show, otherwise, (||))
import Control.Applicative ((<$>), (<*>))
import Control.Lens ((^?), (^..), (^.), Traversal')
import Data.Char (isPunctuation, isSpace)
import Data.Either (Either(Left, Right))
import Data.Foldable (all, concat)
import Data.List (elem)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Text (Text, unpack)
import Data.Traversable (sequence)
import Text.Greek.NewTestament.Bible (Bible(Bible), Book(Book), Segment(..), Milestone(Start,End), Paragraph(..), Chapter(..), Verse(..))
import Text.XML (elementName, Node (NodeElement, NodeContent), Name (Name), Document, Element)
import Text.XML.Lens ((./), attr, el, root, text, attributeIs, nodes)

data SBLError =
    MissingOsisIDWork
  | MissingOsisTitle
  | MissingBookOsisID
  | MissingBookTitle
  | UnexpectedElement ParagraphState Element
  | UnexpectedNode ParagraphState Node
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

data ParagraphState = InsideParagraph | OutsideParagraph
  deriving (Eq, Show)

makeSegment :: ParagraphState -> Node -> Either SBLError [Segment]
makeSegment ps (NodeElement e)
  | eName "p", OutsideParagraph <- ps = concat <$> sequence
    [ Right $ [SegmentParagraph $ Start Paragraph]
    , concat <$> (sequence $ makeSegment InsideParagraph <$> e ^. nodes)
    , Right [SegmentParagraph $ End Paragraph]
    ]
  | eName "title" = Right [SectionTitle $ e ^. text]
  | eName "chapter", Just sid <- e ^? attr "sID" = Right [SegmentChapter $ Start $ Chapter sid]
  | eName "chapter", Just eid <- e ^? attr "eID" = Right [SegmentChapter $ End $ Chapter eid]
  | eName "verse", Just sid <- e ^? attr "sID" = Right [SegmentVerse $ Start $ Verse sid]
  | eName "verse", Just eid <- e ^? attr "eID" = Right [SegmentVerse $ End $ Verse eid]
  | eName "w", InsideParagraph <- ps = Right [Word $ e ^. text]
  | otherwise = Left $ UnexpectedElement ps e
  where
    eName t = elementName e == osisName t
makeSegment ps n@(NodeContent c)
  | OutsideParagraph <- ps, all isSpace $ unpack c = Right $ [Separator c]
  | InsideParagraph <- ps, all (\x -> isSpace x || isPunctuation x || elem x "12") $ unpack c = Right $ [Separator c]
  | otherwise = Left $ UnexpectedNode ps n
makeSegment ps n = Left $ UnexpectedNode ps n

makeBook :: Element -> Either SBLError Book
makeBook e = Book <$> id <*> title <*> segments
  where
    id = whenNothing MissingBookOsisID $ e ^? attr "osisID"
    title = whenNothing MissingBookTitle $ e ^? ($) ./ elo "title" . text
    segments = concat <$> (sequence $ makeSegment OutsideParagraph <$> e ^. nodes)

loadOsis :: Document -> Either SBLError Bible
loadOsis doc = Bible <$> id <*> title <*> books
  where
    id = whenNothing MissingOsisIDWork $ doc ^? osisText . attr "osisIDWork"
    title = whenNothing MissingOsisTitle $ doc ^? osisText ./ elo "header" ./ elo "work" ./ elo "title" . text
    books = sequence $ makeBook <$> doc ^.. osisText ./ elo "div" . attributeIs "type" "book"
