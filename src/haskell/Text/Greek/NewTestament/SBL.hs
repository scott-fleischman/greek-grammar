{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Text.Greek.NewTestament.SBL where

import Prelude (($), (.), Eq((==)), Show, otherwise)
import Control.Applicative ((<$>), (<*>))
import Control.Lens ((^?), (^..), (^.), Traversal')
import Data.Either (Either(Left, Right))
import Data.Foldable (concatMap)
import Data.Maybe (Maybe(Just, Nothing), maybeToList)
import Data.Text (Text)
import Data.Traversable (sequence)
import Text.Greek.NewTestament.Bible (Bible(Bible), Book(Book), Segment(..))
import Text.XML (elementName, Node (NodeElement), Name (Name), Document, Element)
import Text.XML.Lens ((./), attr, el, root, text, attributeIs, nodes)

data SBLError =
    MissingOsisIDWork
  | MissingOsisTitle
  | MissingBookOsisID
  | MissingBookTitle
  | UnknownElement Element
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

makeSegment :: Node -> Either SBLError (Maybe Segment)
makeSegment (NodeElement e)
  | elementName e == osisName "p" = Right $ Just Paragraph
  | elementName e == osisName "title" = Right $ Just $ SectionTitle $ e ^. text
  | elementName e == osisName "chapter" = Right $ Just $ Chapter $ e ^. attr "osisID"
  | elementName e == osisName "verse" = Right $ Just $ Verse $ e ^. attr "osisID"
  | otherwise = Left $ UnknownElement e
makeSegment _ = Right Nothing

makeBook :: Element -> Either SBLError Book
makeBook e = Book <$> id <*> title <*> segments
  where
    id = whenNothing MissingBookOsisID $ e ^? attr "osisID"
    title = whenNothing MissingBookTitle $ e ^? ($) ./ elo "title" . text
    segments = concatMap maybeToList <$> (sequence $ makeSegment <$> e ^. nodes)

loadOsis :: Document -> Either SBLError Bible
loadOsis doc = Bible <$> id <*> title <*> books
  where
    id = whenNothing MissingOsisIDWork $ doc ^? osisText . attr "osisIDWork"
    title = whenNothing MissingOsisTitle $ doc ^? osisText ./ elo "header" ./ elo "work" ./ elo "title" . text
    books = sequence $ makeBook <$> doc ^.. osisText ./ elo "div" . attributeIs "type" "book"
