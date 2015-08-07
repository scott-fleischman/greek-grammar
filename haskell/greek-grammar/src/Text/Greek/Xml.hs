{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Text.Greek.Xml where

import Prelude hiding ((*), (+), log, FilePath)
import Conduit
import Control.Lens
import Data.Text (Text)
import Text.Greek.Utility
import qualified Data.Conduit.Attoparsec as X
import qualified Data.XML.Types as X
import qualified Text.XML.Stream.Parse as X
import qualified Prelude as X (FilePath)

newtype Line = Line { getLine :: Int } deriving (Eq, Ord, Show)
newtype Column = Column { getColumn :: Int } deriving (Eq, Ord, Show)
newtype FilePath = FilePath { getFilePath :: X.FilePath } deriving (Eq, Ord, Show)

type LineReference = Line * Column
type LineReferenceRange = LineReference + LineReference * LineReference
type FileReference = FilePath * LineReferenceRange

type EventAll
  = EventBeginDoctype * Text * (Maybe XmlExternalId)
  + EventEndDoctype
  + EventInstruction * XmlInstruction
  + EventBeginElement * XmlName * XmlAttributes
  + EventEndElement * XmlName
  + EventContent * XmlContent
  + EventComment * Text
  + EventCDATA * Text

readEvents :: X.FilePath -> IO ([ErrorMessage] + [FileReference * EventAll])
readEvents p = fmap xmlTransform . fmap (p *) . readEventsConduit $ p

xmlTransform :: X.FilePath * [Maybe X.PositionRange * X.Event]
  -> [ErrorMessage] + [FileReference * EventAll]
xmlTransform x = return x
  >>. tx1
  >>. tx1a
  >>. tx1b
  >>. tx2
  >>= tx3
  >>= tx4
  >>= tx5
  >>= tx6
  >>. tx7
  >>= tx8


readEventsConduit :: X.FilePath -> IO [(Maybe X.PositionRange, X.Event)]
readEventsConduit p = runResourceT $ sourceFile p =$= X.parseBytesPos X.def $$ sinkList


toLineReference :: X.Position -> LineReference
toLineReference (X.Position line column) = Line line * Column column

toLineReferenceRange :: X.PositionRange -> LineReferenceRange
toLineReferenceRange (X.PositionRange start end)
  | start == end = sum1  (toLineReference start)
  | otherwise    = sum2e (toLineReference start * toLineReference end)


data EventBeginDocument = EventBeginDocument deriving (Eq, Ord, Show)
data EventEndDocument = EventEndDocument deriving (Eq, Ord, Show)
data EventBeginDoctype = EventBeginDoctype deriving (Eq, Ord, Show)
data EventEndDoctype = EventEndDoctype deriving (Eq, Ord, Show)
data EventInstruction = EventInstruction deriving (Eq, Ord, Show)
data EventBeginElement = EventBeginElement deriving (Eq, Ord, Show)
data EventEndElement = EventEndElement deriving (Eq, Ord, Show)
data EventContent = EventContent deriving (Eq, Ord, Show)
data EventComment = EventComment deriving (Eq, Ord, Show)
data EventCDATA = EventCDATA deriving (Eq, Ord, Show)
data XmlNameId = XmlNameId deriving (Eq, Ord, Show)

type XmlName = XmlNameId * Text * Maybe Text * Maybe Text
toXmlName :: X.Name -> XmlName
toXmlName (X.Name a b c) = XmlNameId * a * b * c

data XmlContentText = XmlContentText deriving (Eq, Ord, Show)
data XmlContentEntity = XmlContentEntity deriving (Eq, Ord, Show)

type XmlContent
  = XmlContentText * Text
  + XmlContentEntity * Text
toXmlContent :: X.Content -> XmlContent
toXmlContent (X.ContentText a)   = sum1  (XmlContentText * a)
toXmlContent (X.ContentEntity a) = sum2e (XmlContentEntity * a)

data XmlInstructionId = XmlInstructionId deriving (Eq, Ord, Show)

type XmlInstruction = XmlInstructionId * Text * Text
toXmlInstruction :: X.Instruction -> XmlInstruction
toXmlInstruction (X.Instruction a b) = XmlInstructionId * a * b

data XmlSystemId = XmlSystemId deriving (Eq, Ord, Show)

data XmlPublicId = XmlPublicId deriving (Eq, Ord, Show)

type XmlExternalId
  = XmlSystemId * Text
  + XmlPublicId * Text * Text
toXmlExternalId :: X.ExternalID -> XmlExternalId
toXmlExternalId (X.SystemID a)   = sum1  (XmlSystemId * a)
toXmlExternalId (X.PublicID a b) = sum2e (XmlPublicId * a * b)

type XmlAttributes = [XmlName * [XmlContent]]
toXmlAttributes :: [X.Name * [X.Content]] -> [XmlName * [XmlContent]]
toXmlAttributes = over (each . _1) toXmlName . over (each . _2 . each) toXmlContent

type EventExtra
  = EventBeginDocument
  + EventEndDocument
  + EventBeginDoctype * Text * (Maybe XmlExternalId)
  + EventEndDoctype
  + EventInstruction * XmlInstruction
  + EventBeginElement * XmlName * XmlAttributes
  + EventEndElement * XmlName
  + EventContent * XmlContent
  + EventComment * Text
  + EventCDATA * Text

toEventExtra :: X.Event -> EventExtra
toEventExtra  X.EventBeginDocument     = sum1    EventBeginDocument
toEventExtra  X.EventEndDocument       = sum2    EventEndDocument
toEventExtra (X.EventBeginDoctype t e) = sum3   (EventBeginDoctype * t * fmap toXmlExternalId e)
toEventExtra  X.EventEndDoctype        = sum4    EventEndDoctype
toEventExtra (X.EventInstruction i)    = sum5   (EventInstruction * toXmlInstruction i)
toEventExtra (X.EventBeginElement n a) = sum6   (EventBeginElement * toXmlName n * toXmlAttributes a)
toEventExtra (X.EventEndElement n)     = sum7   (EventEndElement * toXmlName n)
toEventExtra (X.EventContent c)        = sum8   (EventContent * toXmlContent c)
toEventExtra (X.EventComment t)        = sum9   (EventComment * t)
toEventExtra (X.EventCDATA t)          = sum10e (EventCDATA * t)


tx1 :: a * [b * X.Event]
    -> a * [b * EventExtra]
tx1 = _2 . each . _2 %~ toEventExtra

tx1a :: a * [Maybe X.PositionRange    * b]
     -> a * [Maybe LineReferenceRange * b]
tx1a = _2 . each . _1 . _Just %~ toLineReferenceRange

tx1b :: X.FilePath * a
     ->   FilePath * a
tx1b = _1 %~ FilePath

tx2 ::   a * [b  * c]
    -> [(a *  b) * c]
tx2 (c, xs) = (shiftLeftProduct . (*) c) <$> xs

tx3 :: Handler e [a * EventExtra] =>
                 [a * EventExtra]
    -> [e] +     [a * EventExtra]
tx3 = removePrefixWith handle (^. _2) [sum1 EventBeginDocument]

type Event9
  = EventEndDocument
  + EventBeginDoctype * Text * (Maybe XmlExternalId)
  + EventEndDoctype
  + EventInstruction * XmlInstruction
  + EventBeginElement * XmlName * XmlAttributes
  + EventEndElement * XmlName
  + EventContent * XmlContent
  + EventComment * Text
  + EventCDATA * Text

tx4 :: Handler e (a * EventExtra) =>
                 [a * EventExtra]
    -> [e] +     [a * Event9]
tx4 = partialMap _2 tryDrop1

tx5 :: Handler e [a * Event9] =>
                 [a * Event9]
    -> [e] +     [a * Event9]
tx5 = removeSuffixWith handle (^. _2) [sum1 EventEndDocument]

tx6 :: Handler e (a * Event9) =>
                 [a * Event9]
    -> [e] +     [a * EventAll]
tx6 = partialMap _2 tryDrop1

tx7 :: [(a * Maybe b)  * c]
    -> [(a * (() + b)) * c]
tx7 = each . _1 . _2 %~ maybeToEither ()

tx8 :: Handler e ((FilePath * (() + LineReferenceRange)) * a) =>
                 [(FilePath * (() + LineReferenceRange)) * a]
    -> [e] +     [FileReference                          * a]
tx8 = partialMap (_1 . _2) tryDrop1
