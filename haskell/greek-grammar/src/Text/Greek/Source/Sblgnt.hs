{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Greek.Source.Sblgnt where

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

readEvents :: X.FilePath -> IO [(Maybe X.PositionRange, X.Event)]
readEvents p = runResourceT $ sourceFile p =$= X.parseBytesPos X.def $$ sinkList


removePrefixWith :: Eq b => ([a] -> e) -> (a -> b) -> [b] -> [a] -> e + [a]
removePrefixWith e f m as
  | fmap f target == m = Right $ drop matchLength as
  | otherwise          = Left $ e target
  where
    matchLength = length m
    target = take matchLength as

removeSuffixWith :: Eq b => ([a] -> e) -> (a -> b) -> [b] -> [a] -> e + [a]
removeSuffixWith e f m as
  | fmap f reverseTarget == reverse m = Right . reverse . drop matchLength $ reverseList
  | otherwise                         = Left . e . reverse $ reverseTarget
  where
    matchLength = length m
    reverseTarget = take matchLength reverseList
    reverseList = reverse as

maybeToEither :: a -> Maybe b -> a + b
maybeToEither a Nothing = Left a
maybeToEither _ (Just b) = Right b

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

type EventAll
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

toEventAll :: X.Event -> EventAll
toEventAll  X.EventBeginDocument     = sum1    EventBeginDocument
toEventAll  X.EventEndDocument       = sum2    EventEndDocument
toEventAll (X.EventBeginDoctype t e) = sum3   (EventBeginDoctype * t * fmap toXmlExternalId e)
toEventAll  X.EventEndDoctype        = sum4    EventEndDoctype
toEventAll (X.EventInstruction i)    = sum5   (EventInstruction * toXmlInstruction i)
toEventAll (X.EventBeginElement n a) = sum6   (EventBeginElement * toXmlName n * toXmlAttributes a)
toEventAll (X.EventEndElement n)     = sum7   (EventEndElement * toXmlName n)
toEventAll (X.EventContent c)        = sum8   (EventContent * toXmlContent c)
toEventAll (X.EventComment t)        = sum9   (EventComment * t)
toEventAll (X.EventCDATA t)          = sum10e (EventCDATA * t)

type XmlEvent
  = EventBeginElement * XmlName * XmlAttributes
  + EventEndElement * XmlName
  + EventContent * XmlContent

xmlTransform :: X.FilePath * [Maybe X.PositionRange * X.Event]
  -> [ErrorMessage] + [FileReference * XmlEvent]
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
  >>= tx9
  >>= tx10
  >>= tx11
  >>= tx12
  >>= tx13


tx1 :: a * [b * X.Event]
    -> a * [b * EventAll]
tx1 = _2 . each . _2 %~ toEventAll

tx1a :: a * [Maybe X.PositionRange    * b]
     -> a * [Maybe LineReferenceRange * b]
tx1a = _2 . each . _1 . _Just %~ toLineReferenceRange

tx1b :: X.FilePath * a
     ->   FilePath * a
tx1b = _1 %~ FilePath

tx2 ::   a * [b  * c]
    -> [(a *  b) * c]
tx2 (c, xs) = (shiftLeftProduct . (*) c) <$> xs

tx3 :: Handler e [a * EventAll] =>
                 [a * EventAll]
    -> [e] +     [a * EventAll]
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

tx4 :: Handler e (a * EventAll) =>
                 [a * EventAll]
    -> [e] +     [a * Event9]
tx4 = partialMap _2 tryDrop1

tx5 :: Handler e [a * Event9] =>
                 [a * Event9]
    -> [e] +     [a * Event9]
tx5 = removeSuffixWith handle (^. _2) [sum1 EventEndDocument]

type Event8
  = EventBeginDoctype * Text * (Maybe XmlExternalId)
  + EventEndDoctype
  + EventInstruction * XmlInstruction
  + EventBeginElement * XmlName * XmlAttributes
  + EventEndElement * XmlName
  + EventContent * XmlContent
  + EventComment * Text
  + EventCDATA * Text

tx6 :: Handler e (a * Event9) =>
                 [a * Event9]
    -> [e] +     [a * Event8]
tx6 = partialMap _2 tryDrop1

tx7 :: [(a * Maybe b)  * c]
    -> [(a * (() + b)) * c]
tx7 = each . _1 . _2 %~ maybeToEither ()

tx8 :: Handler e ((FilePath * (() + LineReferenceRange)) * a) =>
                 [(FilePath * (() + LineReferenceRange)) * a]
    -> [e] +     [FileReference                          * a]
tx8 = partialMap (_1 . _2) tryDrop1

tx9 :: Handler e (a * (EventBeginDoctype * Text * (Maybe XmlExternalId) + b)) =>          
                 [a * (EventBeginDoctype * Text * (Maybe XmlExternalId) + b)]
    -> [e] +     [a * (                                                   b)]
tx9 = partialMap _2 tryDrop1

tx10 :: Handler e (a * (EventEndDoctype + b)) =>
                  [a * (EventEndDoctype + b)]
     -> [e] +     [a * (                  b)]
tx10 = partialMap _2 tryDrop1

tx11 :: Handler e (a * (EventInstruction * XmlInstruction + b)) =>
                  [a * (EventInstruction * XmlInstruction + b)]
     -> [e] +     [a * (                                    b)]
tx11 = partialMap _2 tryDrop1

tx12 :: Handler e (a * (b1 + b2 + b3 + EventComment * Text + c)) =>
                  [a * (b1 + b2 + b3 + EventComment * Text + c)]
     -> [e] +     [a * (b1 + b2 + b3 +                       c)]
tx12 = partialMap _2 tryDrop4

tx13 :: Handler e (a * (b1 + b2 + b3 + EventCDATA * Text)) =>
                  [a * (b1 + b2 + b3 + EventCDATA * Text)]
     -> [e] +     [a * (b1 + b2 + b3                    )]
tx13 = partialMap _2 tryDrop4e
