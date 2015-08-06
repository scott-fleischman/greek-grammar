{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Greek.Source.Sblgnt where

import Prelude hiding ((*), (+), log, FilePath)
import Conduit
import Control.Lens
import Data.String
import Data.Text (Text)
import Text.Greek.Utility
import qualified Data.Conduit.Attoparsec as X
import qualified Data.XML.Types as X
import qualified Text.XML.Stream.Parse as X
import qualified Prelude as X (FilePath)

newtype FilePath = FilePath { getFilePath :: X.FilePath }
instance Handler ErrorMessage FilePath where
  handle = fromString . getFilePath

type FileReference = FilePath * LineReferenceRange

readEvents :: X.FilePath -> IO [(Maybe X.PositionRange, X.Event)]
readEvents p = runResourceT $ sourceFile p =$= X.parseBytesPos X.def $$ sinkList


removePrefixWith' :: Eq b => ([a] -> e) -> (a -> b) -> [b] -> [a] -> e + [a]
removePrefixWith' e f m as
  | fmap f target == m = Right $ drop matchLength as
  | otherwise          = Left $ e target
  where
    matchLength = length m
    target = take matchLength as

removeSuffixWith' :: Eq b => ([a] -> e) -> (a -> b) -> [b] -> [a] -> e + [a]
removeSuffixWith' e f m as
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
instance Handler ErrorMessage EventBeginDocument where handle = fromString . show

data EventEndDocument = EventEndDocument deriving (Eq, Ord, Show)
instance Handler ErrorMessage EventEndDocument where handle = fromString . show

data EventBeginDoctype = EventBeginDoctype deriving (Eq, Ord, Show)
instance Handler ErrorMessage EventBeginDoctype where handle = fromString . show

data EventEndDoctype = EventEndDoctype deriving (Eq, Ord, Show)
instance Handler ErrorMessage EventEndDoctype where handle = fromString . show

data EventInstruction = EventInstruction deriving (Eq, Ord, Show)
instance Handler ErrorMessage EventInstruction where handle = fromString . show

data EventBeginElement = EventBeginElement deriving (Eq, Ord, Show)
instance Handler ErrorMessage EventBeginElement where handle = fromString . show

data EventEndElement = EventEndElement deriving (Eq, Ord, Show)
instance Handler ErrorMessage EventEndElement where handle = fromString . show

data EventContent = EventContent deriving (Eq, Ord, Show)
instance Handler ErrorMessage EventContent where handle = fromString . show

data EventComment = EventComment deriving (Eq, Ord, Show)
instance Handler ErrorMessage EventComment where handle = fromString . show

data EventCDATA = EventCDATA deriving (Eq, Ord, Show)
instance Handler ErrorMessage EventCDATA where handle = fromString . show

data XmlNameId = XmlNameId deriving (Eq, Ord, Show)
instance Handler ErrorMessage XmlNameId where handle = fromString . show

type XmlName = XmlNameId * Text * Maybe Text * Maybe Text
toXmlName :: X.Name -> XmlName
toXmlName (X.Name a b c) = XmlNameId * a * b * c

data XmlContentText = XmlContentText deriving (Eq, Ord, Show)
instance Handler ErrorMessage XmlContentText where handle = fromString . show

data XmlContentEntity = XmlContentEntity deriving (Eq, Ord, Show)
instance Handler ErrorMessage XmlContentEntity where handle = fromString . show

type XmlContent
  = XmlContentText * Text
  + XmlContentEntity * Text
toXmlContent :: X.Content -> XmlContent
toXmlContent (X.ContentText a)   = sum1  (XmlContentText * a)
toXmlContent (X.ContentEntity a) = sum2e (XmlContentEntity * a)

data XmlInstructionId = XmlInstructionId deriving (Eq, Ord, Show)
instance Handler ErrorMessage XmlInstructionId where handle = fromString . show

type XmlInstruction = XmlInstructionId * Text * Text
toXmlInstruction :: X.Instruction -> XmlInstruction
toXmlInstruction (X.Instruction a b) = XmlInstructionId * a * b

data XmlSystemId = XmlSystemId deriving (Eq, Ord, Show)
instance Handler ErrorMessage XmlSystemId where handle = fromString . show

data XmlPublicId = XmlPublicId deriving (Eq, Ord, Show)
instance Handler ErrorMessage XmlPublicId where handle = fromString . show

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


tx1 :: X.FilePath * [Maybe X.PositionRange * X.Event]
    -> X.FilePath * [Maybe X.PositionRange * EventAll]
tx1 = _2 . each . _2 %~ toEventAll

tx1a :: X.FilePath * [Maybe X.PositionRange    * EventAll]
     -> X.FilePath * [Maybe LineReferenceRange * EventAll]
tx1a = _2 . each . _1 . _Just %~ toLineReferenceRange

tx1b :: X.FilePath * [Maybe LineReferenceRange * EventAll]
     ->   FilePath * [Maybe LineReferenceRange * EventAll]
tx1b = _1 %~ FilePath

tx2 :: FilePath * [Maybe LineReferenceRange * EventAll]
    -> [(FilePath * Maybe LineReferenceRange) * EventAll]
tx2 (c, xs) = (shiftLeftProduct . (*) c) <$> xs

tx3 ::                  [(FilePath * Maybe LineReferenceRange) * EventAll]
    -> [ErrorMessage] + [(FilePath * Maybe LineReferenceRange) * EventAll]
tx3 = removePrefixWith' handle (^. _2) [sum1 EventBeginDocument]

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

tx4 ::                  [(FilePath * Maybe LineReferenceRange) * EventAll]
    -> [ErrorMessage] + [(FilePath * Maybe LineReferenceRange) * Event9]
tx4 = split . fmap (\x -> x & _2 (constHandle tryDrop1 x))

tx5 ::                  [(FilePath * Maybe LineReferenceRange) * Event9]
    -> [ErrorMessage] + [(FilePath * Maybe LineReferenceRange) * Event9]
tx5 = removeSuffixWith' handle (^. _2) [sum1 EventEndDocument]

type Event8
  = EventBeginDoctype * Text * (Maybe XmlExternalId)
  + EventEndDoctype
  + EventInstruction * XmlInstruction
  + EventBeginElement * XmlName * XmlAttributes
  + EventEndElement * XmlName
  + EventContent * XmlContent
  + EventComment * Text
  + EventCDATA * Text

tx6 ::                  [(FilePath * Maybe LineReferenceRange) * Event9]
    -> [ErrorMessage] + [(FilePath * Maybe LineReferenceRange) * Event8]
tx6 = split . fmap (\x -> x & _2 (constHandle tryDrop1 x))

tx7 :: [(FilePath * Maybe LineReferenceRange)  * Event8]
    -> [(FilePath * (() + LineReferenceRange)) * Event8]
tx7 = each . _1 . _2 %~ maybeToEither ()

tx8 ::                  [(FilePath * (() + LineReferenceRange)) * Event8]
    -> [ErrorMessage] + [FileReference                          * Event8]
tx8 = split . fmap (\x -> x & (_1 . _2) (constHandle tryDrop1 x))

tx9 ::                  [FileReference * (EventBeginDoctype * Text * (Maybe XmlExternalId) + EventEndDoctype + EventInstruction * XmlInstruction + EventBeginElement * XmlName * XmlAttributes + EventEndElement * XmlName + EventContent * XmlContent + EventComment * Text + EventCDATA * Text)]
    -> [ErrorMessage] + [FileReference * (                                                   EventEndDoctype + EventInstruction * XmlInstruction + EventBeginElement * XmlName * XmlAttributes + EventEndElement * XmlName + EventContent * XmlContent + EventComment * Text + EventCDATA * Text)]
tx9 = split . fmap (\x -> x & _2 (constHandle tryDrop1 x))

tx10 ::                  [FileReference * (EventEndDoctype + EventInstruction * XmlInstruction + EventBeginElement * XmlName * XmlAttributes + EventEndElement * XmlName + EventContent * XmlContent + EventComment * Text + EventCDATA * Text)]
     -> [ErrorMessage] + [FileReference * (                  EventInstruction * XmlInstruction + EventBeginElement * XmlName * XmlAttributes + EventEndElement * XmlName + EventContent * XmlContent + EventComment * Text + EventCDATA * Text)]
tx10 = split . fmap (\x -> x & _2 (constHandle tryDrop1 x))

tx11 ::                  [FileReference * (EventInstruction * XmlInstruction + EventBeginElement * XmlName * XmlAttributes + EventEndElement * XmlName + EventContent * XmlContent + EventComment * Text + EventCDATA * Text)]
     -> [ErrorMessage] + [FileReference * (                                    EventBeginElement * XmlName * XmlAttributes + EventEndElement * XmlName + EventContent * XmlContent + EventComment * Text + EventCDATA * Text)]
tx11 = split . fmap (\x -> x & _2 (constHandle tryDrop1 x))

tx12 ::                  [FileReference * (EventBeginElement * XmlName * XmlAttributes + EventEndElement * XmlName + EventContent * XmlContent + EventComment * Text + EventCDATA * Text)]
     -> [ErrorMessage] + [FileReference * (EventBeginElement * XmlName * XmlAttributes + EventEndElement * XmlName + EventContent * XmlContent +                       EventCDATA * Text)]
tx12 = split . fmap (\x -> x & _2 (constHandle tryDrop4 x))

tx13 ::                  [FileReference * (EventBeginElement * XmlName * XmlAttributes + EventEndElement * XmlName + EventContent * XmlContent + EventCDATA * Text)]
     -> [ErrorMessage] + [FileReference * XmlEvent]
tx13 = split . fmap (\x -> x & _2 (constHandle tryDrop4e x))
