{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Text.Greek.Xml where

import Prelude hiding ((*), (+), log, FilePath)
import Conduit
import Control.Lens
import Data.Char
import Data.Text (Text)
import Text.Greek.Utility
import qualified Data.Conduit.Attoparsec as X
import qualified Data.Text as T
import qualified Data.XML.Types as X
import qualified Text.XML.Stream.Parse as X
import qualified Prelude as X (FilePath)

newtype Line = Line { getLine :: Int } deriving (Eq, Ord, Show)
newtype Column = Column { getColumn :: Int } deriving (Eq, Ord, Show)
newtype FilePath = FilePath { getFilePath :: X.FilePath } deriving (Eq, Ord, Show)

type LineReference = Line * Column
type LineReferenceRange = LineReference + LineReference * LineReference
type FileReference = FilePath * LineReferenceRange

type XmlEventAll
  = XmlBeginElement * XmlName * XmlAttributes
  + XmlEndElement * XmlName
  + XmlContent
  + XmlCDATA
  + XmlBeginDoctype * XmlDoctypeName * (None + XmlExternalId)
  + XmlEndDoctype
  + XmlInstruction
  + XmlComment

readEvents :: X.FilePath -> IO ([ErrorMessage] + [FileReference * XmlEventAll])
readEvents p = fmap xmlTransform . fmap (p *) . readEventsConduit $ p

xmlTransform :: X.FilePath * [Maybe X.PositionRange * X.Event]
  -> [ErrorMessage] + [FileReference * XmlEventAll]
xmlTransform x = return x
  >>. tx1
  >>. tx1'
  >>. tx1a
  >>. tx1b
  >>. tx2
  >>= tx3
  >>= tx4
  >>= tx5
  >>= tx6
  >>= tx8

trimContent :: [a * XmlEventAll] -> [a * XmlEventAll]
trimContent = foldr trimContentItem []

trimContentItem :: (a * XmlEventAll) -> [a * XmlEventAll] -> [a * XmlEventAll]
trimContentItem x xs
  | (_, b) <- x
  , x1 : x2 : xs' <- xs
  , (_, c) <- x1
  , (_, b2) <- x2
  , Left (XmlBeginElement _, _) <- b
  , Right (Right (Left (Left (XmlContentText t)))) <- c
  , Left (XmlBeginElement _, _) <- b2
  , T.all isSpace t
  = x : x2 : xs'

  | (_, e) <- x
  , x1 : x2 : xs' <- xs
  , (_, c) <- x1
  , (_, e2) <- x2
  , Right (Left (XmlEndElement _, _)) <- e
  , Right (Right (Left (Left (XmlContentText t)))) <- c
  , Right (Left (XmlEndElement _, _)) <- e2
  , T.all isSpace t
  = x : x2 : xs'

  | otherwise
  = x : xs

readEventsConduit :: X.FilePath -> IO [(Maybe X.PositionRange, X.Event)]
readEventsConduit p = runResourceT $ sourceFile p =$= X.parseBytesPos X.def $$ sinkList


toLineReference :: X.Position -> LineReference
toLineReference (X.Position line column) = Line line * Column column

toLineReferenceRange :: X.PositionRange -> LineReferenceRange
toLineReferenceRange (X.PositionRange start end)
  | start == end = sum1  (toLineReference start)
  | otherwise    = sum2e (toLineReference start * toLineReference end)


newtype XmlBeginDocument = XmlBeginDocument () deriving (Eq, Ord, Show)
newtype XmlEndDocument = XmlEndDocument () deriving (Eq, Ord, Show)
newtype XmlBeginDoctype = XmlBeginDoctype () deriving (Eq, Ord, Show)
newtype XmlEndDoctype = XmlEndDoctype () deriving (Eq, Ord, Show)
newtype XmlBeginElement = XmlBeginElement () deriving (Eq, Ord, Show)
newtype XmlEndElement = XmlEndElement () deriving (Eq, Ord, Show)

newtype XmlDoctypeName = XmlDoctypeName Text deriving (Eq, Ord, Show)

newtype XmlComment = XmlComment Text deriving (Eq, Ord, Show)
newtype XmlCDATA = XmlCDATA Text deriving (Eq, Ord, Show)

newtype XmlLocalName = XmlLocalName Text deriving (Eq, Ord, Show)
newtype XmlNamespace = XmlNamespace Text deriving (Eq, Ord, Show)
newtype XmlNamePrefix = XmlNamePrefix Text deriving (Eq, Ord, Show)

type XmlName = XmlLocalName * (None + XmlNamespace) * (None + XmlNamePrefix)
toXmlName :: X.Name -> XmlName
toXmlName (X.Name a b c) = XmlLocalName a * (prism2e %~ XmlNamespace) (maybeToNone b) * (prism2e %~ XmlNamePrefix) (maybeToNone c)

newtype XmlContentText = XmlContentText Text deriving (Eq, Ord, Show)
newtype XmlContentEntity = XmlContentEntity Text deriving (Eq, Ord, Show)

type XmlContent = XmlContentText + XmlContentEntity
toXmlContent :: X.Content -> XmlContent
toXmlContent (X.ContentText a)   = sum1  (XmlContentText a)
toXmlContent (X.ContentEntity a) = sum2e (XmlContentEntity a)

newtype XmlInstructionTarget = XmlInstructionTarget Text deriving (Eq, Ord, Show)
newtype XmlInstructionData = XmlInstructionData Text deriving (Eq, Ord, Show)

type XmlInstruction = XmlInstructionTarget * XmlInstructionData
toXmlInstruction :: X.Instruction -> XmlInstruction
toXmlInstruction (X.Instruction a b) = XmlInstructionTarget a * XmlInstructionData b

newtype XmlSystemId = XmlSystemId Text deriving (Eq, Ord, Show)

newtype XmlPublicOwnerId = XmlPublicOwnerId Text deriving (Eq, Ord, Show)
newtype XmlPublicTextId = XmlPublicTextId Text deriving (Eq, Ord, Show)

type XmlPublicId = XmlPublicOwnerId * XmlPublicTextId

type XmlExternalId = XmlSystemId + XmlPublicId
toXmlExternalId :: X.ExternalID -> XmlExternalId
toXmlExternalId (X.SystemID a)   = sum1  (XmlSystemId a)
toXmlExternalId (X.PublicID a b) = sum2e (XmlPublicOwnerId a * XmlPublicTextId b)

type XmlAttributes = [XmlName * [XmlContent]]
toXmlAttributes :: [X.Name * [X.Content]] -> [XmlName * [XmlContent]]
toXmlAttributes = over (each . _1) toXmlName . over (each . _2 . each) toXmlContent

type XmlEventExtra = XmlBeginDocument + XmlEndDocument + XmlEventAll

toXmlEventExtra :: X.Event -> XmlEventExtra
toXmlEventExtra  X.EventBeginDocument     = sum1   (XmlBeginDocument ())
toXmlEventExtra  X.EventEndDocument       = sum2   (XmlEndDocument ())
toXmlEventExtra (X.EventBeginElement n a) = sum3   (XmlBeginElement () * toXmlName n * toXmlAttributes a)
toXmlEventExtra (X.EventEndElement n)     = sum4   (XmlEndElement () * toXmlName n)
toXmlEventExtra (X.EventContent c)        = sum5   (toXmlContent c)
toXmlEventExtra (X.EventCDATA t)          = sum6   (XmlCDATA t)
toXmlEventExtra (X.EventBeginDoctype t e) = sum7   (XmlBeginDoctype () * XmlDoctypeName t * (prism2e %~ toXmlExternalId) (maybeToNone e))
toXmlEventExtra  X.EventEndDoctype        = sum8   (XmlEndDoctype ())
toXmlEventExtra (X.EventInstruction i)    = sum9   (toXmlInstruction i)
toXmlEventExtra (X.EventComment t)        = sum10e (XmlComment t)


tx1 :: a * [b * X.Event]
    -> a * [b * XmlEventExtra]
tx1 = over (lens2e . each . lens2e) toXmlEventExtra

tx1' :: a * [  Maybe X.PositionRange  * b]
     -> a * [(None + X.PositionRange) * b]
tx1' = over (lens2e . each . lens1) maybeToNone

tx1a :: a * [(None + X.PositionRange   ) * b]
     -> a * [(None + LineReferenceRange) * b]
tx1a = over (lens2e . each . lens1 . prism2e) toLineReferenceRange

tx1b :: X.FilePath * a
     ->   FilePath * a
tx1b = over lens1 FilePath

tx2 ::   a * [b  * c]
    -> [(a *  b) * c]
tx2 (c, xs) = (shiftLeftProduct . (*) c) <$> xs

tx3 :: Handler e [a * XmlEventExtra] =>
                 [a * XmlEventExtra]
    -> [e] +     [a * XmlEventExtra]
tx3 = removePrefixWith handle (^. lens2e) [sum1 (XmlBeginDocument ())]

tx4 :: Handler e (a * (XmlBeginDocument + XmlEndDocument + XmlEventAll)) =>
                 [a * (XmlBeginDocument + XmlEndDocument + XmlEventAll)]
    -> [e] +     [a * (                   XmlEndDocument + XmlEventAll)]
tx4 = handleMap lens2e tryDrop1

tx5 :: Handler e [a * (XmlEndDocument + XmlEventAll)] =>
                 [a * (XmlEndDocument + XmlEventAll)]
    -> [e] +     [a * (XmlEndDocument + XmlEventAll)]
tx5 = removeSuffixWith handle (^. lens2e) [sum1 (XmlEndDocument ())]

tx6 :: Handler e (a * (XmlEndDocument + XmlEventAll)) =>
                 [a * (XmlEndDocument + XmlEventAll)]
    -> [e] +     [a * (                 XmlEventAll)]
tx6 = handleMap lens2e tryDrop1

tx8 :: Handler e ((FilePath * (None + LineReferenceRange)) * a) =>
                 [(FilePath * (None + LineReferenceRange)) * a]
    -> [e] +     [(FilePath *         LineReferenceRange ) * a]
tx8 = handleMap (lens1 . lens2e) tryDrop1
