{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Xml where

import Prelude hiding ((*), (+))
import Conduit
import Control.Lens
import Data.Char
import Data.Text (Text)
import Text.Greek.Utility
import qualified Data.Conduit.Attoparsec as X
import qualified Data.Text as T
import qualified Data.XML.Types as X
import qualified Text.XML.Stream.Parse as X

xmlNamespace :: Text
xmlNamespace = "http://www.w3.org/XML/1998/namespace"

xmlNamespacePrefix :: Text
xmlNamespacePrefix = "xml"

newtype Line = Line { getLine :: Int } deriving (Eq, Ord, Show)
newtype Column = Column { getColumn :: Int } deriving (Eq, Ord, Show)
newtype Path = Path { getPath :: FilePath } deriving (Eq, Ord, Show)

readEventsConduit :: FilePath -> IO [Maybe X.PositionRange * X.Event]
readEventsConduit p = runResourceT $ sourceFile p =$= X.parseBytesPos X.def $$ sinkList

data XmlInternalError
  = XmlInternalErrorEmptyEvents
  | XmlInternalErrorExpectedBeginDocument (Maybe X.PositionRange, X.Event)
  | XmlInternalErrorExpectedEndDocument (Maybe X.PositionRange, X.Event)
  | XmlInternalErrorUnexpectedEmptyPositionRange (Maybe X.PositionRange, X.Event)
  deriving (Show)

data XmlError c
  = XmlErrorNonBasicEvent c X.Event
  | XmlErrorUnexpectedNamespace c X.Name
  deriving (Show)

data LineReference = LineReference
  { lineReferenceLine :: Line
  , lineReferenceColumn :: Column }
  deriving (Show)
data FileReference = FileReference
  { fileReferencePath :: Path
  , fileReferenceBegin :: LineReference
  , fileReferenceEnd :: LineReference }
  deriving (Show)

toFileReference :: FilePath -> X.PositionRange -> FileReference
toFileReference p (X.PositionRange (X.Position startLine startColumn) (X.Position endLine endColumn))
  = FileReference (Path p) (LineReference (Line startLine) (Column startColumn)) (LineReference (Line endLine) (Column endColumn))

tryDropBeginDocument :: [Maybe X.PositionRange * X.Event] -> [XmlInternalError] + [Maybe X.PositionRange * X.Event]
tryDropBeginDocument [] = Left [XmlInternalErrorEmptyEvents]
tryDropBeginDocument (x : xs) = case x of
  (_, X.EventBeginDocument) -> Right xs
  _ -> Left . pure $ XmlInternalErrorExpectedBeginDocument x

tryDropEndDocument :: [Maybe X.PositionRange * X.Event] -> [XmlInternalError] + [Maybe X.PositionRange * X.Event]
tryDropEndDocument [] = Left [XmlInternalErrorEmptyEvents]
tryDropEndDocument (x : xs) = case x of
  (_, X.EventEndDocument) -> Right xs
  _ -> Left . pure $ XmlInternalErrorExpectedEndDocument x

tryConvertPosition :: FilePath -> Maybe X.PositionRange * X.Event -> XmlInternalError + (FileReference * X.Event)
tryConvertPosition p (Just r, e) = Right (toFileReference p r, e)
tryConvertPosition _ x = Left $ XmlInternalErrorUnexpectedEmptyPositionRange x

tryConvertPositions :: FilePath -> [Maybe X.PositionRange * X.Event] -> [XmlInternalError] + [FileReference * X.Event]
tryConvertPositions p = splitMap (tryConvertPosition p)

dropComment :: (FileReference * X.Event) -> [FileReference * X.Event] -> [FileReference * X.Event]
dropComment (_, X.EventComment _) xs = xs
dropComment x xs = x : xs

dropComments :: [FileReference * X.Event] -> [FileReference * X.Event]
dropComments = foldr dropComment []

type XmlAttributes = [X.Name * [X.Content]]
type BasicEvent e c a = BasicEventFull e e c a
data BasicEventFull be ee c a
  = BasicEventBeginElement be a
  | BasicEventEndElement ee
  | BasicEventContent c
  deriving (Show)
makePrisms ''BasicEventFull

toBasicEvent :: X.Event -> X.Event + BasicEvent X.Name X.Content XmlAttributes
toBasicEvent (X.EventBeginElement n as) = Right (BasicEventBeginElement n as)
toBasicEvent (X.EventEndElement n) = Right (BasicEventEndElement n)
toBasicEvent (X.EventContent c) = Right (BasicEventContent c)
toBasicEvent x = Left x

toBasicEvents :: [c * X.Event] -> [XmlError c] + [c * BasicEvent X.Name X.Content XmlAttributes]
toBasicEvents = splitMap (tryOver _2 toBasicEvent (errorContext XmlErrorNonBasicEvent _1))

readEvents :: FilePath -> IO ([XmlInternalError] + [FileReference * X.Event])
readEvents p = fmap (initialTransform p) . readEventsConduit $ p

initialTransform :: FilePath -> [Maybe X.PositionRange * X.Event] -> [XmlInternalError] + [FileReference * X.Event]
initialTransform p x = return x
  >>= tryDropBeginDocument
  >>. reverse
  >>= tryDropEndDocument
  >>. reverse
  >>= tryConvertPositions p

trimContent :: Getter s X.Event -> [s] -> [s]
trimContent g = foldr (trimContentItem g) []

trimContentItem :: Getter s X.Event -> s -> [s] -> [s]
trimContentItem g x xs
  | x1 : x2 : xs' <- xs
  , X.EventBeginElement _ _ <- view g x
  , X.EventContent (X.ContentText t) <- view g x1
  , X.EventBeginElement _ _ <- view g x2
  , T.all isSpace t
  = x : x2 : xs'

  | x1 : x2 : xs' <- xs
  , X.EventEndElement _ <- view g x
  , X.EventContent (X.ContentText t) <- view g x1
  , X.EventEndElement _ <- view g x2
  , T.all isSpace t
  = x : x2 : xs'

  | x1 : x2 : xs' <- xs
  , X.EventEndElement _ <- view g x
  , X.EventContent (X.ContentText t) <- view g x1
  , X.EventBeginElement _ _ <- view g x2
  , T.all isSpace t
  = x : x2 : xs'

  | otherwise
  = x : xs

newtype XmlLocalName = XmlLocalName Text deriving (Eq, Ord, Show)

tryDropNamespace :: X.Name -> X.Name + XmlLocalName
tryDropNamespace (X.Name n Nothing Nothing) = Right $ XmlLocalName n
tryDropNamespace n = Left $ n

-- tryDropNamespace :: Getter s c -> Lens s t X.Name XmlLocalName -> s -> XmlError c + t
-- tryDropNamespace g l s = case namespace of
--   Nothing -> Right $ set l (XmlLocalName localName) s
--   Just _  -> Left  $ XmlErrorUnexpectedNamespace (view g s) name
--   where
--     name@(X.Name localName namespace _) = s ^. l

-- tryDropEventElementNamespace
--   :: LensLike (Either X.Name) s t (BasicEventFull be ee c a) (BasicEventFull be' ee' c a)
--   -> Getter s ctx
--   -> s
--   -> XmlError ctx + t
-- tryDropEventElementNamespace l1 l2 c1 c2 x
--   =   tryOver (l1 . _BasicEventBeginElement . _1) tryDropNamespace XmlErrorUnexpectedNamespace c1 x
--   >>= tryOver (l2 . _BasicEventEndElement) tryDropNamespace XmlErrorUnexpectedNamespace c2

{-

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
  >>. useXmlTypes
  >>. nonePositionRange
  >>. useLineReferenceRange
  >>. wrapFilePath
  >>. mapContext
  >>= removeBeginDocumentData
  >>= removeBeginDocumentType
  >>= removeEndDocumentData
  >>= removeEndDocumentType
  >>= ensureLineReferenceRange


removeNamespace
  :: (None + (XmlNamespace * XmlNamePrefix) -> e)
  -> XmlLocalName * ((None + XmlNamespace) * (None + XmlNamePrefix))
  -> e + XmlLocalName
removeNamespace e = tryDrop2eNone e . over lens2e extractSum


trimContent :: [a * XmlEventAll] -> [a * XmlEventAll]
trimContent = foldr trimContentItem []

trimContentItem :: (a * XmlEventAll) -> [a * XmlEventAll] -> [a * XmlEventAll]
trimContentItem x xs
  | x1 : x2 : xs' <- xs
  , Just (XmlBeginElement _, _) <- x  ^? lens2e . prism1
  , Just (XmlContentText t)     <- x1 ^? lens2e . prism3 . prism1
  , Just (XmlBeginElement _, _) <- x2 ^? lens2e . prism1
  , T.all isSpace t
  = x : x2 : xs'

  | x1 : x2 : xs' <- xs
  , Just (XmlEndElement _, _) <- x  ^? lens2e . prism2
  , Just (XmlContentText t)   <- x1 ^? lens2e . prism3 . prism1
  , Just (XmlEndElement _, _) <- x2 ^? lens2e . prism2
  , T.all isSpace t
  = x : x2 : xs'

  | x1 : x2 : xs' <- xs
  , Just (XmlEndElement _, _)   <- x  ^? lens2e . prism2
  , Just (XmlContentText t)     <- x1 ^? lens2e . prism3 . prism1
  , Just (XmlBeginElement _, _) <- x2 ^? lens2e . prism1
  , T.all isSpace t
  = x : x2 : xs'

  | otherwise
  = x : xs


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


useXmlTypes
  :: a * [b * X.Event]
  -> a * [b * XmlEventExtra]
useXmlTypes = over (lens2e . each . lens2e) toXmlEventExtra

nonePositionRange
  :: a * [  Maybe X.PositionRange  * b]
  -> a * [(None + X.PositionRange) * b]
nonePositionRange = over (lens2e . each . lens1) maybeToNone

useLineReferenceRange
  :: a * [(None + X.PositionRange   ) * b]
  -> a * [(None + LineReferenceRange) * b]
useLineReferenceRange = over (lens2e . each . lens1 . prism2e) toLineReferenceRange

wrapFilePath
  :: X.FilePath * a
  ->   FilePath * a
wrapFilePath = over lens1 FilePath

mapContext
  ::   a * [b  * c]
  -> [(a *  b) * c]
mapContext (c, xs) = (shiftLeftProduct . (*) c) <$> xs

removeBeginDocumentData
  :: Handler e [a * XmlEventExtra] =>
               [a * XmlEventExtra]
  -> [e] +     [a * XmlEventExtra]
removeBeginDocumentData = removePrefixWith handle (^. lens2e) [sum1 (XmlBeginDocument ())]

removeBeginDocumentType
  :: Handler e (a * (XmlBeginDocument + XmlEndDocument + XmlEventAll)) =>
               [a * (XmlBeginDocument + XmlEndDocument + XmlEventAll)]
  -> [e] +     [a * (                   XmlEndDocument + XmlEventAll)]
removeBeginDocumentType = handleMap lens2e tryDrop1

removeEndDocumentData
  :: Handler e [a * (XmlEndDocument + XmlEventAll)] =>
               [a * (XmlEndDocument + XmlEventAll)]
  -> [e] +     [a * (XmlEndDocument + XmlEventAll)]
removeEndDocumentData = removeSuffixWith handle (^. lens2e) [sum1 (XmlEndDocument ())]

removeEndDocumentType
  :: Handler e (a * (XmlEndDocument + XmlEventAll)) =>
               [a * (XmlEndDocument + XmlEventAll)]
  -> [e] +     [a * (                 XmlEventAll)]
removeEndDocumentType = handleMap lens2e tryDrop1

ensureLineReferenceRange
  :: Handler e ((FilePath * (None + LineReferenceRange)) * a) =>
               [(FilePath * (None + LineReferenceRange)) * a]
  -> [e] +     [(FilePath *         LineReferenceRange ) * a]
ensureLineReferenceRange = handleMap (lens1 . lens2e) tryDrop1
-}
