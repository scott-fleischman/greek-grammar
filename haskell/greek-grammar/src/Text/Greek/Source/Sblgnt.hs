{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Greek.Source.Sblgnt where

import Prelude hiding ((*), (+), log)
import Conduit
import Control.Lens
import Data.String
import Data.Text (Text)
import Text.Greek.Utility
import qualified Data.Conduit.Attoparsec as P
import qualified Data.XML.Types as X
import qualified Text.XML.Stream.Parse as P


readEvents :: FilePath -> IO [(Maybe P.PositionRange, X.Event)]
readEvents p = runResourceT $ sourceFile p =$= P.parseBytesPos P.def $$ sinkList


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

removePrefixWith :: Eq b => (a -> b) -> [b] -> [a] -> Maybe [a]
removePrefixWith _ []       ys                  = Just ys
removePrefixWith f (x : xs) (y : ys) | x == f y = removePrefixWith f xs ys
removePrefixWith _ _        _                   = Nothing

removeSuffixWithHelper :: Eq b => (a -> b) -> a -> Maybe ([b], [a]) -> Maybe ([b], [a])
removeSuffixWithHelper _ y (Just ([],     ys))            = Just ([], y : ys)
removeSuffixWithHelper f y (Just (x : xs, ys)) | x == f y = Just (xs,     ys)
removeSuffixWithHelper _ _ _                              = Nothing

removeSuffixWith :: Eq b => (a -> b) -> [b] -> [a] -> Maybe [a]
removeSuffixWith f xs = fmap snd . foldr (removeSuffixWithHelper f) (Just (reverse $ xs, []))

maybeToEitherWith :: (x -> a) -> (x -> Maybe b) -> x -> Either a b
maybeToEitherWith e f x = case f x of
  Just x' -> Right x'
  Nothing -> Left (e x)

maybeToEither :: a -> Maybe b -> a + b
maybeToEither a Nothing = Left a
maybeToEither _ (Just b) = Right b

toLineReference :: P.Position -> LineReference
toLineReference (P.Position line column) = Line line * Column column

toLineReferenceRange :: P.PositionRange -> LineReferenceRange
toLineReferenceRange (P.PositionRange start end)
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

type Event8
  = EventBeginDoctype * Text * (Maybe XmlExternalId)
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

xmlTransform :: FilePath * [Maybe P.PositionRange * X.Event]
  -> [ErrorMessage] + [(FilePath * Maybe LineReferenceRange) * Event8]
xmlTransform x = return x
  >>. tx1
  >>. tx1a
  >>. tx2
  >>= tx3
  >>= tx4
  >>= tx5
  >>= tx6
  -- >>. tx7



tx1 :: FilePath * [Maybe P.PositionRange * X.Event]
    -> FilePath * [Maybe P.PositionRange * EventAll]
tx1 = _2 . each . _2 %~ toEventAll

tx1a :: FilePath * [Maybe P.PositionRange    * EventAll]
     -> FilePath * [Maybe LineReferenceRange * EventAll]
tx1a = _2 . each . _1 . _Just %~ toLineReferenceRange

tx2 :: FilePath * [Maybe LineReferenceRange * EventAll]
    -> [(FilePath * Maybe LineReferenceRange) * EventAll]
tx2 (c, xs) = (shiftLeftProduct . (*) c) <$> xs



tx3 ::                  [(FilePath * Maybe LineReferenceRange) * EventAll]
    -> [ErrorMessage] + [(FilePath * Maybe LineReferenceRange) * EventAll]
tx3 = removePrefixWith' handle (^. _2) [sum1 EventBeginDocument]

tx4 ::                  [(FilePath * Maybe LineReferenceRange) * EventAll]
    -> [ErrorMessage] + [(FilePath * Maybe LineReferenceRange) * Event9]
tx4 = split . fmap (\x -> x & _2 (tryDrop1 . const . handle $ x))

tx5 ::                  [(FilePath * Maybe LineReferenceRange) * Event9]
    -> [ErrorMessage] + [(FilePath * Maybe LineReferenceRange) * Event9]
tx5 = removeSuffixWith' handle (^. _2) [sum1 EventEndDocument]

tx6 ::                  [(FilePath * Maybe LineReferenceRange) * Event9]
    -> [ErrorMessage] + [(FilePath * Maybe LineReferenceRange) * Event8]
tx6 = split . fmap (\x -> x & _2 (tryDrop1 . const . handle $ x))

-- tx7 :: [(FilePath * Maybe LineReferenceRange) * Event8]
--     -> [(FilePath * (() + LineReferenceRange)) * Event8]
-- tx7 = each . _1 . _2 %~ maybeToEither ()


-- tx8 :: [(FilePath * (() + LineReferenceRange)) * Event8]
--     -> [(FilePath * LineReferenceRange) * Event8]
-- tx8 = partialMapContexts (_2 %~ tryDrop1 log)

-- tf1 :: FilePath * [Maybe P.PositionRange * X.Event]
--     -> FilePath * [Maybe P.PositionRange * XmlEventAll]
-- tf1 = _2 %~ fmap (_2 %~ toXmlEventAll)


-- tf2 ::              ([Maybe P.PositionRange * XmlEventAll] -> e)
--   -> FilePath *      [Maybe P.PositionRange * XmlEventAll]
--   -> FilePath * (e + [Maybe P.PositionRange * XmlEventAll])
-- tf2 e = _2 %~ maybeToEither (e . take (length match)) (removePrefixWith (^. _2) match)
--   where match = [sum1 XmlBeginDocument]

-- tf2' ::                          ([Maybe P.PositionRange * XmlEventAll] -> e)
--   ->  FilePath *                  [Maybe P.PositionRange * XmlEventAll]
--   -> (FilePath * e) + (FilePath * [Maybe P.PositionRange * XmlEventAll])
-- tf2' e f = distributeProduct (tf2 e f)

-- tf2'' ::                           ([Maybe P.PositionRange * XmlEventAll] -> e)
--   ->  FilePath *                    [Maybe P.PositionRange * XmlEventAll]
--   -> (FilePath * [e]) + (FilePath * [Maybe P.PositionRange * XmlEventAll])
-- tf2'' e = singleErrorContext . tf2' e


-- tf3 ::              ([Maybe P.PositionRange * XmlEventAll] -> e)
--   -> FilePath *      [Maybe P.PositionRange * XmlEventAll]
--   -> FilePath * (e + [Maybe P.PositionRange * XmlEventAll])
-- tf3 e = _2 %~ maybeToEither (e . take (length match)) (removeSuffixWith (^. _2) match)
--   where match = [sum2 XmlEndDocument]

-- tf3' ::                          ([Maybe P.PositionRange * XmlEventAll] -> e)
--   ->  FilePath *                  [Maybe P.PositionRange * XmlEventAll]
--   -> (FilePath * e) + (FilePath * [Maybe P.PositionRange * XmlEventAll])
-- tf3' e f = distributeProduct (tf3 e f)

-- tf3'' ::                           ([Maybe P.PositionRange * XmlEventAll] -> e)
--   ->  FilePath *                    [Maybe P.PositionRange * XmlEventAll]
--   -> (FilePath * [e]) + (FilePath * [Maybe P.PositionRange * XmlEventAll])
-- tf3'' e = singleErrorContext . tf3' e


-- tf4 ::                                       (XmlEventAll -> e)
--   -> FilePath *      [Maybe P.PositionRange * XmlEventAll]
--   -> FilePath * ([e] +     [P.PositionRange * XmlEventAll])
-- tf4 e = _2 %~ combineEithers . fmap (maybeToEither (e . snd) (traverseOf _1 id))

-- tf4' ::                                                     (XmlEventAll -> e)
--   ->  FilePath *                    [Maybe P.PositionRange * XmlEventAll]
--   -> (FilePath * [e]) + (FilePath * [P.PositionRange       * XmlEventAll])
-- tf4' e f = distributeProduct (tf4 e f)


-- tf5 :: FilePath * [P.PositionRange * XmlEventAll]
--     -> [FileReference * XmlEventAll]
-- tf5 (c, xs) = fmap (_1 %~ toFileReference c) xs

-- tf6 :: (XmlBeginDocument -> e) -> XmlBeginDocument + a -> e + a
-- tf6 = tryDrop1

-- tf6a
--   :: (XmlBeginDocument -> e)
--   -> c * (XmlBeginDocument + a)
--   -> c * (e + a)
-- tf6a e = _2 %~ tf6 e

-- tf6b
--   :: (XmlBeginDocument -> e)
--   -> c * (XmlBeginDocument + a)
--   -> (c * e) + (c * a)
-- tf6b e = distributeProduct . tf6a e

-- tf6'
--   :: (XmlBeginDocument -> e)
--   -> [c * (XmlBeginDocument + a)]
--   -> [c * e] + [c * a]
-- tf6' e = transformAll (tf6b e)

-- tf7 :: (XmlEndDocument -> e) -> XmlEndDocument + a -> e + a
-- tf7 = tryDrop1

-- tf8 :: (XmlBeginDoctype * Text * (Maybe X.ExternalID) -> e)
--     ->  XmlBeginDoctype * Text * (Maybe X.ExternalID) + a
--     ->  e + a
-- tf8 = tryDrop1

-- tf9 :: (XmlEndDoctype -> e) -> XmlEndDoctype + a -> e + a
-- tf9 = tryDrop1

-- tf10 :: (XmlInstruction * X.Instruction -> e)
--      ->  XmlInstruction * X.Instruction + a
--      ->  e + a
-- tf10 = tryDrop1

-- tf11 ::             (XmlComment * Text -> e)
--   ->     a + b + c + XmlComment * Text + d
--   -> e + a + b + c + d
-- tf11 = tryDrop4

-- tf12 ::             (XmlCDATA * Text -> e)
--   ->     a + b + c + XmlCDATA * Text
--   -> e + a + b + c
-- tf12 = tryDrop4e


-- tfShow :: Show x => x -> ErrorMessage
-- tfShow = ErrorMessage . show

-- type ErrorMap x x' = x -> [ErrorMessage] + x'
-- type ErrorMap' x = x -> [ErrorMessage] + x

-- showErrorMessage :: Show a => String -> a -> ErrorMessage
-- showErrorMessage m = errorMap ((++) (m ++ " ")) . tfShow

-- addErrorContext :: Show c => c -> ErrorMessage -> ErrorMessage
-- addErrorContext c = errorMap ((++) (show c ++ " "))

-- addErrorContext' :: Show c => c * ErrorMessage -> ErrorMessage
-- addErrorContext' (c, m) = addErrorContext c m

-- mapErrorContexts :: Show c => (c * [ErrorMessage]) -> [ErrorMessage]
-- mapErrorContexts (c, es) = fmap (addErrorContext c) es

-- tf2p :: ErrorMap' (FilePath * [Maybe P.PositionRange * XmlEventAll])
-- tf2p = (_Left %~ mapErrorContexts) . tf2'' (showErrorMessage "expected prefix BeginDocument")

-- tf3p :: ErrorMap' (FilePath * [Maybe P.PositionRange * XmlEventAll])
-- tf3p = (_Left %~ mapErrorContexts) . tf3'' (showErrorMessage "expected suffix EndDocument")

-- tf4p :: ErrorMap (FilePath * [Maybe P.PositionRange * XmlEventAll]) (FilePath * [P.PositionRange * XmlEventAll])
-- tf4p = (_Left %~ mapErrorContexts) . tf4' (showErrorMessage "missing PositionRange")


-- tryDrop1All
--   :: (a -> e)
--   -> [c * (a + b)]
--   -> [c * e] + [c * b]
-- tryDrop1All e = transformAll (distributeProduct . (_2 %~ tryDrop1 e))

-- tryDrop1AllError :: (Show a, Show c) => ErrorMap [c * (a + b)] [c * b]
-- tryDrop1AllError = (_Left %~ fmap addErrorContext') . tryDrop1All (showErrorMessage "unexpected")

-- tf6p :: ErrorMap
--   [FileReference * XmlEventAll]
--   [FileReference *
--     ( XmlBeginElement * X.Name * [(X.Name, [X.Content])]
--     + XmlEndElement * X.Name
--     + XmlContent * X.Content
--     + XmlComment * Text
--     + XmlCDATA * Text)]
-- tf6p x = tryDrop1AllError x >>= tryDrop1AllError >>= tryDrop1AllError >>= tryDrop1AllError >>= tryDrop1AllError

-- readEvents' :: FilePath -> IO (FilePath * [Maybe P.PositionRange * XmlEventAll])
-- readEvents' p = readEvents p >>= return . tf1 . (,) p
