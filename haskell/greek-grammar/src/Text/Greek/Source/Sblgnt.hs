{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Text.Greek.Source.Sblgnt where

import Prelude hiding ((*), (+), error)
import Conduit
import Control.Lens
import Data.Text (Text)
import Text.Greek.Utility
import qualified Data.Conduit.Attoparsec as P
import qualified Data.XML.Types as X
import qualified Text.XML.Stream.Parse as P


readEvents :: FilePath -> IO [(Maybe P.PositionRange, X.Event)]
readEvents p = runResourceT $ sourceFile p =$= P.parseBytesPos P.def $$ sinkList


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

maybeToEither :: (x -> a) -> (x -> Maybe b) -> x -> Either a b
maybeToEither e f x = case f x of
  Just x' -> Right x'
  Nothing -> Left (e x)


newtype Line = Line { getLine :: Int } deriving (Show, Eq, Ord)
newtype Column = Column { getColumn :: Int } deriving (Show, Eq, Ord)
data LineReference = LineReference Line Column deriving (Show, Eq, Ord)
data FileReference
  = FileReferencePoint FilePath LineReference
  | FileReferenceRange FilePath LineReference LineReference
  deriving (Show, Eq, Ord)

lineReferenceToString :: LineReference -> String
lineReferenceToString (LineReference (Line line) (Column column)) = show line ++ ":" ++ show column

fileReferenceToString :: FileReference -> String
fileReferenceToString (FileReferencePoint p r) = show p ++ ":" ++ lineReferenceToString r
fileReferenceToString (FileReferenceRange p r1 r2) = show p ++ ":" ++ lineReferenceToString r1 ++ "-" ++ lineReferenceToString r2

toLineReference :: P.Position -> LineReference
toLineReference (P.Position line column) = LineReference (Line line) (Column column)

toFileReference :: FilePath -> P.PositionRange -> FileReference
toFileReference fp (P.PositionRange start end)
  | start == end = FileReferencePoint fp (toLineReference start)
  | otherwise    = FileReferenceRange fp (toLineReference start) (toLineReference end)



data XmlBeginDocument = XmlBeginDocument deriving (Eq, Ord, Show)
data XmlEndDocument = XmlEndDocument deriving (Eq, Ord, Show)
data XmlBeginDoctype = XmlBeginDoctype deriving (Eq, Ord, Show)
data XmlEndDoctype = XmlEndDoctype deriving (Eq, Ord, Show)
data XmlInstruction = XmlInstruction deriving (Eq, Ord, Show)
data XmlBeginElement = XmlBeginElement deriving (Eq, Ord, Show)
data XmlEndElement = XmlEndElement deriving (Eq, Ord, Show)
data XmlContent = XmlContent deriving (Eq, Ord, Show)
data XmlComment = XmlComment deriving (Eq, Ord, Show)
data XmlCDATA = XmlCDATA deriving (Eq, Ord, Show)

type XmlEventAll
  = XmlBeginDocument
  + XmlEndDocument
  + XmlBeginDoctype * Text * (Maybe X.ExternalID)
  + XmlEndDoctype
  + XmlInstruction * X.Instruction
  + XmlBeginElement * X.Name * [(X.Name, [X.Content])]
  + XmlEndElement * X.Name
  + XmlContent * X.Content
  + XmlComment * Text
  + XmlCDATA * Text

toXmlEventAll :: X.Event -> XmlEventAll
toXmlEventAll  X.EventBeginDocument     = sum1    XmlBeginDocument
toXmlEventAll  X.EventEndDocument       = sum2    XmlEndDocument
toXmlEventAll (X.EventBeginDoctype t e) = sum3   (XmlBeginDoctype * t * e)
toXmlEventAll  X.EventEndDoctype        = sum4    XmlEndDoctype
toXmlEventAll (X.EventInstruction i)    = sum5   (XmlInstruction * i)
toXmlEventAll (X.EventBeginElement n a) = sum6   (XmlBeginElement * n * a)
toXmlEventAll (X.EventEndElement n)     = sum7   (XmlEndElement * n)
toXmlEventAll (X.EventContent c)        = sum8   (XmlContent * c)
toXmlEventAll (X.EventComment t)        = sum9   (XmlComment * t)
toXmlEventAll (X.EventCDATA t)          = sum10e (XmlCDATA * t)

type XmlEvent
  = XmlBeginElement * X.Name * [(X.Name, [X.Content])]
  + XmlEndElement * X.Name
  + XmlContent * X.Content



tf1 :: FilePath * [Maybe P.PositionRange * X.Event]
    -> FilePath * [Maybe P.PositionRange * XmlEventAll]
tf1 = _2 %~ fmap (_2 %~ toXmlEventAll)


tf2 ::              ([Maybe P.PositionRange * XmlEventAll] -> e)
  -> FilePath *      [Maybe P.PositionRange * XmlEventAll]
  -> FilePath * (e + [Maybe P.PositionRange * XmlEventAll])
tf2 e = _2 %~ maybeToEither (e . take (length match)) (removePrefixWith (^. _2) match)
  where match = [sum1 XmlBeginDocument]

tf2' ::                          ([Maybe P.PositionRange * XmlEventAll] -> e)
  ->  FilePath *                  [Maybe P.PositionRange * XmlEventAll]
  -> (FilePath * e) + (FilePath * [Maybe P.PositionRange * XmlEventAll])
tf2' e f = distributeProduct (tf2 e f)

tf2'' ::                           ([Maybe P.PositionRange * XmlEventAll] -> e)
  ->  FilePath *                    [Maybe P.PositionRange * XmlEventAll]
  -> (FilePath * [e]) + (FilePath * [Maybe P.PositionRange * XmlEventAll])
tf2'' e = singleErrorContext . tf2' e


tf3 ::              ([Maybe P.PositionRange * XmlEventAll] -> e)
  -> FilePath *      [Maybe P.PositionRange * XmlEventAll]
  -> FilePath * (e + [Maybe P.PositionRange * XmlEventAll])
tf3 e = _2 %~ maybeToEither (e . take (length match)) (removeSuffixWith (^. _2) match)
  where match = [sum2 XmlEndDocument]

tf3' ::                          ([Maybe P.PositionRange * XmlEventAll] -> e)
  ->  FilePath *                  [Maybe P.PositionRange * XmlEventAll]
  -> (FilePath * e) + (FilePath * [Maybe P.PositionRange * XmlEventAll])
tf3' e f = distributeProduct (tf3 e f)

tf3'' ::                           ([Maybe P.PositionRange * XmlEventAll] -> e)
  ->  FilePath *                    [Maybe P.PositionRange * XmlEventAll]
  -> (FilePath * [e]) + (FilePath * [Maybe P.PositionRange * XmlEventAll])
tf3'' e = singleErrorContext . tf3' e


tf4 ::                                       (XmlEventAll -> e)
  -> FilePath *      [Maybe P.PositionRange * XmlEventAll]
  -> FilePath * ([e] +     [P.PositionRange * XmlEventAll])
tf4 e = _2 %~ combineEithers . fmap (maybeToEither (e . snd) (traverseOf _1 id))

tf4' ::                                                     (XmlEventAll -> e)
  ->  FilePath *                    [Maybe P.PositionRange * XmlEventAll]
  -> (FilePath * [e]) + (FilePath * [P.PositionRange       * XmlEventAll])
tf4' e f = distributeProduct (tf4 e f)


tf5 :: FilePath * [P.PositionRange * XmlEventAll]
    -> [FileReference * XmlEventAll]
tf5 (c, xs) = fmap (_1 %~ toFileReference c) xs

tf6 :: (XmlBeginDocument -> e) -> XmlBeginDocument + a -> e + a
tf6 = tryDrop1

tf6a
  :: (XmlBeginDocument -> e)
  -> c * (XmlBeginDocument + a)
  -> c * (e + a)
tf6a e = _2 %~ tf6 e

tf6b
  :: (XmlBeginDocument -> e)
  -> c * (XmlBeginDocument + a)
  -> (c * e) + (c * a)
tf6b e = distributeProduct . tf6a e

tf6'
  :: (XmlBeginDocument -> e)
  -> [c * (XmlBeginDocument + a)]
  -> [c * e] + [c * a]
tf6' e = transformAll (tf6b e)

tf7 :: (XmlEndDocument -> e) -> XmlEndDocument + a -> e + a
tf7 = tryDrop1

tf8 :: (XmlBeginDoctype * Text * (Maybe X.ExternalID) -> e)
    ->  XmlBeginDoctype * Text * (Maybe X.ExternalID) + a
    ->  e + a
tf8 = tryDrop1

tf9 :: (XmlEndDoctype -> e) -> XmlEndDoctype + a -> e + a
tf9 = tryDrop1

tf10 :: (XmlInstruction * X.Instruction -> e)
     ->  XmlInstruction * X.Instruction + a
     ->  e + a
tf10 = tryDrop1

tf11 ::             (XmlComment * Text -> e)
  ->     a + b + c + XmlComment * Text + d
  -> e + a + b + c + d
tf11 = tryDrop4

tf12 ::             (XmlCDATA * Text -> e)
  ->     a + b + c + XmlCDATA * Text
  -> e + a + b + c
tf12 = tryDrop4e


tfShow :: Show x => x -> ErrorMessage
tfShow = ErrorMessage . show

type ErrorMap x x' = x -> [ErrorMessage] + x'
type ErrorMap' x = x -> [ErrorMessage] + x

showErrorMessage :: Show a => String -> a -> ErrorMessage
showErrorMessage m = errorMap ((++) (m ++ " ")) . tfShow

addErrorContext :: Show c => c -> ErrorMessage -> ErrorMessage
addErrorContext c = errorMap ((++) (show c ++ " "))

addErrorContext' :: Show c => c * ErrorMessage -> ErrorMessage
addErrorContext' (c, m) = addErrorContext c m

mapErrorContexts :: Show c => (c * [ErrorMessage]) -> [ErrorMessage]
mapErrorContexts (c, es) = fmap (addErrorContext c) es

tf2p :: ErrorMap' (FilePath * [Maybe P.PositionRange * XmlEventAll])
tf2p = (_Left %~ mapErrorContexts) . tf2'' (showErrorMessage "expected prefix BeginDocument")

tf3p :: ErrorMap' (FilePath * [Maybe P.PositionRange * XmlEventAll])
tf3p = (_Left %~ mapErrorContexts) . tf3'' (showErrorMessage "expected suffix EndDocument")

tf4p :: ErrorMap (FilePath * [Maybe P.PositionRange * XmlEventAll]) (FilePath * [P.PositionRange * XmlEventAll])
tf4p = (_Left %~ mapErrorContexts) . tf4' (showErrorMessage "missing PositionRange")


tryDrop1All
  :: (a -> e)
  -> [c * (a + b)]
  -> [c * e] + [c * b]
tryDrop1All e = transformAll (distributeProduct . (_2 %~ tryDrop1 e))

tryDrop1AllError :: (Show a, Show c) => ErrorMap [c * (a + b)] [c * b]
tryDrop1AllError = (_Left %~ fmap addErrorContext') . tryDrop1All (showErrorMessage "unexpected")

tf6p :: ErrorMap
  [FileReference * XmlEventAll]
  [FileReference *
    ( XmlBeginElement * X.Name * [(X.Name, [X.Content])]
    + XmlEndElement * X.Name
    + XmlContent * X.Content
    + XmlComment * Text
    + XmlCDATA * Text)]
tf6p x = tryDrop1AllError x >>= tryDrop1AllError >>= tryDrop1AllError >>= tryDrop1AllError >>= tryDrop1AllError

readEvents' :: FilePath -> IO (FilePath * [Maybe P.PositionRange * XmlEventAll])
readEvents' p = readEvents p >>= return . tf1 . (,) p
