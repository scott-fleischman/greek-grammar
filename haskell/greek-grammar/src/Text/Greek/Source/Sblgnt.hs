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


data Error a = Error
  { _errorContext :: a
  , _errorMessage :: ErrorMessage
  }
makeLenses ''Error

makeError :: a -> String -> Error a
makeError c m = Error c (ErrorMessage m)

maybeToError :: c -> String -> Maybe a -> Either (Error c) a
maybeToError c m x = case x of
  Just x' -> Right x'
  Nothing -> Left . makeError c $ m

errorToString :: (a -> String) -> Error a -> String
errorToString f (Error c (ErrorMessage m)) = f c ++ " " ++ m

initialProcessEvents :: FilePath -> [(Maybe P.PositionRange, X.Event)] -> Either (Error FilePath) [(FileReference, X.Event)]
initialProcessEvents fp xs
    = (e "Missing prefix BeginDocument" . removePrefixWith snd [X.EventBeginDocument] $ xs)
  >>= (e "Missing suffix EndDocument"   . removeSuffixWith snd [X.EventEndDocument])
  >>= (e "Missing PositionRange"        . traverse (withFileReference fp))
  where e = maybeToError fp


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


withFileReference :: FilePath -> (Maybe P.PositionRange, X.Event) -> Maybe (FileReference, X.Event)
withFileReference fp = traverseOf _1 (toFileReference fp <$>)


data XNCEvent
  = XNCBeginElement X.Name [(X.Name, [X.Content])]
  | XNCEndElement X.Name
  | XNCContent X.Content
  deriving (Show)

toXNCEvent :: X.Event -> Maybe XNCEvent
toXNCEvent (X.EventBeginElement n as) = Just $ XNCBeginElement n as
toXNCEvent (X.EventEndElement n)      = Just $ XNCEndElement n
toXNCEvent (X.EventContent c)         = Just $ XNCContent c
toXNCEvent _                          = Nothing


toXNCEventContext :: (FileReference, X.Event) -> Maybe (FileReference, XNCEvent)
toXNCEventContext = traverseOf _2 toXNCEvent

tryMap :: Either [Error x] a -> Maybe b -> Error x -> (b -> a -> c) -> Either [Error x] c
tryMap (Left  es) Nothing  e _ = Left  (e : es)
tryMap (Left  es) (Just _) _ _ = Left  es
tryMap (Right _ ) Nothing  e _ = Left  [e]
tryMap (Right a ) (Just b) _ f = Right (f b a)

tryConvertAll :: (a -> Error c) -> (a -> Maybe b) -> [a] -> Either [Error c] [b]
tryConvertAll e f xs = foldr (\a b -> tryMap b (f a) (e a) (:)) (Right []) xs

showError :: Show b => String -> (a, b) -> Error a
showError m (x, y) = Error x (ErrorMessage (m ++ " " ++ show y))

toXNCEventsError :: [(FileReference, X.Event)] -> Either [Error FileReference] [(FileReference, XNCEvent)]
toXNCEventsError = tryConvertAll (showError "Unexpected event") toXNCEventContext


newtype ElementName = ElementName { getElementName :: Text }
newtype AttributeName = AttributeName { getAttributeName :: Text }
data XCEvent
  = XCBeginElement ElementName [(AttributeName, [X.Content])]
  | XCEndElement ElementName
  | XCContent X.Content

nameToText :: X.Name -> Maybe Text
nameToText (X.Name t Nothing Nothing) = Just t
nameToText _ = Nothing


toAttributeName :: (X.Name, a) -> Maybe (AttributeName, a)
toAttributeName = traverseOf _1 (fmap AttributeName . nameToText)

toAttributeNameError :: [(X.Name, a)] -> Either [Error ()] [(AttributeName, a)]
toAttributeNameError = tryConvertAll (showError "Unexpected attribute namespace" . ((,) ()) . fst) toAttributeName

toElementName :: X.Name -> Maybe ElementName
toElementName = fmap ElementName . nameToText

ensureNoNamespaces :: XNCEvent -> Either [Error ()] XCEvent
ensureNoNamespaces (XNCBeginElement n as) = tryMap (toAttributeNameError as) (ElementName <$> nameToText n) (makeError () ("Unexpected element namespace" ++ show n)) XCBeginElement
ensureNoNamespaces (XNCEndElement n) = fmap XCEndElement (_Left %~ pure $ (maybeToError () ("Unexpected element namespace " ++ show n) (ElementName <$> nameToText n)))
ensureNoNamespaces (XNCContent c) = pure (XCContent c)

ensureNoNamespacesContext :: (FileReference, XNCEvent) -> Either [Error FileReference] (FileReference, XCEvent)
ensureNoNamespacesContext (x, y) = (_Left %~ (fmap (errorContext .~ x))) . (_Right %~ ((,) x)) $ (ensureNoNamespaces y)

ensureNoNamespacesAll :: [(FileReference, XNCEvent)] -> Either [Error FileReference] [(FileReference, XCEvent)]
ensureNoNamespacesAll = foldr combineEitherList (Right []) . fmap ensureNoNamespacesContext









makeErrorMessage :: (Show c, Show a) => c -> String -> a -> ErrorMessage
makeErrorMessage c s i = ErrorMessage $ show c ++ " " ++ s ++ " " ++ show i

data ShowApp e = ShowApp
  { appTryDrop1 :: forall a1 a2. Show a1 => a1 + a2 -> e + a2
  , appTryDrop2 :: forall a1 a2 a3. Show a2 => a1 + a2 + a3 -> e + a1 + a3
  , appTryDrop3 :: forall a1 a2 a3 a4. Show a3 => a1 + a2 + a3 + a4 -> e + a1 + a2 + a4
  , appTryDrop4 :: forall a1 a2 a3 a4 a5. Show a4 => a1 + a2 + a3 + a4 + a5 -> e + a1 + a2 + a3 + a5
  , toError :: String -> e
  }

simpleShowApp :: (Show c) => c -> ShowApp ErrorMessage
simpleShowApp c = ShowApp
  { appTryDrop1 = tryDrop1 (makeErrorMessage c unexpected)
  , appTryDrop2 = tryDrop2 (makeErrorMessage c unexpected)
  , appTryDrop3 = tryDrop3 (makeErrorMessage c unexpected)
  , appTryDrop4 = tryDrop4 (makeErrorMessage c unexpected)
  , toError = ErrorMessage
  }
  where unexpected = "Unexpected"

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

mapContext :: (Item c i -> c') -> Items c i -> Items c' i
mapContext f = fmap (\x@(_, i) -> f x * i)



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

type PartialTransform e c x x' = c * x -> e + (c * x')
type PartialTransform' e c x = PartialTransform e c x x

showErrorMessage :: Show a => String -> a -> ErrorMessage
showErrorMessage m = errorMap (m ++) . tfShow

addErrorContext :: Show c => c -> ErrorMessage -> ErrorMessage
addErrorContext c = errorMap ((++) (show c ++ " "))

mapErrorContexts :: Show c => (c * [ErrorMessage]) -> [ErrorMessage]
mapErrorContexts (c, es) = fmap (addErrorContext c) es

tf2p :: PartialTransform' [ErrorMessage] FilePath [Maybe P.PositionRange * XmlEventAll]
tf2p = (_Left %~ mapErrorContexts) . tf2'' (showErrorMessage "missing BeginDocument")

tf3p :: PartialTransform' [ErrorMessage] FilePath [Maybe P.PositionRange * XmlEventAll]
tf3p = (_Left %~ mapErrorContexts) . tf3'' (showErrorMessage "missing EndDocument")

tf4p :: PartialTransform [ErrorMessage] FilePath [Maybe P.PositionRange * XmlEventAll] [P.PositionRange * XmlEventAll]
tf4p = (_Left %~ mapErrorContexts) . tf4' (showErrorMessage "Missing PositionRange")

readEvents' :: FilePath -> IO (FilePath * [(Maybe P.PositionRange, XmlEventAll)])
readEvents' p = readEvents p >>= return . tf1 . (,) p
