{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Text.Greek.Source.Sblgnt where

import Prelude hiding ((*), (+), error)
import Conduit
import Control.Lens
import Data.Text (Text)
import qualified Data.Conduit.Attoparsec as P
import qualified Data.XML.Types as X
import qualified Text.XML.Stream.Parse as P

readEvents :: FilePath -> IO [(Maybe P.PositionRange, X.Event)]
readEvents fp = runResourceT $ sourceFile fp =$= P.parseBytesPos P.def $$ sinkList


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


newtype ErrorMessage = ErrorMessage { getErrorMessage :: String }
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



type a + b = Either a b
infixr 6 +

type a * b = (a, b)
infixr 7 *

(*) :: a -> b -> a * b
(*) = (,)



type Item              context item =                      context * item
type ResultItem  error context item = [context * error] +  context * item
type Items             context item =                     [context * item]
type ResultItems error context item = [context * error] + [context * item]

mapItem :: (i -> i') -> Item c i -> Item c i'
mapItem = fmap

tryMapItem :: (i -> [e] + i') -> Item c i -> ResultItem e c i'
tryMapItem f x@(c, _) = _Left %~ fmap ((,) c) $ traverse f x

tryMapItem' :: (i -> e) -> (i -> Maybe i') -> Item c i -> ResultItem e c i'
tryMapItem' e f (c, i) = case f i of
  Just i' -> Right (c * i')
  Nothing -> Left [(c * e i)]

mapItems :: (i -> i') -> Items c i -> Items c i'
mapItems = fmap . mapItem

tryMapItems :: (i -> [e] + i') -> Items c i -> ResultItems e c i'
tryMapItems f = foldr combineEitherList (Right []) . fmap (tryMapItem f)

tryMapItems' :: (i -> e) -> (i -> Maybe i') -> Items c i -> ResultItems e c i'
tryMapItems' e f = foldr combineEitherList (Right []) . fmap (tryMapItem' e f)


combineEitherList :: [a] + b -> [a] + [b] -> [a] + [b]
combineEitherList (Left as)   (Left  as') = Left (as ++ as')
combineEitherList (Left as)   (Right _  ) = Left as
combineEitherList (Right _) e@(Left  _  ) = e
combineEitherList (Right b)   (Right bs ) = Right (b : bs)






sum1 :: a1 -> a1 + a2
sum1 = Left
sum2 :: a2 -> a1 + a2 + a3
sum2 = Right . sum1
sum3 :: a3 -> a1 + a2 + a3 + a4
sum3 = Right . sum2
sum4 :: a4 -> a1 + a2 + a3 + a4 + a5
sum4 = Right . sum3
sum5 :: a5 -> a1 + a2 + a3 + a4 + a5 + a6
sum5 = Right . sum4
sum6 :: a6 -> a1 + a2 + a3 + a4 + a5 + a6 + a7
sum6 = Right . sum5
sum7 :: a7 -> a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8
sum7 = Right . sum6
sum8 :: a8 -> a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9
sum8 = Right . sum7
sum9 :: a9 -> a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10
sum9 = Right . sum8
sum10 :: a10 -> a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11
sum10 = Right . sum9

sum2e :: a2 -> a1 + a2
sum2e = Right
sum3e :: a3 -> a1 + a2 + a3
sum3e = Right . sum2e
sum4e :: a4 -> a1 + a2 + a3 + a4
sum4e = Right . sum3e
sum5e :: a5 -> a1 + a2 + a3 + a4 + a5
sum5e = Right . sum4e
sum6e :: a6 -> a1 + a2 + a3 + a4 + a5 + a6
sum6e = Right . sum5e
sum7e :: a7 -> a1 + a2 + a3 + a4 + a5 + a6 + a7
sum7e = Right . sum6e
sum8e :: a8 -> a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8
sum8e = Right . sum7e
sum9e :: a9 -> a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9
sum9e = Right . sum8e
sum10e :: a10 -> a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10
sum10e = Right . sum9e
sum11e :: a11 -> a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11
sum11e = Right . sum10e


prism1 :: Prism (a1 + a2) (a1' + a2) a1 a1'
prism1 = _Left
prism2 :: Prism (a1 + a2 + a3) (a1 + a2' + a3) a2 a2'
prism2 = _Right . prism1
prism3 :: Prism (a1 + a2 + a3 + a4) (a1 + a2 + a3' + a4) a3 a3'
prism3 = _Right . prism2
prism4 :: Prism (a1 + a2 + a3 + a4 + a5) (a1 + a2 + a3 + a4' + a5) a4 a4'
prism4 = _Right . prism3

prism2e :: Prism (a1 + a2) (a1 + a2') a2 a2'
prism2e = _Right
prism3e :: Prism (a1 + a2 + a3) (a1 + a2 + a3') a3 a3'
prism3e = _Right . prism2e
prism4e :: Prism (a1 + a2 + a3 + a4) (a1 + a2 + a3 + a4') a4 a4'
prism4e = _Right . prism3e

get2e :: a1 + a2 -> a2 + a1
get2e (Left a1) = Right a1
get2e (Right a2) = Left a2

shiftLeft :: a + (b + c) -> (a + b) + c
shiftLeft (Left a) = Left . Left $ a
shiftLeft (Right (Left b)) = Left . Right $ b
shiftLeft (Right (Right c)) = Right c

get2 :: a1 + a2 + a3 -> a2 + a1 + a3
get2 (Left a1) = Right . Left $ a1
get2 (Right (Left a2)) = Left a2
get2 (Right (Right a3)) = Right . Right $ a3

get3 :: a1 + a2 + a3 + a4 -> a3 + a1 + a2 + a4
get3 = get2 . over _Right get2
get4 :: a1 + a2 + a3 + a4 + a5 -> a4 + a1 + a2 + a3 + a5
get4 = get2 . over _Right get3



tryDrop1 :: (a1 -> e) -> a1 + a2 -> e + a2
tryDrop1 = over prism1
tryDrop2 :: (a2 -> e) -> a1 + a2 + a3 -> e + a1 + a3
tryDrop2 f = get2 . over prism2 f
tryDrop3 :: (a3 -> e) -> a1 + a2 + a3 + a4 -> e + a1 + a2 + a4
tryDrop3 f = get3 . over prism3 f
tryDrop4 :: (a4 -> e) -> a1 + a2 + a3 + a4 + a5 -> e + a1 + a2 + a3 + a5
tryDrop4 f = get4 . over prism4 f

tryDrop2e :: (b -> e) -> a + b -> e + a
tryDrop2e f = get2e . over prism2e f


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

em :: Show a => (a -> ErrorMessage)
em = ErrorMessage . show


tf1 :: FilePath * [Maybe P.PositionRange * X.Event] -> FilePath * [Maybe P.PositionRange * XmlEventAll]
tf1 = _2 %~ fmap (_2 %~ toXmlEventAll)

tf2 ::              ([Maybe P.PositionRange * XmlEventAll] -> e)
  -> FilePath *      [Maybe P.PositionRange * XmlEventAll]
  -> FilePath * (e + [Maybe P.PositionRange * XmlEventAll])
tf2 e (c, xs) = c * maybeToEither (e . take (length match)) (removePrefixWith (^. _2) match) xs
  where match = [sum1 XmlBeginDocument]

tf3 ::              ([Maybe P.PositionRange * XmlEventAll] -> e)
  -> FilePath *      [Maybe P.PositionRange * XmlEventAll]
  -> FilePath * (e + [Maybe P.PositionRange * XmlEventAll])
tf3 e (c, xs) = c * maybeToEither (e . take (length match)) (removeSuffixWith (^. _2) match) xs
  where match = [sum2 XmlEndDocument]
