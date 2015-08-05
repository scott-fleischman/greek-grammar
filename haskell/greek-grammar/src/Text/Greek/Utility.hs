{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Greek.Utility where

import Prelude hiding ((*), (+), getLine)
import Control.Lens
import Data.List (intersperse)
import Data.String
import Data.Text (Text, unpack)
import Data.Tuple

type a + b = Either a b
infixr 6 +

type a * b = (a, b)
infixr 7 *

(*) :: a -> b -> a * b
(*) = (,)

addSplit :: a + b -> [a] + [b] -> [a] + [b]
addSplit (Left  a)   (Left  as') = Left (a : as')
addSplit (Left  a)   (Right _  ) = Left [a]
addSplit (Right _) e@(Left  _  ) = e
addSplit (Right b)   (Right bs ) = Right (b : bs)

split :: [a + b] -> [a] + [b]
split = foldr addSplit (Right [])


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

get3e :: a1 + a2 + a3 -> a3 + a1 + a2
get3e = get2 . over _Right get2e
get4e :: a1 + a2 + a3 + a4 -> a4 + a1 + a2 + a3
get4e = get2 . over _Right get3e

shiftLeftSum :: a + (b + c) -> (a + b) + c
shiftLeftSum (Left a) = Left . Left $ a
shiftLeftSum (Right (Left b)) = Left . Right $ b
shiftLeftSum (Right (Right c)) = Right c

shiftLeftProduct :: a * (b * c) -> (a * b) * c
shiftLeftProduct (a, (b, c)) = (a * b) * c

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

tryDrop2e :: (a2 -> e) -> a1 + a2 -> e + a1
tryDrop2e f = get2e . over prism2e f
tryDrop3e :: (a3 -> e) -> a1 + a2 + a3 -> e + a1 + a2
tryDrop3e f = get3e . over prism3e f
tryDrop4e :: (a4 -> e) -> a1 + a2 + a3 + a4 -> e + a1 + a2 + a3
tryDrop4e f = get4e . over prism4e f


newtype ErrorMessage = ErrorMessage { getErrorMessage :: String } deriving (Show)

class Handler e a where
  handle :: a -> e

concatErrors :: [ErrorMessage] -> ErrorMessage
concatErrors = ErrorMessage . concat . fmap getErrorMessage

instance IsString ErrorMessage where
  fromString = ErrorMessage

distributeProductRight :: a * (b + c) -> (a * b) + (a * c)
distributeProductRight (a, Left  b) = Left  (a * b)
distributeProductRight (a, Right c) = Right (a * c)

distributeProductLeft :: (a + b) * c -> (a * c) + (b * c)
distributeProductLeft = over _Right swap . over _Left swap . distributeProductRight . swap

singleErrorContext :: (a * e) + b -> (a * [e]) + b
singleErrorContext = _Left %~ (_2 %~ pure)

(>.) :: (a -> b) -> (b -> c) -> (a -> c)
(>.) = flip (.)
infixr 9 >.

(>>.) :: forall a b m. Functor m => m a -> (a -> b) -> m b
(>>.) = flip fmap
infixl 1 >>.


-- transformAll :: (a -> e + b) -> [a] -> [e] + [b]
-- transformAll f = combineEithers . fmap f

instance (Handler e a) => Handler [e] a where
  handle = pure . handle

instance (Handler ErrorMessage a, Handler ErrorMessage b) => Handler ErrorMessage (a * b) where
  handle (a, b) = concatErrors [handle a, " ", handle b]
instance (Handler ErrorMessage a, Handler ErrorMessage b) => Handler ErrorMessage (a + b) where
  handle (Left a) = handle a
  handle (Right b) = handle b
instance Handler ErrorMessage Char where
  handle = fromString . pure
instance Handler ErrorMessage Int where
  handle = fromString . show
instance Handler ErrorMessage a => Handler ErrorMessage [a] where
  handle = concatErrors . intersperse "," . fmap handle
instance Handler ErrorMessage a => Handler ErrorMessage (Maybe a) where
  handle Nothing = "-"
  handle (Just a) = handle a
instance Handler ErrorMessage Text where
  handle = fromString . unpack


newtype Line = Line { getLine :: Int } deriving (Eq, Ord, Show)
instance Handler ErrorMessage Line where handle (Line l) = concatErrors ["line:", handle l]

newtype Column = Column { getColumn :: Int } deriving (Eq, Ord, Show)
instance Handler ErrorMessage Column where handle (Column c) = concatErrors ["column:", handle c]

type LineReference = Line * Column
type LineReferenceRange = LineReference + LineReference * LineReference
type FileReference = FilePath * LineReferenceRange


-- contextPartialMap :: (a -> e + b) -> c * a -> c * e + c * b
-- contextPartialMap f = distributeProductRight . over _2 f

-- contextPartialMapContext :: (c -> e + c') -> c * a -> a * e + c' * a
-- contextPartialMapContext f = (_Left %~ swap) . distributeProductLeft . over _1 f

-- partialMapError :: Display c => (a -> ErrorMessage + b) -> c * a -> ErrorMessage + c * b
-- partialMapError f = (_Left %~ log) . contextPartialMap f

-- partialMapErrors :: Display c => (a -> ErrorMessage + b) -> [c * a] -> [ErrorMessage] + [c * b]
-- partialMapErrors f = combineEithers . fmap (partialMapError f)

-- partialMapContext :: Display a => (c -> ErrorMessage + c') -> c * a -> ErrorMessage + c' * a
-- partialMapContext f = (_Left %~ log) . contextPartialMapContext f

-- partialMapContexts :: Display a => (c -> ErrorMessage + c') -> [c * a] -> [ErrorMessage] + [c' * a]
-- partialMapContexts f = combineEithers . fmap (partialMapContext f)
