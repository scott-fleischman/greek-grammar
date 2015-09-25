{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Greek.Utility where

import Prelude hiding ((*), (+))
import Control.Lens
import Data.Map (Map)
import Data.Tuple
import qualified Data.Map as M

type a + b = Either a b
infixr 6 +
type a * b = (a, b)
infixr 7 *

(>>.) :: forall a b m. Functor m => m a -> (a -> b) -> m b
(>>.) = flip fmap
infixl 1 >>.

maybeToEither :: a -> Maybe b -> a + b
maybeToEither a Nothing = Left a
maybeToEither _ (Just b) = Right b

addSplit :: a + b -> [a] + [b] -> [a] + [b]
addSplit (Left  a)   (Left  as') = Left (a : as')
addSplit (Left  a)   (Right _  ) = Left [a]
addSplit (Right _) e@(Left  _  ) = e
addSplit (Right b)   (Right bs ) = Right (b : bs)

split :: [a + b] -> [a] + [b]
split = foldr addSplit (Right [])

splitMap :: (a -> e + b) -> [a] -> [e] + [b]
splitMap f = split . fmap f


tryOver
  :: LensLike (Either e') s t a b -> (a -> e' + b)
  -> (s -> e' -> e)
  -> s
  -> e + t
tryOver l f e x = over _Left handleError basicTraverse
  where
    handleError = e x
    basicTraverse = l f x

tryOverAll
  :: LensLike (Either e') s t a b -> (a -> e' + b)
  -> (s -> e' -> e)
  -> [s]
  -> [e] + [t]
tryOverAll l f e = splitMap (tryOver l f e)


errorContext :: (c -> a -> e) -> Getter s c -> s -> (a -> e)
errorContext e g = e . (view g)


consValue :: Ord b => a -> b -> Map b [a] -> Map b [a]
consValue a b m = case M.lookup b m of
  Just as -> M.insert b (a : as) m
  Nothing -> M.insert b [a] m

mapGroupBy :: forall a b. (Ord b) => (a -> b) -> [a] -> Map b [a]
mapGroupBy f = foldr g M.empty where
  g :: a -> Map b [a] -> Map b [a]
  g a = consValue a (f a)

query :: (Ord b) => (a -> b) -> [a] -> [b * [a]]
query f = M.toList . mapGroupBy f

concatMapGroupBy :: forall a b. (Ord b) => (a -> [b]) -> [a] -> Map b [a]
concatMapGroupBy f = foldr (\(a, b) -> consValue a b) M.empty . concatMap (_2 id) . fmap (\s -> (s, f s))

concatQuery :: (Ord b) => (a -> [b]) -> [a] -> [b * [a]]
concatQuery f = M.toList . concatMapGroupBy f



partialMapGroupBy :: forall a b e. (Ord b) => (a -> e + b) -> [a] -> [a] * Map b [a]
partialMapGroupBy f = foldr g ([], M.empty) where
  g :: a -> [a] * Map b [a] -> [a] * Map b [a]
  g a = case f a of
    Left _  -> _1 %~ (a :)
    Right b -> _2 %~ consValue a b

partialQuery :: (Ord b) => (a -> e + b) -> [a] -> [a] * [b * [a]]
partialQuery f = (_2 %~ M.toList) . partialMapGroupBy f

choose :: forall a b e. (a -> e + b) -> [a] -> [b]
choose f = foldr g [] where
  g :: a -> [b] -> [b]
  g a bs = case f a of
    Left _  -> bs
    Right b -> b : bs

choose' :: forall a b. (a -> Maybe b) -> [a] -> [b]
choose' f = choose (maybeToEither () . f)


liftErrors :: Each a0 b0 a1 b1 => (a1 -> b1) -> a0 + c -> b0 + c
liftErrors = over (_Left . each)

liftError :: (a -> a1) -> a + c -> [a1] + c
liftError f = over _Left (pure . f)

single :: e -> (s -> e) -> [s] -> [e] + s
single e _ [] = Left . pure $ e
single _ _ [s] = Right s
single _ f (_ : ss) = Left (fmap f ss)

empty :: [s] -> [s] + ()
empty [] = Right ()
empty xs = Left xs

distributeProductRight :: a * (b + c) -> (a * b) + (a * c)
distributeProductRight (a, Left  b) = Left  (a, b)
distributeProductRight (a, Right c) = Right (a, c)

distributeProductLeft :: (a + b) * c -> (a * c) + (b * c)
distributeProductLeft = over _Right swap . over _Left swap . distributeProductRight . swap

extractSum :: (a + b) * (a + c) -> a + b * c
extractSum (Left a, _) = Left a
extractSum (_, Left a) = Left a
extractSum (Right b, Right c) = Right (b, c)

extractProduct :: (a * b) + (a * c) -> a * (b + c)
extractProduct (Left (a, b))  = (a, Left b)
extractProduct (Right (a, c)) = (a, Right c)

groupEithers :: [a + b] -> [[a] + [b]]
groupEithers = foldr go []
  where
    go :: Either a b -> [Either [a] [b]] -> [Either [a] [b]]
    go (Left a) ((Left as) : xs) = (Left (a : as)) : xs
    go (Left a) xs = (Left [a]) : xs
    go (Right b) ((Right bs) : xs) = (Right (b : bs)) : xs
    go (Right b) xs = (Right [b]) : xs
