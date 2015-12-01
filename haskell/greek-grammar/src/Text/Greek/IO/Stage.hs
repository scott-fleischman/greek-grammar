{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Greek.IO.Stage where

import Data.Text (Text)
import qualified Control.Lens as Lens
import qualified Data.Functor.Identity as Functor
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text.Lazy as Lazy
import qualified Text.Greek.IO.Json as Json
import qualified Text.Greek.IO.Render as Render
import qualified Text.Greek.IO.Type as Type
import qualified Text.Greek.Script.Word as Word
import qualified Text.Greek.Source.Work as Work
import qualified Text.Greek.Utility as Utility

type WordSurface a b = [Work.Indexed [Word.Word a b]]
type WordSurfaceBasic a = WordSurface Word.Basic a

data Stage a = Stage
  { stagePrimaryType :: a
  , stagePartTypes :: [a]
  }

makeSimpleValue :: Render.Render a => a -> Value
makeSimpleValue = ValueSimple . Lazy.toStrict . Render.render

makeWorkInfoType :: (Ord a, Render.Render a) => Json.TypeKind -> Type.Name -> (Work.IndexSourceTitle -> a)
  -> [Work.Indexed [Word.Word (Word.IndexedP b) c]] -> TypeData
makeWorkInfoType k t f = generateType k t makeSimpleValue . flattenWords (\x _ -> f x)

makeWordPartType :: (Ord b, Render.Render b) => Json.TypeKind -> Type.Name -> (Word.Word (Word.IndexedP c) a -> [b])
  -> WordSurface (Word.IndexedP c) a -> TypeData
makeWordPartType k t f = generateType k t makeSimpleValue . flatten . flattenWords (\_ x -> f x)
  where flatten = concatMap (\(l, m) -> fmap (\x -> (l, x)) m)

makeSurfaceType :: (Ord a, Render.Render a) => Json.TypeKind -> Type.Name
  -> WordSurface (Word.IndexedP b) [a] -> TypeData
makeSurfaceType k t = generateType k t makeSimpleValue . flattenSurface

makeSurfacePartType :: (Ord b, Render.Render b) => Json.TypeKind -> Type.Name -> (a -> [b])
  -> WordSurface (Word.IndexedP c) [a] -> TypeData
makeSurfacePartType k t f = generateType k t makeSimpleValue . extract . flattenSurface
  where extract = concatMap (\(l, m) -> fmap (\x -> (l, x)) (f m))

makeIndexedSurfacePartType :: (Ord (b, i), Render.Render (b, i)) => Json.TypeKind -> Type.Name -> (Int -> i) -> (a -> b)
  -> WordSurface (Word.IndexedP c) [a] -> TypeData
makeIndexedSurfacePartType k t g f
  = generateType k (Type.Indexed t) makeSimpleValue
  . Lens.over (traverse . Lens._2 . Lens._2) g
  . concatIndexedSnd
  . flattenWords (\_ -> fmap f . Word.getSurface)

makeReverseIndexedSurfacePartType2 :: (Ord (b, i), Render.Render (b, i)) => Json.TypeKind -> Type.Name -> (Int -> i) -> (a -> [Maybe b])
  -> WordSurface (Word.IndexedP c) a -> TypeData
makeReverseIndexedSurfacePartType2 k t g f
  = generateType k (Type.ReverseIndexed t) makeSimpleValue
  . Lens.over (traverse . Lens._2 . Lens._2) g
  . Maybe.mapMaybe ((Lens._2 . Lens._1) id)
  . concatReverseIndexedSnd
  . flattenWords (\_ -> f . Word.getSurface)

makeReverseIndexedSurfacePartType :: (Ord (b, i), Render.Render (b, i)) => Json.TypeKind -> Type.Name -> (Int -> i) -> (a -> b)
  -> WordSurface (Word.IndexedP c) [a] -> TypeData
makeReverseIndexedSurfacePartType k t g f
  = generateType k (Type.ReverseIndexed t) makeSimpleValue
  . Lens.over (traverse . Lens._2 . Lens._2) g
  . concatReverseIndexedSnd
  . flattenWords (\_ -> fmap f . Word.getSurface)

dupApply' :: ((d -> Functor.Identity (d, b)) -> a -> Functor.Identity c) -> (d -> b) -> a -> c
dupApply' a b = Functor.runIdentity . dupApply a (Functor.Identity . b)

dupApply :: Functor f => ((a -> f (a, b)) -> t) -> (a -> f b) -> t
dupApply lens f = lens (apply . dup)
  where
    apply = Lens._2 f
    dup x = (x, x)

wordSurfaceLens :: Applicative f =>
  (a -> f b)
  -> [Work.Indexed [Word.Word c a]]
  -> f [Work.Indexed [Word.Word c b]]
wordSurfaceLens = traverse . Work.content . traverse . Word.surface

type WordLocation = (Work.Index, Word.Index)
data Value
  = ValueSimple Text
  deriving (Eq, Ord, Show)
data TypeData = TypeData
  { typeDataName :: Type.Name
  , typeDataJson :: Json.Type
  }

generateType :: forall a. Ord a => Json.TypeKind -> Type.Name -> (a -> Value) -> [(Json.Instance, a)] -> TypeData
generateType k t f is = TypeData t $ Json.Type (Lazy.toStrict . Render.render $ t) k (fmap storeValue typedValueInstances)
  where
    valueInstances :: [(a, [Json.Instance])]
    valueInstances = Lens.over (traverse . Lens._2 . traverse) fst . Map.assocs . Utility.mapGroupBy snd $ is

    typedValueInstances :: [(Value, [Json.Instance])]
    typedValueInstances = Lens.over (traverse . Lens._1) f valueInstances

    storeValue :: (Value, [Json.Instance]) -> Json.Value
    storeValue ((ValueSimple vt), ls) = Json.Value vt ls

flattenSurface :: forall a b. [Work.Indexed [Word.Word (Word.IndexedP a) [b]]] -> [(Json.Instance, b)]
flattenSurface = concatInstanceValues . flattenWords (\_ -> Word.getSurface)

concatInstanceValues :: [(Json.Instance, [b])] -> [(Json.Instance, b)]
concatInstanceValues = concatMap (\(x, ys) -> mapAtomIndexes x ys)

mapAtomIndexes :: Json.Instance -> [t] -> [(Json.Instance, t)]
mapAtomIndexes a = fmap (\(i, y) -> (setAtomIndex i a, y)) . zip [0..]
  where
    setAtomIndex z (Json.Instance x y _) = Json.Instance x y (Just . Json.AtomIndex $ z)

concatIndexedSnd :: [(Json.Instance, [b])] -> [(Json.Instance, (b, Int))]
concatIndexedSnd = concatMap (\(x, ys) -> fmap (\(i, (a, b)) -> (a, (b, i))) . zip [0..] . mapAtomIndexes x $ ys)

concatReverseIndexedSnd :: [(Json.Instance, [b])] -> [(Json.Instance, (b, Int))]
concatReverseIndexedSnd = concatMap (\(x, ys) -> reverse . fmap (\(i, (a, b)) -> (a, (b, i))) . zip [0..] . reverse . mapAtomIndexes x $ ys)

flattenWords :: forall a b c. (Work.IndexSourceTitle -> Word.Word (Word.IndexedP a) b -> c)
  -> [Work.Indexed [Word.Word (Word.IndexedP a) b]]
  -> [(Json.Instance, c)]
flattenWords f = concatMap getIndexedWorkProps
  where
    getIndexedWorkProps :: Work.Indexed [Word.Word (Word.IndexedP a) b] -> [(Json.Instance, c)]
    getIndexedWorkProps w = fmap (\(i, p) -> (Json.Instance (getWorkIndex w) i Nothing, p)) (getWorkProps w)

    getWorkProps :: Work.Indexed [Word.Word (Word.IndexedP a) b] -> [(Word.Index, c)]
    getWorkProps k = fmap (getIndexedWordProp (Work.getInfo k)) . Work.getContent $ k

    getWorkIndex :: Work.Indexed x -> Work.Index
    getWorkIndex = Lens.view (Work.info . Lens._1)

    getIndexedWordProp :: Work.IndexSourceTitle -> Word.Word (Word.IndexedP a) b -> (Word.Index, c)
    getIndexedWordProp k d = (getWordIndex d, f k d)

    getWordIndex :: Word.Word (Word.IndexedP a) b -> Word.Index
    getWordIndex = Lens.view (Word.info . Word.indexLens)
