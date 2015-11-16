{-# LANGUAGE RankNTypes #-}

module Text.Greek.Script.Place where

import qualified Data.Set as Set

data Initial = IsInitial | NotInitial deriving (Eq, Ord, Show)
data Medial = IsMedial | NotMedial deriving (Eq, Ord, Show)
data Final = IsFinal | NotFinal deriving (Eq, Ord, Show)

type Place3 = (Initial, Medial, Final)

initialFinal :: Place3
initialFinal = (IsInitial, NotMedial, IsFinal)

initial :: Place3
initial = (IsInitial, NotMedial, NotFinal)

final :: Place3
final = (NotInitial, NotMedial, IsFinal)

medial :: Place3
medial = (NotInitial, IsMedial, NotFinal)

getInitialSet :: Ord c => [(c, Place3)] -> Set.Set c
getInitialSet = Set.fromList . concatMap go
  where
    go (c, p) | (IsInitial,_,_) <- p = [c]
    go _ = []

data AttestedInitial = AttestedInitial | UnattestedInitial deriving (Eq, Ord, Show)

type Place4 = (Initial, Medial, Final, AttestedInitial)

applyAttestation :: Ord c => Set.Set c -> (c, Place3) -> (c, Place4)
applyAttestation ss (c, p)
  | (_,IsMedial,_) <- p
  , Set.member c ss
  = (c, addAttestedInitial p AttestedInitial)
applyAttestation _ (c, p) = (c, addAttestedInitial p UnattestedInitial)

addAttestedInitial :: Place3 -> AttestedInitial -> Place4
addAttestedInitial (a, b, c) d = (a, b, c, d)
