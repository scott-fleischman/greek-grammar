module Text.Greek.Script.Place where

data Initial = IsInitial | NotInitial deriving (Eq, Ord, Show)
data Medial = IsMedial | NotMedial deriving (Eq, Ord, Show)
data Final = IsFinal | NotFinal deriving (Eq, Ord, Show)

data Place = Place
  { getPlaceInitial :: Initial
  , getPlaceMedial :: Medial
  , getPlaceFinal :: Final
  } deriving (Eq, Ord, Show)

initialFinal :: Place
initialFinal = Place IsInitial NotMedial IsFinal

initial :: Place
initial = Place IsInitial NotMedial NotFinal

final :: Place
final = Place NotInitial NotMedial IsFinal

medial :: Place
medial = Place NotInitial IsMedial NotFinal
