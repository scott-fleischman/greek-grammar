{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Sublist where

import Text.Greek.Utility

data Error s b b' e e' c p
  = ErrorOnlyBegin b'
  | ErrorOnlyEnd e'
  | ErrorInvalidBegin b
  | ErrorInvalidEnd e
  | ErrorNestedBegin b b'
  | ErrorNestedEnd e e'
  | ErrorInvalidItem b' e' [c]
  | ErrorInvalidChild p
  deriving (Show)

data TopLevel b e t
  = TopLevelBegin b
  | TopLevelEnd e
  | TopLevelPass t

data Builder s b b' e e' c p t = Builder
  { splitItem :: s -> TopLevel b e p
  , refineBegin :: b -> Maybe b'
  , refineEnd :: e -> Maybe e'
  , getChild :: p -> Maybe c
  , create :: b' -> e' -> [c] -> Maybe t
  , passItem :: p -> t
  }

foldrSublist :: Builder s b b' e e' c p t -> [s] -> Error s b b' e e' c p + [t]
foldrSublist builder items = case foldr (buildSublistR builder) (StateROutside []) items of
  StateRError e -> Left e
  StateRInside end _ _ -> Left (ErrorOnlyEnd end)
  StateROutside outside -> Right outside

data StateR s b b' e e' c p t
  = StateRError (Error s b b' e e' c p)
  | StateRInside e' [c] [t]
  | StateROutside [t]

buildSublistR
  :: Builder s b b' e e' c p t
  -> s
  -> StateR s b b' e e' c p t
  -> StateR s b b' e e' c p t
buildSublistR _ _ e@(StateRError _) = e
buildSublistR builder item (StateRInside end children outside) = case splitItem builder item of
  TopLevelBegin begin -> case refineBegin builder begin of
    Just begin' -> case create builder begin' end children of
      Just result -> StateROutside (result : outside)
      Nothing -> StateRError (ErrorInvalidItem begin' end children)
    Nothing -> StateRError (ErrorInvalidBegin begin)
  TopLevelEnd end' -> StateRError (ErrorNestedEnd end' end)
  TopLevelPass pass -> case getChild builder pass of
    Just child -> StateRInside end (child : children) outside
    Nothing -> StateRError (ErrorInvalidChild pass)

buildSublistR builder item (StateROutside outside) = case splitItem builder item of
  TopLevelBegin begin -> case refineBegin builder begin of
    Just begin' -> StateRError (ErrorOnlyBegin begin')
    Nothing -> StateRError (ErrorInvalidBegin begin)
  TopLevelEnd end -> case refineEnd builder end of
    Just end' -> StateRInside end' [] outside
    Nothing -> StateRError (ErrorInvalidEnd end)
  TopLevelPass pass -> StateROutside (passItem builder pass : outside)
