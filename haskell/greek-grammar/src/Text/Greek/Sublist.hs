{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Sublist where

import Text.Greek.Utility

data Error b e
  = ErrorOnlyBegin b
  | ErrorOnlyEnd e
  | ErrorNestedEnd e e
  deriving (Show)

data TopLevel b e t
  = TopLevelBegin b
  | TopLevelEnd e
  | TopLevelPass t

foldrSublist :: (s -> TopLevel b e t) -> [s] -> Error b e + [t + (b, e, [t])]
foldrSublist f xs = case foldr buildSublistR (StateROutside []) . fmap f $ xs of
  StateRError e -> Left e
  StateRInside e _ _ -> Left (ErrorOnlyEnd e)
  StateROutside os -> Right os

data StateR b e t
  = StateRError (Error b e)
  | StateROutside [t + (b, e, [t])]
  | StateRInside e [t] [t + (b, e, [t])]

buildSublistR
  :: TopLevel b e t
  -> StateR b e t
  -> StateR b e t
buildSublistR _               e@(StateRError _) = e

buildSublistR (TopLevelBegin b) (StateRInside e cs os) = StateROutside (Right (b, e, cs) : os)
buildSublistR (TopLevelEnd e')  (StateRInside e _ _)   = StateRError (ErrorNestedEnd e' e)
buildSublistR (TopLevelPass t)  (StateRInside e cs os) = StateRInside e (t : cs) os

buildSublistR (TopLevelBegin b) (StateROutside _)      = StateRError (ErrorOnlyBegin b)
buildSublistR (TopLevelEnd e)   (StateROutside os)     = StateRInside e [] os
buildSublistR (TopLevelPass t)  (StateROutside os)     = StateROutside (Left t : os)
