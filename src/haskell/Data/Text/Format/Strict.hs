module Data.Text.Format.Strict where

import Data.Text (Text)
import Data.Text.Format (Format, format)
import Data.Text.Format.Params (Params)
import Data.Text.Lazy (toStrict)

format' :: Params s => Format -> s -> Text
format' fmt ps = toStrict . format fmt $ ps
