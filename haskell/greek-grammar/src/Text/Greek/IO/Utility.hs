module Text.Greek.IO.Utility where

import qualified Control.Lens as Lens
import qualified Control.Monad.Except as Monad
import qualified Text.Greek.Utility as Utility

handleIOError :: Show a => IO (Either a b) -> Monad.ExceptT String IO b
handleIOError x = Monad.liftIO x >>= handleError

handleMaybe :: String -> Maybe a -> Monad.ExceptT String IO a
handleMaybe s = handleError . Utility.maybeToEither s

handleError :: Show a => Either a b -> Monad.ExceptT String IO b
handleError (Left x) = Monad.throwError . show $ x
handleError (Right x) = return x

showError :: Show a => Either a b -> Either String b
showError = Lens.over Lens._Left show
