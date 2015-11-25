{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (Word)
import Control.Monad.Except
import qualified Text.Greek.IO.Process as Process

main :: IO ()
main = runExceptIO Process.processSblgnt

runExceptIO :: ExceptT String IO () -> IO ()
runExceptIO x = do
  result <- runExceptT x
  handleResult result

handleResult :: Either String () -> IO ()
handleResult (Left e) = putStrLn e
handleResult (Right ()) = putStrLn "Complete"
