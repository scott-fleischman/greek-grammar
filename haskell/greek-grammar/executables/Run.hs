{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (Word)
import Control.Monad.Except
import qualified Text.Greek.IO.Pipeline as Pipeline

main :: IO ()
main = runExceptIO Pipeline.runSblgnt

runExceptIO :: ExceptT String IO () -> IO ()
runExceptIO x = do
  result <- runExceptT x
  handleResult result

handleResult :: Either String () -> IO ()
handleResult (Left e) = putStrLn e
handleResult (Right ()) = putStrLn "Complete"
