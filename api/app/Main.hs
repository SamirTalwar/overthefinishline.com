{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid (mconcat)
import System.Environment (getArgs)
import Web.Scotty

main = do
  port <- read . head <$> getArgs
  scotty port $
    get "/:word" $ do
      beam <- param "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
