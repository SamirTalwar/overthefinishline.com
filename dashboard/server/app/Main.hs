{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile)
import Data.Monoid (mconcat)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import System.Environment (getEnv)
import System.FilePath
import Web.Scotty

type Port = Int
data Configuration = Configuration { port :: Port, clientPath :: FilePath }

main = readConfiguration >>= server

server (Configuration port clientPath) =
  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase clientPath)
    get "/" $ file (clientPath </> "index.html")

readConfiguration =
  Configuration
   <$> (read <$> getEnv "PORT")
   <*> getEnv "CLIENT_PATH"
