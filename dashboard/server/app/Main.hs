{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile)
import Data.Monoid (mconcat)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import System.Environment (getEnv)
import System.FilePath
import Web.Scotty

type Port = Int
data ServerOptions = ServerOptions { port :: Port, clientPath :: FilePath }

main = readOptions >>= server

server (ServerOptions port clientPath) =
  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase clientPath)
    get "/" $ file (clientPath </> "index.html")

readOptions =
  ServerOptions
   <$> (read <$> getEnv "PORT")
   <*> getEnv "CLIENT_PATH"
