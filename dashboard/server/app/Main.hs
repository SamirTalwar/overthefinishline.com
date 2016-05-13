{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile)
import Data.Monoid (mconcat)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Options.Applicative
import System.Environment (getArgs)
import System.FilePath
import Web.Scotty

type Port = Int
data ServerOptions = ServerOptions { clientPath :: FilePath, port :: Port }

main = execParser parseOptions >>= server

server (ServerOptions clientPath port) =
  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase clientPath)
    get "/" $ file (clientPath </> "index.html")

parseOptions =
  info (helper <*> parser) fullDesc
  where
    parser = ServerOptions
      <$> strOption (long "client" <> metavar "CLIENT-PATH" <> help "Path to the client files")
      <*> option auto (long "port" <> metavar "PORT" <> help "Server port")
