module OverTheFinishLine.Dashboard.Infrastructure (
  Context,
  Infrastructure (..),
  runWithInfrastructure
) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql (ConnectionPool, withPostgresqlPool)
import qualified Network.HTTP.Client
import qualified Network.HTTP.Client.TLS
import qualified Network.Wai.Handler.Warp as Warp
import System.Posix.Signals (Handler (Catch), installHandler, sigTERM)
import qualified Web.Spock as Spock
import Web.Spock.Config

import OverTheFinishLine.Dashboard.Configuration

type Context = Spock.SpockActionCtx () () () ()

data Infrastructure = Infrastructure {
  databaseConnectionPool :: ConnectionPool,
  httpManager :: Network.HTTP.Client.Manager,
  warpSettings :: Warp.Settings,
  spockConfiguration :: SpockCfg () () (),
  configuration :: Configuration
}

runWithInfrastructure :: Configuration -> (Infrastructure -> IO ()) -> IO ()
runWithInfrastructure configuration' startApp =
  runStdoutLoggingT $ withPostgresqlPool databaseConnectionString databasePoolSize $ \databaseConnectionPool' -> liftIO $ do
    httpManager' <- Network.HTTP.Client.newManager httpClientSettings
    spockConfiguration' <- defaultSpockCfg () PCNoDatabase ()
    let infrastructure = Infrastructure databaseConnectionPool' httpManager' warpSettings' spockConfiguration' configuration'
    startApp infrastructure
  where
    databaseConnectionString = configurationDatabaseConnectionString configuration'
    databasePoolSize = configurationDatabasePoolSize configuration'
    httpClientSettings = Network.HTTP.Client.TLS.tlsManagerSettings {
      Network.HTTP.Client.managerResponseTimeout =
        Network.HTTP.Client.responseTimeoutMicro (
          configurationHttpClientTimeoutInSeconds configuration' * secondsInMicroseconds)
    }
    warpSettings' = Warp.defaultSettings
      |> Warp.setPort (configurationPort configuration')
      |> Warp.setGracefulShutdownTimeout (Just 1)
      |> Warp.setInstallShutdownHandler (\closeSockets ->
          void $ installHandler sigTERM (Catch closeSockets) Nothing)

secondsInMicroseconds :: Int
secondsInMicroseconds = 1000000

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
