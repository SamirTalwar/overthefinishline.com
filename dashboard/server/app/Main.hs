{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception (catch)
import Control.Monad (forM_, mzero, unless, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Crypto.Random (getRandomBytes)
import Data.Aeson as Aeson hiding (json)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as ByteStringC
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Either as Either
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, isJust, listToMaybe, mapMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (NominalDiffTime, addUTCTime, getCurrentTime)
import qualified Database.Persist as Database
import qualified Database.Esqueleto as Sql
import Database.Esqueleto hiding (delete, get)
import Database.Persist.Postgresql (ConnectionString, runMigration, runSqlPersistMPool, withPostgresqlPool)
import Network.HTTP.Client as HTTPClient
import Network.HTTP.Client.TLS as HTTPClientTLS
import Network.HTTP.Conduit as HTTPConduit
import Network.HTTP.Types.Status (Status (..), badRequest400, unauthorized401, internalServerError500)
import Network.HTTP.Types.URI as URI
import Network.OAuth.OAuth2 as OAuth2
import qualified Network.Wai.Parse as Parse
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import System.Environment (getEnv, lookupEnv)
import System.FilePath
import System.IO.Error
import System.Posix.Signals
import Web.Spock

import OverTheFinishLine.Dashboard.Enumerations
import OverTheFinishLine.Dashboard.GitHub
import OverTheFinishLine.Dashboard.Model

data Environment = Development | Production
  deriving (Eq, Read, Show)
type Port = Int
data Configuration = Configuration {
  configurationEnvironment :: Environment,
  configurationPort :: Port,
  configurationClientPath :: FilePath,
  configurationGitHubOAuthCredentials :: OAuth2,
  configurationDatabaseConnectionString :: ConnectionString,
  configurationDatabasePoolSize :: Int,
  configurationSessionTTL :: NominalDiffTime,
  configurationSessionStoreInterval :: NominalDiffTime,
  configurationHttpClientTimeoutInSeconds :: Int
}

type Context = SpockActionCtx () () () ()

instance ToJSON Status where
  toJSON (Status code message) = object ["code" .= code, "message" .= decodeUtf8 message]

main :: IO ()
main = readConfiguration >>= server

server :: Configuration -> IO ()
server configuration =
  runStdoutLoggingT $ withPostgresqlPool databaseConnectionString databasePoolSize $ \pool -> liftIO $ do
    putStrLn ("Application starting on port " ++ show port ++ ".")
    flip runSqlPersistMPool pool $ runMigration migrateAll
    httpManager <- HTTPClient.newManager httpClientSettings
    app <- createApp configuration pool httpManager
    Warp.runSettings warpSettings app
  where
    databaseConnectionString = configurationDatabaseConnectionString configuration
    databasePoolSize = configurationDatabasePoolSize configuration
    port = configurationPort configuration
    httpClientSettings = HTTPClientTLS.tlsManagerSettings {
      managerResponseTimeout = Just (configurationHttpClientTimeoutInSeconds configuration * secondsInMicroseconds)
    }
    warpSettings = Warp.defaultSettings
      |> Warp.setPort port
      |> Warp.setGracefulShutdownTimeout (Just 1)
      |> Warp.setInstallShutdownHandler (\closeSockets ->
          void $ installHandler sigTERM (Catch (closeSockets >> putStrLn "Application shut down.")) Nothing)

createApp configuration databaseConnectionPool httpManager =
  spockAsApp $ spock (defaultSpockCfg () PCNoDatabase ()) $ do
    middleware $ case configurationEnvironment configuration of
      Development -> logStdoutDev
      Production -> logStdout

    middleware $ staticPolicy (noDots >-> addBase (configurationClientPath configuration))

    get root appHtml

    post ("authentication" <//> "by" <//> "github") authenticateWithGitHub

    get ("authorization" <//> "by" <//> "github") $ do
      response <- runExceptT $ do
        userId <- storeUser
        storeSession userId
      either handleFailure (const (redirect "/")) response

    post "sign-out" $ do
      response <- runExceptT removeSession
      either handleFailure (const (redirect "/")) response

    get "projects" appHtml

    post "projects" $ do
      requestParams <- params
      project <- runExceptT $ do
        user <- readUser
        storeProject user requestParams
      either handleFailure (redirect . uncurry projectUrl) project

    get ("projects" <//> (var :: Var Text) <//> (var :: Var Text)) $ const $ const appHtml

    get ("projects" <//> (var :: Var Text) <//> (var :: Var Text) <//> "edit") $ const $ const appHtml

    decode2 (post ("projects" <//> var <//> var <//> "edit")) $ \username projectName -> do
      requestParams <- params
      project <- runExceptT $ do
        user <- readUserByName username
        updateProject user projectName requestParams
      either handleFailure (redirect . uncurry projectUrl) project

    subcomponent "api" $ do
      get "me" $ do
        me <- runExceptT $ do
          Entity userId user <- readUser
          projects <- readMyProjects userId user
          return (user, projects)
        either handleFailure (uncurry renderMe) me

      decode2 (get ("projects" <//> var <//> var)) $ \username projectName -> do
        now <- liftIO getCurrentTime
        potentialAccessToken <- runExceptT readAccessToken
        potentialRepositories <- runExceptT $ withDatabase (
            select $ from $ \(user `InnerJoin` project `InnerJoin` repository) -> do
                on (project ^. ProjectId ==. repository ^. ProjectRepositoryProjectId)
                on (user ^. UserId ==. project ^. ProjectUserId)
                where_ $
                  user ^. UserUsername ==. val username
                  &&. project ^. ProjectName ==. val projectName
                return repository
          ) `onEmpty` QueryFailure "No repositories found."

        case (potentialAccessToken, potentialRepositories) of
          (Left failure, _) -> handleFailure failure
          (_, Left failure) -> handleFailure failure
          (Right accessToken, Right repositories) -> do
            requests <- mapM (runExceptT . fetchGitHubPullRequests accessToken . projectRepositoryName . entityVal) repositories
            let (failures, responses) = Either.partitionEithers requests
            let pullRequests = List.sortBy (compare `Function.on` prUpdatedAt) (concat responses)
            render failures $ Dashboard now pullRequests

      decode2 (get ("projects" <//> var <//> var <//> "edit")) $ \username projectName -> do
        myProject <- runExceptT $ do
          Entity userId user <- readUserByName username
          project <- readProject user projectName
          return $ MySingleProject user project
        either handleFailure (render []) myProject

  where
    appHtml = file "text/html" (configurationClientPath configuration </> "index.html")

    authenticateWithGitHub = redirect $ decodeUtf8 $ authorizationUrl gitHubOAuthCredentials

    readUserId :: ExceptT Failure Context UserId
    readUserId = do
      now <- liftIO getCurrentTime
      authId <- cookie "authId" `orFailure` UnauthenticatedUser
      sessions <- withDatabase $ do
        Sql.delete $ from $ \session ->
          where_ $ session ^. SessionExpiryTime <. val now
        select $ from $ \session -> do
          where_ $ session ^. SessionAuthId ==. val authId
          return session
      Entity sessionId session <- return (listToMaybe sessions) `orFailure` UnauthenticatedUser
      let newExpiryTime = addUTCTime sessionTTL now
      withDatabase $ update $ \session -> do
        set session [SessionExpiryTime =. val newExpiryTime]
        where_ $ session ^. SessionId ==. val sessionId
      return $ sessionUserId session

    storeSession :: UserId -> ExceptT Failure Context ()
    storeSession userId = do
      authId <- decodeUtf8 . Base64.encode <$> liftIO (getRandomBytes 64)
      now <- liftIO getCurrentTime
      let expiryTime = addUTCTime sessionTTL now
      withDatabase $ insert (Session authId expiryTime userId)
      lift $ setCookie "authId" authId sessionCookieSettings

    removeSession :: ExceptT Failure Context ()
    removeSession = do
      authId <- lift $ cookie "authId"
      when (isJust authId) $ do
        withDatabase $ Sql.delete $ from $ \session ->
          where_ $ session ^. SessionAuthId ==. val (fromJust authId)
        lift $ deleteCookie "authId"

    sessionCookieSettings = defaultCookieSettings { cs_EOL = CookieValidFor sessionTTL }

    sessionTTL = configurationSessionTTL configuration

    storeUser = do
      code <- param "code" `orFailure` MissingParam "code"
      accessToken <- withExceptT (QueryFailure . decodeUtf8 . toStrict) $
        ExceptT $ liftIO $ fetchAccessToken httpManager gitHubOAuthCredentials (encodeUtf8 code)
      (GitHubUser gitHubUserId gitHubUserLogin gitHubUserAvatarUrl) <- fetchGitHubUser accessToken
      let accessTokenString = OAuth2.accessToken accessToken
      withDatabase $ do
        let user = User gitHubUserLogin gitHubUserAvatarUrl
        let serviceUser = ServiceUser GitHub gitHubUserId
        serviceCredentials <- getBy serviceUser
        case serviceCredentials of
          Nothing -> do
            userId <- insert user
            insert $ ServiceCredentials userId GitHub gitHubUserId accessTokenString
            return userId
          Just (Entity serviceCredentialsId (ServiceCredentials userId _ _ _)) -> do
            repsert userId user
            update $ \serviceCredentials -> do
              set serviceCredentials [ServiceCredentialsAccessToken =. val accessTokenString]
              where_ (serviceCredentials ^. ServiceCredentialsId ==. val serviceCredentialsId)
            return userId

    storeProject (Entity userId user) requestParams = do
      (project, repositoryNames) <- projectParams userId requestParams
      withDatabase $ do
        projectId <- insert project
        mapM_ (insert . ProjectRepository projectId) repositoryNames
      return (user, project)

    updateProject (Entity userId user) existingProjectName requestParams = do
      (project, repositoryNames) <- projectParams userId requestParams
      Entity projectId _ <- withDatabase (getBy (UniqueProjectNameByUser userId existingProjectName))
                              `orFailure` MissingProject existingProjectName
      withDatabase $ do
        replace projectId project
        Sql.delete $ from $ \repository -> where_ (repository ^. ProjectRepositoryProjectId ==. val projectId)
        mapM_ (insert . ProjectRepository projectId) repositoryNames
        return (user, project)

    projectParams userId requestParams = do
      projectName <- textParam "project-name" requestParams
      repositoryNames <- textListParam "repository-names[]" requestParams
      let project = Project userId projectName
      return (project, repositoryNames)

    fetchGitHubUser accessToken =
      fetch accessToken "https://api.github.com/user"

    fetchGitHubPullRequests accessToken repositoryName =
      fetch accessToken $ mconcat ["https://api.github.com/repos/", encodeUtf8 repositoryName, "/pulls"]

    fetch :: (MonadIO m, FromJSON a) => AccessToken -> URI -> ExceptT Failure m a
    fetch accessToken url =
      withExceptT (RequestFailure (decodeUtf8 url) . decodeUtf8 . toStrict) $ ExceptT $ liftIO $
        authGetJSON httpManager accessToken url
        `catch` \(StatusCodeException status _ _) ->
          return $ Left $ fromStrict $ ByteStringC.pack $ show status

    readAccessToken :: ExceptT Failure Context AccessToken
    readAccessToken = do
      userId <- readUserId
      let userService = UserService userId GitHub
      (Entity _ (ServiceCredentials _ _ _ accessToken)) <- withDatabase (getBy userService) `orFailure` MissingUser
      return $ AccessToken accessToken Nothing Nothing Nothing Nothing

    readUser :: ExceptT Failure Context (Entity User)
    readUser = do
      userId <- readUserId
      user <- withDatabase (Database.get userId) `orFailure` MissingUser
      return $ Entity userId user

    readUserByName :: Text -> ExceptT Failure Context (Entity User)
    readUserByName username = do
      userId <- readUserId
      users <- withDatabase $
        select $ from $ \user -> do
          where_ (user ^. UserId ==. val userId &&. user ^. UserUsername ==. val username)
          return user
      return (listToMaybe users) `orFailure` QueryFailure "Invalid user."

    readMyProjects :: Key User -> User -> ExceptT Failure Context [MyProject]
    readMyProjects userId user = do
      projectsAndRepositoryEntities <- withDatabase $
        select $ from $ \(project `LeftOuterJoin` repository) -> do
          on (just (project ^. ProjectId) ==. repository ?. ProjectRepositoryProjectId)
          where_ (project ^. ProjectUserId ==. val userId)
          orderBy [asc (project ^. ProjectName), asc (repository ?. ProjectRepositoryName)]
          return (project, repository)
      let projectsAndRepositories = map (\(p, r) -> (entityVal p, entityVal <$> r)) projectsAndRepositoryEntities
      let groupedProjectsAndRepositories = groupQueryBy fst snd projectsAndRepositories
      let myProject project repositories = MyProject (projectName project) (projectUrl user project) (map projectRepositoryName (catMaybes repositories))
      return $ map (uncurry myProject) groupedProjectsAndRepositories

    readProject :: User -> Text -> ExceptT Failure Context MyProject
    readProject user projectName = do
      projectAndRepositories :: [(Entity Project, Maybe (Entity ProjectRepository))] <- withDatabase $
        select $ from $ \(project `LeftOuterJoin` repository) -> do
          on (just (project ^. ProjectId) ==. repository ?. ProjectRepositoryProjectId)
          where_ (project ^. ProjectName ==. val projectName)
          orderBy [asc (repository ?. ProjectRepositoryName)]
          return (project, repository)
      project <- return (entityVal . fst <$> listToMaybe projectAndRepositories)
                   `orFailure` QueryFailure "Invalid project."
      let repositories = map entityVal $ mapMaybe snd projectAndRepositories
      return $ MyProject projectName (projectUrl user project) (map projectRepositoryName repositories)

    renderMe user projects = render [] (Me user projects)

    render failures value = json (AuthenticatedResponse failures value)

    textParam :: Monad a => Text -> [(Text, Text)] -> ExceptT Failure a Text
    textParam name params = return (lookup name params) `orFailure` MissingParam name

    textListParam :: Monad a => Text -> [(Text, Text)] -> ExceptT Failure a [Text]
    textListParam name params = return values `onEmpty` MissingParam name
      where
        values = filter (/= "") $ map snd $ filter ((== name) . fst) params

    orFailure :: Monad m => m (Maybe a) -> e -> ExceptT e m a
    maybe `orFailure` failure = maybeToExceptT failure (MaybeT maybe)

    onEmpty :: Monad m => m [a] -> e -> ExceptT e m [a]
    list `onEmpty` failure = ExceptT ((\l -> when (null l) (Left failure) >> Right l) <$> list)

    handleFailure failure = do
      let (status, response) = failureResponse failure
      runStdoutLoggingT $ $logDebugS "Web" (decodeUtf8 $ toStrict $ encode response)
      setStatus status
      json response

    failureResponse :: Failure -> (Status, Aeson.Value)
    failureResponse UnauthenticatedUser =
      (unauthorized401, toJSON unauthenticatedResponse)
    failureResponse MissingUser =
      (unauthorized401, toJSON unauthenticatedResponse)
    failureResponse (MissingProject project) =
      failureJSON badRequest400 "missing project" ["project" .= project]
    failureResponse (MissingParam param) =
      failureJSON badRequest400 "missing param" ["param" .= param]
    failureResponse (QueryFailure message) =
      failureJSON internalServerError500 "internal failure" ["message" .= message]
    failureResponse (RequestFailure url message) =
      failureJSON internalServerError500 "request failure" ["url" .= url, "message" .= message]

    failureJSON status message extras =
      (status, object ([
        "error" .= (message :: Text),
        "status" .= status
      ] ++ extras))

    gitHubOAuthCredentials = configurationGitHubOAuthCredentials configuration

    withDatabase :: MonadIO m => SqlPersistM a -> m a
    withDatabase = flip liftSqlPersistMPool databaseConnectionPool

readConfiguration =
  Configuration
    <$> readDefaultedEnv "ENVIRONMENT" Production
    <*> readEnv "PORT"
    <*> getEnv "CLIENT_PATH"
    <*> (OAuth2
      <$> byteStringEnv "GITHUB_OAUTH_CLIENT_ID"
      <*> byteStringEnv "GITHUB_OAUTH_CLIENT_SECRET"
      <*> pure "https://github.com/login/oauth/authorize?scope=user:email%20repo"
      <*> pure "https://github.com/login/oauth/access_token"
      <*> pure Nothing)
    <*> do
      host <- getEnv "DATABASE_HOST"
      port <- getEnv "DATABASE_PORT"
      database <- getEnv "DATABASE_DASHBOARD_NAME"
      username <- getEnv "DATABASE_DASHBOARD_USERNAME"
      password <- getEnv "DATABASE_DASHBOARD_PASSWORD"
      return $ ByteStringC.pack $ unwords $ map (\(key, value) -> key ++ "='" ++ value ++ "'") [
        ("host", host),
        ("port", port),
        ("dbname", database),
        ("user", username),
        ("password", password)]
    <*> readEnv "DATABASE_POOL_SIZE"
    <*> (fromInteger <$> readDefaultedEnv "SESSION_TTL" 3600)
    <*> (fromInteger <$> readDefaultedEnv "SESSION_STORE_INTERVAL" 10)
    <*> readDefaultedEnv "HTTP_CLIENT_TIMEOUT" 5
  where
    byteStringEnv name = ByteStringC.pack <$> getEnv name

    readEnv :: Read a => String -> IO a
    readEnv name = do
      value <- getEnv name
      readIO value `catchIOError` \theError -> do
        putStrLn $ "Failed to parse the variable " ++ name ++ " with the value \"" ++ value ++ "\"."
        ioError theError

    readDefaultedEnv :: Read a => String -> a -> IO a
    readDefaultedEnv name defaultValue = do
      value <- lookupEnv name
      maybe (return defaultValue) (\value ->
        readIO value `catchIOError` \theError -> do
          putStrLn $ "Failed to parse the variable " ++ name ++ " with the value \"" ++ value ++ "\"."
          ioError theError
        ) value

decode2 request handler = request $ \a b -> handler (decode a) (decode b)
  where
    decode = decodeUtf8 . URI.urlDecode True . encodeUtf8

groupQueryBy :: Eq k => (v -> k) -> (v -> w) -> [v] -> [(k, [w])]
groupQueryBy keyFunction valueFunction list =
  map (\g -> (keyFunction (List.head g), map valueFunction g)) grouped
  where
    grouped = List.groupBy ((==) `Function.on` keyFunction) list

secondsInMicroseconds = 1000000
