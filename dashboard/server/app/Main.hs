{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (catch)
import Control.Monad (forM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Aeson as Aeson hiding (json)
import qualified Data.Aeson.Types
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Either as Either
import qualified Data.Function as Function
import qualified Data.List as List
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import qualified Database.Persist as Database
import qualified Database.Persist.Postgresql as Postgresql
import qualified Database.Esqueleto as Sql
import Database.Esqueleto hiding (delete, get)
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (StatusCodeException), responseStatus)
import Network.HTTP.Types.Status (Status (..), badRequest400, unauthorized401, internalServerError500)
import qualified Network.HTTP.Types.URI as URI
import qualified Network.OAuth.OAuth2 as OAuth2
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import System.FilePath
import Web.Spock (Var, file, get, json, middleware, param, params, post, redirect, root, setStatus, spock, spockAsApp, var, (<//>))

import OverTheFinishLine.Dashboard.Configuration
import OverTheFinishLine.Dashboard.Infrastructure
import OverTheFinishLine.Dashboard.Enumerations
import OverTheFinishLine.Dashboard.Lists
import qualified OverTheFinishLine.Dashboard.GitHub as GitHub
import OverTheFinishLine.Dashboard.Model
import qualified OverTheFinishLine.Dashboard.Session as Session

main :: IO ()
main = do
  appConfiguration <- readConfiguration
  runWithInfrastructure appConfiguration $ \infrastructure -> do
    app <- createApp infrastructure
    Warp.runSettings (warpSettings infrastructure) app

createApp :: Infrastructure -> IO Network.Wai.Application
createApp infrastructure = do
  withDatabase $ Postgresql.runMigration migrateAll
  spockAsApp $ spock (spockConfiguration infrastructure) $ do
    middleware $ case configurationEnvironment (configuration infrastructure) of
      Development -> logStdoutDev
      Production -> logStdout

    middleware $ staticPolicy (noDots >-> addBase (configurationClientPath (configuration infrastructure)))

    get root appHtml

    post ("authentication" <//> "by" <//> "github") authenticateWithGitHub

    get ("authorization" <//> "by" <//> "github") $ do
      response <- runExceptT $ do
        userId <- storeUser
        Session.store userId infrastructure
      either handleFailure (const (redirect "/")) response

    post "sign-out" $ do
      response <- runExceptT (Session.remove infrastructure)
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

    decode2 (post ("projects" <//> var <//> var <//> "edit")) $ \selectedUserName selectedProjectName -> do
      requestParams <- params
      project <- runExceptT $ do
        user <- readUserByName selectedUserName
        updateProject user selectedProjectName requestParams
      either handleFailure (redirect . uncurry projectUrl) project

    get ("api" <//> "me") $ do
      me <- runExceptT $ do
        Entity userId user <- readUser
        projects <- readMyProjects userId user
        return (user, projects)
      either handleFailure (uncurry renderMe) me

    decode2 (get ("api" <//> "projects" <//> var <//> var)) $ \selectedUserName selectedProjectName -> do
      now <- liftIO getCurrentTime
      potentialAccessToken <- runExceptT readAccessToken
      potentialRepositories <- runExceptT $ do
        userId <- entityKey <$> readUserByName selectedUserName
        repositories <- withDatabase $
          select $ from $ \(project `InnerJoin` repository) -> do
            on (project ^. ProjectId ==. repository ^. ProjectRepositoryProjectId)
            where_ $
              project ^. ProjectUserId ==. val userId
              &&. project ^. ProjectName ==. val selectedProjectName
            return repository
        return repositories `onEmpty` QueryFailure "No repositories found."

      case (potentialAccessToken, potentialRepositories) of
        (Left failure, _) -> handleFailure failure
        (_, Left failure) -> handleFailure failure
        (Right accessToken, Right repositories) -> do
          requests :: [Either Failure [PullRequestWithStatus]] <- forM repositories $ \repository -> runExceptT $ do
            let name = projectRepositoryName (entityVal repository)
            fetchGitHubPullRequests accessToken name
          let (failures, responses) = Either.partitionEithers requests
          let pullRequests = List.sortBy (compare `Function.on` prsUpdatedAt) (concat responses)
          render failures $ Dashboard now pullRequests

    decode2 (get ("api" <//> "projects" <//> var <//> var <//> "edit")) $ \selectedUserName selectedProjectName -> do
      myProject <- runExceptT $ do
        user <- entityVal <$> readUserByName selectedUserName
        project <- readProject user selectedProjectName
        return $ MySingleProject user project
      either handleFailure (render []) myProject

  where
    appHtml = file "text/html" (configurationClientPath (configuration infrastructure) </> "index.html")

    authenticateWithGitHub = redirect $ decodeUtf8 $ OAuth2.authorizationUrl gitHubOAuthCredentials

    storeUser = do
      code <- param "code" `orFailure` MissingParam "code"
      accessToken <- withExceptT (QueryFailure . decodeUtf8 . toStrict) $
        ExceptT $ liftIO $ OAuth2.fetchAccessToken (httpManager infrastructure) gitHubOAuthCredentials (encodeUtf8 code)
      (GitHub.User gitHubUserId gitHubUserLogin gitHubUserAvatarUrl) <- fetchGitHubUser accessToken
      let accessTokenString = OAuth2.accessToken accessToken
      withDatabase $ do
        let user = User gitHubUserLogin gitHubUserAvatarUrl
        let serviceUser = ServiceUser GitHub gitHubUserId
        userServiceCredentials <- getBy serviceUser
        case userServiceCredentials of
          Nothing -> do
            userId <- insert user
            _ <- insert $ ServiceCredentials userId GitHub gitHubUserId accessTokenString
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
      selectedProjectName <- textParam "project-name" requestParams
      repositoryNames <- textListParam "repository-names[]" requestParams
      let project = Project userId selectedProjectName
      return (project, repositoryNames)

    fetchGitHubUser :: (MonadIO m) => OAuth2.AccessToken -> ExceptT Failure m GitHub.User
    fetchGitHubUser accessToken =
      fetch accessToken "https://api.github.com/user"

    fetchGitHubPullRequests :: (MonadIO m) => OAuth2.AccessToken -> Text -> ExceptT Failure m [PullRequestWithStatus]
    fetchGitHubPullRequests accessToken repositoryName = do
      pullRequests <- fetch accessToken (mconcat ["https://api.github.com/repos/", repositoryName, "/pulls"])
      statuses <- fetchMany accessToken (map GitHub.prStatusesUrl pullRequests)
      return $ zipWith combinePullRequestWithStatuses pullRequests statuses
      where
        combinePullRequestWithStatuses githubPr statuses =
          let pr = GitHub.unPullRequest githubPr
              status = either (const StatusNone) (singleStatus . GitHub.unStatuses) statuses
          in PullRequestWithStatus {
            prsRepository = prRepository pr,
            prsNumber = prNumber pr,
            prsTitle = prTitle pr,
            prsStatus = status,
            prsUpdatedAt = prUpdatedAt pr,
            prsUrl = prUrl pr
          }

    fetch :: (MonadIO m, FromJSON a) => OAuth2.AccessToken -> Url -> ExceptT Failure m a
    fetch accessToken url =
      withExceptT (RequestFailure url . decodeUtf8 . toStrict) $ ExceptT $ liftIO $
        OAuth2.authGetJSON (httpManager infrastructure) accessToken (encodeUtf8 url)
        `catch` \(HttpExceptionRequest _ (StatusCodeException response _)) ->
          return $ Left $ fromStrict $ statusMessage $ responseStatus response

    fetchMany :: (MonadIO m, FromJSON a) => OAuth2.AccessToken -> [Url] -> m [Either Failure a]
    fetchMany accessToken urls =
      liftIO (mapConcurrently (runExceptT . fetch accessToken) urls)

    readAccessToken :: ExceptT Failure Context OAuth2.AccessToken
    readAccessToken = do
      userId <- Session.retrieve infrastructure
      let userService = UserService userId GitHub
      (Entity _ (ServiceCredentials _ _ _ accessToken)) <- withDatabase (getBy userService) `orFailure` MissingUser
      return $ OAuth2.AccessToken accessToken Nothing Nothing Nothing Nothing

    readUser :: ExceptT Failure Context (Entity User)
    readUser = do
      userId <- Session.retrieve infrastructure
      user <- withDatabase (Database.get userId) `orFailure` MissingUser
      return $ Entity userId user

    readUserByName :: Text -> ExceptT Failure Context (Entity User)
    readUserByName username = do
      userId <- Session.retrieve infrastructure
      user <- listToMaybe <$> withDatabase (
        select $ from $ \user -> do
          where_ (user ^. UserId ==. val userId &&. user ^. UserUsername ==. val username)
          return user)
      return user `orFailure` QueryFailure "Invalid user."

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
    readProject user selectedProjectName = do
      projectAndRepositories :: [(Entity Project, Maybe (Entity ProjectRepository))] <- withDatabase $
        select $ from $ \(project `LeftOuterJoin` repository) -> do
          on (just (project ^. ProjectId) ==. repository ?. ProjectRepositoryProjectId)
          where_ (project ^. ProjectName ==. val selectedProjectName)
          orderBy [asc (repository ?. ProjectRepositoryName)]
          return (project, repository)
      project <- return (entityVal . fst <$> listToMaybe projectAndRepositories)
                   `orFailure` QueryFailure "Invalid project."
      let repositories = map entityVal $ mapMaybe snd projectAndRepositories
      return $ MyProject selectedProjectName (projectUrl user project) (map projectRepositoryName repositories)

    renderMe user projects = render [] (Me user projects)

    render failures value = json (AuthenticatedResponse failures value)

    textParam :: Monad a => Text -> [(Text, Text)] -> ExceptT Failure a Text
    textParam name requestParams = return (lookup name requestParams) `orFailure` MissingParam name

    textListParam :: Monad a => Text -> [(Text, Text)] -> ExceptT Failure a [Text]
    textListParam name requestParams = return values `onEmpty` MissingParam name
      where
        values = filter (/= "") $ map snd $ filter ((== name) . fst) requestParams

    orFailure :: Monad m => m (Maybe a) -> e -> ExceptT e m a
    maybeValue `orFailure` failure = maybeToExceptT failure (MaybeT maybeValue)

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
    failureResponse (MissingParam missingParam) =
      failureJSON badRequest400 "missing param" ["param" .= missingParam]
    failureResponse (QueryFailure message) =
      failureJSON internalServerError500 "internal failure" ["message" .= message]
    failureResponse (RequestFailure url message) =
      failureJSON internalServerError500 "request failure" ["url" .= url, "message" .= message]

    failureJSON :: Status -> Text -> [Data.Aeson.Types.Pair] -> (Status, Aeson.Value)
    failureJSON status message extras =
      (status, object ([
        "error" .= message,
        "status" .= HttpStatus (statusCode status) (decodeUtf8 (statusMessage status))
      ] ++ extras))

    gitHubOAuthCredentials = configurationGitHubOAuthCredentials (configuration infrastructure)

    withDatabase :: MonadIO m => SqlPersistM a -> m a
    withDatabase = flip liftSqlPersistMPool (databaseConnectionPool infrastructure)

decode2 :: ((Text -> Text -> a) -> b) -> (Text -> Text -> a) -> b
decode2 request handler = request $ \a b -> handler (decodeUrlSegment a) (decodeUrlSegment b)
  where
    decodeUrlSegment = decodeUtf8 . URI.urlDecode True . encodeUtf8

data HttpStatus = HttpStatus Int Text
  deriving (Eq, Show)

instance ToJSON HttpStatus where
  toJSON (HttpStatus code message) = object ["code" .= code, "message" .= message]
