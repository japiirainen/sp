module Spotify (main) where

import Control.Monad (unless)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful (runEff)
import Effectful.Concurrent
import Effectful.Concurrent.Async (race)
import Effectful.Concurrent.Chan qualified as Chan
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static (ask, runReader)
import Servant.Client (showBaseUrl)
import Servant.Links
import Web.HttpApiData

import Data.Functor (void)
import Spotify.AppEnv
import Spotify.CLI (Command (..))
import Spotify.CLI qualified
import Spotify.CallbackServer qualified as CallbackServer
import Spotify.Effect.Browser qualified as Browser
import Spotify.Effect.Config qualified as Config
import Spotify.Effect.Console qualified as Console
import Spotify.Effect.FileSystem qualified as FileSystem
import Spotify.Effect.Log qualified as Log
import Spotify.Effect.Spotify qualified as Spotify
import Spotify.Effect.Spotify.Servant
import Spotify.Effect.Spotify.TokenResponse qualified as TokenResponse
import Spotify.Errors
import Spotify.Types
import Spotify.UserConfig as Config

scope :: Scope
scope =
  scopeFromList
    [ "playlist-read-collaborative"
    , "playlist-read-private"
    , "user-read-playback-state"
    , "user-modify-playback-state"
    , "user-read-currently-playing"
    , "user-read-recently-played"
    , "playlist-modify-private"
    , "playlist-modify-public"
    ]

redirectUri :: String
redirectUri = "http://localhost:7777/callback"

refresh :: Program ()
refresh = do
  Log.debug "Refreshing access token."

  c <- Config.readConfig ConfigFile

  let auth = Just (TokenAuthorization (Config.clientId c) (Config.clientSecret c))

  res <-
    Spotify.makeTokenRequest
      auth
      TokenRequest
        { grant_type = RefreshTokenGrantType
        , refresh_token = Just (Config.refreshToken c)
        , code = Nothing
        , redirect_uri = Nothing
        }

  Config.writeConfig
    ConfigFile
    (UserConfig (Config.clientId c) (Config.clientSecret c) (Config.refreshToken c) (access_token res))

withRefresh :: Program () -> Program ()
withRefresh prog = do
  Error.catchError @SpotifyError
    prog
    (\_ e -> case e of InvalidTokenError -> refresh >> prog; _ -> Error.throwError e)

withAuth :: (Authorization -> Program ()) -> Program ()
withAuth prog = do
  tok <- Config.accessToken <$> Config.readConfig ConfigFile
  prog (Authorization tok)

playProg :: Program ()
playProg = withRefresh $ withAuth $ \auth ->
  void
    ( Spotify.makePlayRequest
        auth
        PlayRequest
          { context_uri = Nothing
          , uris = Nothing
          , offset = Nothing
          , position_ms = 0
          }
    )

pauseProg :: Program ()
pauseProg = withRefresh $ withAuth do void . Spotify.makePauseRequest

nextProg :: Program ()
nextProg = withRefresh $ withAuth do void . Spotify.makeNextRequest

prevProg :: Program ()
prevProg = withRefresh $ withAuth do void . Spotify.makePrevRequest

replayProg :: Program ()
replayProg = withRefresh $ withAuth \auth ->
  void (Spotify.makeSeekRequest auth 0)

seekProg :: Int -> Program ()
seekProg s = withRefresh $ withAuth \auth ->
  void (Spotify.makeSeekRequest auth (s * 1000))

authUrl :: String -> Text
authUrl clientId =
  Text.pack (showBaseUrl accountsBaseUrl)
    <> "/"
    <> toUrlPiece (safeLink authorizeApi authorizeApi clientId ResponseType redirectUri Nothing scope False)

authorize :: Program ()
authorize = do
  ci <- Console.prompt "Enter Client ID : "
  cs <- Console.prompt "Enter Client Secret : "

  let url = authUrl (Text.unpack ci)

  browserOpened <- Browser.open (Text.unpack url)

  unless browserOpened $
    Log.info $
      "Open link manually in your favorite browser: " <> url

  env <- ask @AppEnv

  chan <- Chan.newChan

  rec <- race (Chan.readChan chan) (CallbackServer.runServer chan env)

  userCode <- case rec of
    Left co -> pure co
    Right _ -> Error.throwError (UnexpectedError "Server died before receiving code from channel.")

  res <-
    Spotify.makeTokenRequest
      (Just (TokenAuthorization ci cs))
      TokenRequest
        { grant_type = AuthorizationCodeGrantType
        , code = Just userCode
        , redirect_uri = Just (Text.pack redirectUri)
        , refresh_token = Nothing
        }

  let at = TokenResponse.access_token res

  rt <- case TokenResponse.refresh_token res of
    Just tok -> pure tok
    Nothing -> Error.throwError (UnexpectedError "Didn't receive refresh_token from /authorize")

  Config.writeConfig ConfigFile (UserConfig ci cs rt at)

  Log.info "Authorization flow was succesful!"

runSpotifyEffect :: AppEnv -> Program () -> IO (Either SpotifyError ())
runSpotifyEffect env prog =
  prog
    & Browser.runBrowserIO
    & Spotify.runSpotifyServant
    & Config.runConfigIO
    & FileSystem.runFileSystemIO
    & Log.runLogIO
    & Console.runConsole
    & runReader @AppEnv env
    & runErrorNoCallStack @SpotifyError
    & runConcurrent
    & runEff

runCommand :: AppEnv -> Command -> IO ()
runCommand env cmd = case cmd of
  Authorize -> runSpotifyEffect env authorize >>= handleErrors
  Play -> runSpotifyEffect env playProg >>= handleErrors
  Pause -> runSpotifyEffect env pauseProg >>= handleErrors
  Next -> runSpotifyEffect env nextProg >>= handleErrors
  Prev -> runSpotifyEffect env prevProg >>= handleErrors
  Replay -> runSpotifyEffect env replayProg >>= handleErrors
  Seek s -> runSpotifyEffect env (seekProg s) >>= handleErrors
  where
    runEffs m = m & Log.runLogIO & runReader @AppEnv env & runEff
    handleErrors = \case
      Left err -> Log.error (Text.pack (show err)) & runEffs
      Right _ -> Log.debug ("Command `" <> Text.pack (show cmd) <> "` executed succesfully.") & runEffs

main :: IO ()
main = do
  opts <- Spotify.CLI.getOpts
  let logLevel = if Spotify.CLI.debug opts then LevelDebug else LevelInfo
  env <- AppEnv <$> accountsEnv <*> mainEnv <*> pure logLevel
  runCommand env (Spotify.CLI.userCommand opts)
