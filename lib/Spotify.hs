module Spotify (main) where

import Control.Monad (unless)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful (runEff)
import Effectful.Concurrent
import Effectful.Concurrent.Async (race_)
import Effectful.Concurrent.MVar (newEmptyMVar)
import Effectful.Concurrent.MVar.Strict (takeMVar)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static (ask, runReader)
import Servant.Client (showBaseUrl)
import Servant.Links
import Web.HttpApiData

import Data.ByteString (fromStrict)
import Data.Functor (void)
import Data.Text.Encoding (encodeUtf8)
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
import Spotify.Errors
import Spotify.Types
import Spotify.UserConfig as Config
import Spotify.UserConfig qualified as UC

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

refreshToken :: Program ()
refreshToken = do
  c <- Config.readConfig BaseConfig
  tok <- Config.readToken TokenFile
  let auth = Just (TokenAuthorization (UC.clientId c) (UC.clientSecret c))
  res <-
    Spotify.makeTokenRequest
      auth
      TokenRequest
        { grant_type = RefreshTokenGrantType
        , code = Nothing
        , redirect_uri = Nothing
        , refresh_token = Just tok
        }
  Log.debug "Refreshing access token."
  Config.writeToken (fromStrict (encodeUtf8 (access_token res)))

withRefresh :: Program () -> Program ()
withRefresh prog = do
  Error.catchError @SpotifyError
    prog
    (\_ e -> case e of TokenRequestError -> refreshToken >> playProg; _ -> Error.throwError e)

playProg :: Program ()
playProg = withRefresh $ do
  tok <- Config.readToken TokenFile
  void
    ( Spotify.makePlayRequest
        (Authorization tok)
        PlayRequest
          { context_uri = Nothing
          , uris = Nothing
          , offset = Nothing
          , position_ms = 0
          }
    )

pauseProg :: Program ()
pauseProg = withRefresh $ do
  tok <- Config.readToken TokenFile
  void
    ( Spotify.makePauseRequest
        (Authorization tok)
    )

nextProg :: Program ()
nextProg = withRefresh $ do
  tok <- Config.readToken TokenFile
  void
    ( Spotify.makeNextRequest
        (Authorization tok)
    )

prevProg :: Program ()
prevProg = withRefresh $ do
  tok <- Config.readToken TokenFile
  void
    ( Spotify.makePrevRequest
        (Authorization tok)
    )

replayProg :: Program ()
replayProg = withRefresh $ do
  tok <- Config.readToken TokenFile
  void
    ( Spotify.makeSeekRequest
        (Authorization tok)
        0
    )

seekProg :: Int -> Program ()
seekProg s = withRefresh $ do
  tok <- Config.readToken TokenFile
  void
    ( Spotify.makeSeekRequest
        (Authorization tok)
        (s * 1000)
    )

authUrl :: String -> Text
authUrl clientId =
  Text.pack (showBaseUrl accountsBaseUrl)
    <> "/"
    <> toUrlPiece (safeLink authorizeApi authorizeApi clientId ResponseType redirectUri Nothing scope False)

authorize :: Program ()
authorize = do
  c <- Config.readConfig BaseConfig

  let url = authUrl (Text.unpack (Config.clientId c))

  browserOpened <- Browser.open (Text.unpack url)

  unless browserOpened $
    Log.info $
      "Open link manually in your favorite browser: " <> url

  env <- ask @AppEnv

  toDie <- newEmptyMVar

  race_ (takeMVar toDie) (CallbackServer.runServer toDie env)

  let auth = Just $ TokenAuthorization (UC.clientId c) (UC.clientSecret c)

  userCode <- Console.readLine

  res <-
    Spotify.makeTokenRequest
      auth
      TokenRequest
        { grant_type = AuthorizationCodeGrantType
        , code = Just userCode
        , redirect_uri = Just (Text.pack redirectUri)
        , refresh_token = Nothing
        }

  Config.writeToken (fromStrict (encodeUtf8 (access_token res)))

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
