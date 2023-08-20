module Sp (main) where

import Control.Monad (unless)
import Data.Foldable (forM_)
import Data.Function ((&))
import Data.Functor (void)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read (decimal)
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

import Sp.AppEnv
import Sp.CLI (Command (..))
import Sp.CLI qualified
import Sp.CallbackServer qualified as CallbackServer
import Sp.Effect.Browser qualified as Browser
import Sp.Effect.Config qualified as Config
import Sp.Effect.Console qualified as Console
import Sp.Effect.FileSystem qualified as FileSystem
import Sp.Effect.Log qualified as Log
import Sp.Effect.Spotify qualified as Spotify
import Sp.Effect.Spotify.Servant
import Sp.Effect.Spotify.TokenResponse qualified as TokenResponse
import Sp.Errors
import Sp.Types
import Sp.UserConfig as Config

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

  UserConfig {clientId, clientSecret, refreshToken} <- Config.readConfig ConfigFile

  TokenResponse {access_token} <-
    Spotify.makeTokenRequest
      (Just (TokenAuthorization clientId clientSecret))
      TokenRequest
        { grant_type = RefreshTokenGrantType
        , refresh_token = Just refreshToken
        , code = Nothing
        , redirect_uri = Nothing
        }

  Config.writeConfig ConfigFile (UserConfig clientId clientSecret refreshToken access_token)

withRefresh :: Program () -> Program ()
withRefresh prog = do
  Error.catchError @SpError
    prog
    (\_ e -> case e of InvalidTokenError -> refresh >> prog; _ -> Error.throwError e)

withAuth :: (Authorization -> Program ()) -> Program ()
withAuth prog = do
  tok <- Config.accessToken <$> Config.readConfig ConfigFile
  prog (Authorization tok)

playProg :: Maybe [Text] -> Program ()
playProg uris = withRefresh $ withAuth $ \auth ->
  void
    ( Spotify.makePlayRequest
        auth
        PlayRequest
          { context_uri = Nothing
          , uris = uris
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

promptTracks :: [Track] -> Program Track
promptTracks ts = do
  let choices = zip ts [1 :: Int ..]

  forM_ choices \(Track {name = n, artists = as}, i) -> do
    Console.writeLine (Text.pack (show i) <> ". " <> artistNames as <> " - " <> n)

  choice <- Console.prompt "Choose track from the options above : "

  choice' <- case decimal @Int choice of
    Left _ -> Error.throwError (UnexpectedError "Invalid input.")
    Right (i, _) -> pure i

  case find ((== choice') . snd) choices of
    Nothing -> Error.throwError (UnexpectedError "Choice out of bounds.")
    Just (track, _) -> pure track
  where
    artistNames = Text.intercalate ", " . map go
      where
        go Artist {name = n} = n

searchTracksProg :: Text -> Program ()
searchTracksProg q = withRefresh $ withAuth \auth -> do
  SearchResponse {tracks} <- Spotify.makeSearchRequest auth (trackSearch q)
  Track {uri} <- promptTracks (items tracks)
  playProg (pure (pure uri))

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

  code <- race (Chan.readChan chan) (CallbackServer.runServer chan env)

  userCode <- case code of
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

runSpotifyEffect :: AppEnv -> Program () -> IO (Either SpError ())
runSpotifyEffect env prog =
  prog
    & Browser.runBrowserIO
    & Spotify.runSpotifyServant
    & Config.runConfigIO
    & FileSystem.runFileSystemIO
    & Log.runLogIO
    & Console.runConsole
    & runReader @AppEnv env
    & runErrorNoCallStack @SpError
    & runConcurrent
    & runEff

runCommand :: AppEnv -> Command -> IO ()
runCommand env cmd = case cmd of
  Authorize -> runSpotifyEffect env authorize >>= handleErrors
  Play -> runSpotifyEffect env (playProg Nothing) >>= handleErrors
  Pause -> runSpotifyEffect env pauseProg >>= handleErrors
  Next -> runSpotifyEffect env nextProg >>= handleErrors
  Prev -> runSpotifyEffect env prevProg >>= handleErrors
  Replay -> runSpotifyEffect env replayProg >>= handleErrors
  Seek s -> runSpotifyEffect env (seekProg s) >>= handleErrors
  SearchTrack q -> runSpotifyEffect env (searchTracksProg q) >>= handleErrors
  where
    runEffs m = m & Log.runLogIO & runReader @AppEnv env & runEff
    handleErrors = \case
      Left err -> Log.error (Text.pack (show err)) & runEffs
      Right _ -> Log.debug ("Command `" <> Text.pack (show cmd) <> "` executed succesfully.") & runEffs

main :: IO ()
main = do
  opts <- Sp.CLI.getOpts
  let logLevel = if Sp.CLI.debug opts then LevelDebug else LevelInfo
  env <- AppEnv <$> accountsEnv <*> mainEnv <*> pure logLevel
  runCommand env (Sp.CLI.userCommand opts)
