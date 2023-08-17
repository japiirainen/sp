module Spotify (main) where

import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (Reader, ask, runReader)
import Servant.Client (showBaseUrl)

import Spotify.AppEnv

import Spotify.Effect.Config (Config)
import Spotify.Effect.Config qualified as Config

import Spotify.Effect.FileSystem qualified as FileSystem

import Spotify.Effect.Spotify (Spotify)
import Spotify.Effect.Spotify qualified as Spotify

import Spotify.Effect.CallbackServer qualified as CallbackServer

import Spotify.UserConfig as Config

import Data.Text (Text)
import Data.Text qualified as Text
import Servant.Links
import Spotify.Effect.Spotify.Servant

import Web.HttpApiData

import Control.Monad (unless)
import Data.Function ((&))
import Spotify.CLI (Command (..))
import Spotify.CLI qualified
import Spotify.Effect.Browser (Browser)
import Spotify.Effect.Browser qualified as Browser
import Spotify.Effect.Log (Log)
import Spotify.Effect.Log qualified as Log
import Spotify.Errors
import Spotify.Types

-- prog :: (Config :> es, Spotify :> es) => Eff es ()
-- prog = do
--   c <- readConfig
--   let auth = Just $ TokenAuthorization (UC.clientId c) (UC.clientSecret c)
--   let tokReq =
--         TokenRequest
--           { grant_type = RefreshTokenGrantType
--           , code = Nothing
--           , redirect_uri = Just "https://github.com/japiirainen/spotify"
--           , refresh_token = Just (refreshToken c)
--           }
--   res <- makeTokenRequest auth tokReq
--   let mauth = Authorization (access_token res)
--   let playReq =
--         PlayRequest
--           { context_uri = Nothing
--           , uris = Nothing
--           , offset = Nothing
--           , position_ms = 10
--           }
--   makePlayRequest mauth playReq

-- makePauseRequest mauth

scope :: Scope
scope = scopeFromList ["playlist-read-collaborative", "playlist-read-private", "user-read-playback-state", "user-modify-playback-state", "user-read-currently-playing", "user-read-recently-played", "playlist-modify-private", "playlist-modify-public"]

redirectUri :: String
redirectUri = "http://localhost:7777/callback"

authUrl :: String -> Text
authUrl clientId =
  Text.pack (showBaseUrl accountsBaseUrl)
    <> "/"
    <> toUrlPiece (safeLink authorizeApi authorizeApi clientId ResponseType redirectUri Nothing scope False)

-- ---- Authorization flow ----
--
-- 1. initial refresh token via /authorize
-- 2. save to config
-- 3. on each request ask for new access_token via saved refresh_token

authorize :: (Reader AppEnv :> es, Spotify :> es, Config :> es, Browser :> es, Log :> es, IOE :> es) => Eff es ()
authorize = do
  c <- Config.readConfig BaseConfig
  let url = authUrl (Text.unpack (Config.clientId c))
  browserOpened <- Browser.open (Text.unpack url)
  unless browserOpened $
    Log.info $
      "Open link manually in your favorite browser: " <> url
  env <- ask @AppEnv
  CallbackServer.runServer env

runSpotifyEffect :: AppEnv -> Program () -> IO (Either SpotifyError ())
runSpotifyEffect env prog =
  prog
    & Browser.runBrowserIO
    & Spotify.runSpotifyServant
    & Config.runConfigIO
    & FileSystem.runFileSystemIO
    & Log.runLogIO
    & runReader @AppEnv env
    & runErrorNoCallStack @SpotifyError
    & runEff

runCommand :: AppEnv -> Command -> IO ()
runCommand env = \case
  Authorize -> runSpotifyEffect env authorize >>= handleErrors
  Play -> undefined
  Pause -> undefined
  where
    runEffs m = m & Log.runLogIO & runReader @AppEnv env & runEff
    handleErrors = \case
      Left err -> Log.error (Text.pack (show err)) & runEffs
      Right _ -> Log.info "Command executed succesfully." & runEffs

main :: IO ()
main = do
  opts <- Spotify.CLI.getOpts
  let logLevel = if Spotify.CLI.debug opts then LevelDebug else LevelInfo
  env <- AppEnv <$> accountsEnv <*> mainEnv <*> pure logLevel
  runCommand env (Spotify.CLI.userCommand opts)
