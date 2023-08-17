module Spotify.Effect.CallbackServer (runServer) where

import Control.Monad.Except qualified as T
import Servant
import Servant.API.Generic
import Servant.Server.Generic

import Data.Function ((&))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Effectful (Eff, IOE, liftIO, runEff)
import Effectful qualified as Eff
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setPort,
 )
import Spotify.AppEnv (AppEnv)
import Spotify.Effect.Config qualified as Config
import Spotify.Effect.FileSystem qualified as FileSystem
import Spotify.Effect.Log qualified as Log
import Spotify.Errors
import Spotify.Types (CBServer)

newtype Routes route = Routes
  { callback ::
      route
        :- "callback"
          :> QueryParam "state" Text
          :> QueryParam' '[Required, Strict] "code" Text
          :> Get '[JSON] String
  }
  deriving stock (Generic)

server :: Routes (AsServerT CBServer)
server =
  Routes
    { callback = handler
    }
  where
    handler :: Maybe Text -> Text -> CBServer String
    handler _ code = do
      Config.writeToken (encodeUtf8 (fromStrict code))
      Log.debug "Wrote token file."
      pure "success"

effToHandler ::
  forall (a :: Type).
  () =>
  Eff '[IOE] (Either SpotifyError (Either ServerError a)) ->
  Handler a
effToHandler computation = do
  v <- liftIO . runEff $! computation
  case v of
    Right foo -> case foo of
      Left err -> T.throwError err
      Right a -> pure a
    Left _ -> T.throwError err500

nt :: AppEnv -> CBServer a -> Handler a
nt env prog =
  prog
    & Config.runConfigIO
    & FileSystem.runFileSystemIO
    & Log.runLogIO
    & runReader @AppEnv env
    & runErrorNoCallStack @ServerError
    & runErrorNoCallStack @SpotifyError
    & effToHandler

mkServer :: AppEnv -> Application
mkServer env = genericServeT (nt env) server

runServer :: (IOE Eff.:> es) => AppEnv -> Eff es ()
runServer = liftIO . runSettings settings . mkServer
  where
    settings = setPort 7777 defaultSettings
