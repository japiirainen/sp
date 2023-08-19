module Spotify.CallbackServer (runServer) where

import Control.Monad.Except qualified as T
import Data.Function ((&))
import Data.Kind (Type)
import Data.Text (Text)
import Effectful (Eff, IOE, liftIO, runEff)
import Effectful qualified as Eff
import Effectful.Concurrent (runConcurrent)
import Effectful.Concurrent.Chan (Chan)
import Effectful.Concurrent.Chan qualified as Chan
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setPort,
 )
import Servant
import Servant.API.Generic
import Servant.Server.Generic

import Spotify.AppEnv (AppEnv)
import Spotify.Effect.Config qualified as Config
import Spotify.Effect.FileSystem qualified as FileSystem
import Spotify.Effect.Log qualified as Log
import Spotify.Errors (SpotifyError)
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

server :: Chan Text -> Routes (AsServerT CBServer)
server chan =
  Routes
    { callback = handler
    }
  where
    handler :: Maybe Text -> Text -> CBServer String
    handler _ code = Chan.writeChan chan code >> pure "authorized"

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
    & runConcurrent
    & effToHandler

mkServer :: Chan Text -> AppEnv -> Application
mkServer chan env = genericServeT (nt env) (server chan)

runServer :: (IOE Eff.:> es) => Chan Text -> AppEnv -> Eff es ()
runServer chan = liftIO . runSettings settings . mkServer chan
  where
    settings = setPort 7777 defaultSettings
