module Sp.CallbackServer (runServer) where

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
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setPort,
 )
import Servant
import Servant.API.Generic
import Servant.Server.Generic

import Sp.Types (CBServer)

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
  Eff '[IOE] (Either ServerError a) ->
  Handler a
effToHandler computation = do
  v <- liftIO . runEff $! computation
  case v of
    Left err -> T.throwError err
    Right a -> pure a

nt :: CBServer a -> Handler a
nt prog =
  prog
    & runErrorNoCallStack @ServerError
    & runConcurrent
    & effToHandler

mkServer :: Chan Text -> Application
mkServer chan = genericServeT nt (server chan)

runServer :: (IOE Eff.:> es) => Chan Text -> Eff es ()
runServer = liftIO . runSettings settings . mkServer
  where
    settings = setPort 7777 defaultSettings
