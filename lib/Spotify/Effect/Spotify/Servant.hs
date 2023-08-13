module Spotify.Effect.Spotify.Servant (
  routes,
  accountsEnv,
  mainEnv,
  Routes (..),
  TokenGrantType (..),
  TokenAuthorization (..),
  TokenRequest (..),
  TokenResponse (..),
)
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON)
import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 as C8
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Client.TLS qualified as TLS
import Servant.API
import Servant.API.Generic
import Servant.Client
import Web.FormUrlEncoded (FromForm, ToForm (..))

type TokenRoute =
  "token"
    :> Header "Authorization" TokenAuthorization
    :> ReqBody '[FormUrlEncoded] TokenRequest
    :> Post '[JSON] TokenResponse

newtype Routes route = Routes
  {token :: route :- TokenRoute}
  deriving stock (Generic)

routes :: Routes (AsClientT ClientM)
routes = client (Proxy :: Proxy (NamedRoutes Routes))

data TokenResponse = TokenResponse
  { access_token :: Text
  , token_type :: Text
  , expires_in :: Int
  }
  deriving stock (Generic, Show)

instance FromJSON TokenResponse
instance FromForm TokenResponse

data TokenGrantType
  = AuthorizationCodeGrantType
  | RefreshTokenGrantType
  | ClientCredentials

instance ToHttpApiData TokenGrantType where
  toQueryParam AuthorizationCodeGrantType = "authorization_code"
  toQueryParam RefreshTokenGrantType = "refresh-token"
  toQueryParam ClientCredentials = "client_credentials"

data TokenRequest = TokenRequest
  { grant_type :: TokenGrantType
  , code :: Maybe Text
  , redirectUri :: Maybe Text
  , refreshToken :: Maybe Text
  }
  deriving stock (Generic)

instance ToForm TokenRequest

data TokenAuthorization = TokenAuthorization
  { clientId :: Text
  , clientSecretKey :: Text
  }
  deriving stock (Generic, Eq)

instance Show TokenAuthorization where
  show authorization = value
    where
      clientIdKey = Text.unpack $ clientId authorization
      clientSecret = Text.unpack $ clientSecretKey authorization
      keys = clientIdKey ++ ":" ++ clientSecret
      encodedKeys = encode $ C8.pack keys
      value = "Basic " ++ C8.unpack encodedKeys

instance ToHttpApiData TokenAuthorization where
  toQueryParam = Text.pack . show

accountsBaseUrl :: BaseUrl
accountsBaseUrl =
  BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "accounts.spotify.com"
    , baseUrlPath = "/api"
    , baseUrlPort = 443
    }

accountsEnv :: (MonadIO m) => m ClientEnv
accountsEnv = do
  manager <- TLS.newTlsManager
  pure $ mkClientEnv manager accountsBaseUrl

mainBaseUrl :: BaseUrl
mainBaseUrl =
  BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "spotify.com"
    , baseUrlPath = "/api"
    , baseUrlPort = 443
    }

mainEnv :: (MonadIO m) => m ClientEnv
mainEnv = do
  manager <- TLS.newTlsManager
  pure $ mkClientEnv manager mainBaseUrl
