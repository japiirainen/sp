module Sp.Effect.Spotify.Servant (
  accountRoutes,
  accountsEnv,
  accountsBaseUrl,
  mainEnv,
  routes,
  authorizeApi,
  scopeFromList,
  trackSearch,
  albumSearch,
  ResponseType (..),
  AccountRoutes (..),
  TokenGrantType (..),
  TokenAuthorization (..),
  TokenRequest (..),
  TokenResponse (..),
  Routes (..),
  Authorization (..),
  PlayRequest (..),
  Scope (..),
  SearchAlbumsResponse (..),
  SearchTracksResponse (..),
  Artist (..),
  Track (..),
  Tracks (..),
  SearchParams (..),
  Albums (..),
  Album (..),
)
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON (..), withText)
import Data.Aeson.Types (FromJSON (..))
import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 as C8
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Client.TLS qualified as TLS
import Servant.API
import Servant.API.Generic
import Servant.Client
import Web.FormUrlEncoded (ToForm (..))

import Sp.Effect.Spotify.TokenResponse

type TokenRoute =
  "token"
    :> Header "Authorization" TokenAuthorization
    :> ReqBody '[FormUrlEncoded] TokenRequest
    :> Post '[JSON] TokenResponse

type AuthorizeRoute =
  "authorize"
    :> QueryParam' '[Required, Strict] "client_id" String
    :> QueryParam' '[Required, Strict] "response_type" ResponseType
    :> QueryParam' '[Required, Strict] "redirect_uri" String
    :> QueryParam "state" State
    :> QueryParam' '[Required, Strict] "scope" Scope
    :> QueryParam' '[Required, Strict] "show_dialog" Bool
    :> Get '[PlainText] String

authorizeApi :: Proxy AuthorizeRoute
authorizeApi = Proxy

newtype AccountRoutes route = AccountRoutes
  {token :: route :- TokenRoute}
  deriving stock (Generic)

accountRoutes :: AccountRoutes (AsClientT ClientM)
accountRoutes = client (Proxy :: Proxy (NamedRoutes AccountRoutes))

type AuthorizedRequest = Header' '[Strict, Required] "Authorization" Authorization

newtype Offset = Offset
  {position :: Int}
  deriving stock (Generic, Show)

instance ToJSON Offset
instance FromJSON Offset

data PlayRequest = PlayRequest
  { context_uri :: Maybe Text
  , uris :: Maybe [Text]
  , offset :: Maybe Offset
  , position_ms :: Int
  }
  deriving stock (Generic, Show)

instance ToJSON PlayRequest
instance FromJSON PlayRequest

data Artist = Artist
  { name :: Text
  , uri :: Text
  }
  deriving stock (Generic, Show)

instance ToJSON Artist
instance FromJSON Artist

data Track = Track
  { name :: Text
  , uri :: Text
  , artists :: [Artist]
  }
  deriving stock (Generic, Show)

instance ToJSON Track
instance FromJSON Track

data Tracks = Tracks
  { href :: Text
  , limit :: Int
  , next :: Maybe Text
  , previous :: Maybe Text
  , offset :: Int
  , total :: Int
  , items :: [Track]
  }
  deriving stock (Generic, Show)

instance ToJSON Tracks
instance FromJSON Tracks

data Album = Album
  { name :: Text
  , uri :: Text
  , artists :: [Artist]
  }
  deriving stock (Generic, Show)

instance ToJSON Album
instance FromJSON Album

data Albums = Albums
  { href :: Text
  , limit :: Int
  , next :: Maybe Text
  , previous :: Maybe Text
  , offset :: Int
  , total :: Int
  , items :: [Album]
  }
  deriving stock (Generic, Show)

instance ToJSON Albums
instance FromJSON Albums

newtype SearchTracksResponse = SearchTracksResponse
  { tracks :: Tracks
  }
  deriving stock (Generic, Show)

instance ToJSON SearchTracksResponse
instance FromJSON SearchTracksResponse

newtype SearchAlbumsResponse = SearchAlbumsResponse
  { albums :: Albums
  }
  deriving stock (Generic, Show)

instance ToJSON SearchAlbumsResponse
instance FromJSON SearchAlbumsResponse

type Search ret =
  "search"
    :> AuthorizedRequest
    :> QueryParam' '[Required, Strict] "q" Text
    :> QueryParam' '[Required, Strict] "type" Text
    :> QueryParam "market" Text
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "include_external" Text
    :> Get '[JSON] ret

data SearchParams = SearchParams
  { q :: Text
  , type_ :: Text
  , market :: Maybe Text
  , limit :: Maybe Int
  , offset :: Maybe Int
  , include_external :: Maybe Text
  }
  deriving stock (Show)

trackSearch :: Text -> SearchParams
trackSearch query = SearchParams query "track" Nothing (Just 5) Nothing Nothing

albumSearch :: Text -> SearchParams
albumSearch query = SearchParams query "album" Nothing (Just 5) Nothing Nothing

data Routes route = Routes
  { play :: route :- "me" :> "player" :> "play" :> ReqBody '[JSON] PlayRequest :> AuthorizedRequest :> PutNoContent
  , pause :: route :- "me" :> "player" :> "pause" :> AuthorizedRequest :> PutNoContent
  , next_ :: route :- "me" :> "player" :> "next" :> AuthorizedRequest :> PostNoContent
  , prev :: route :- "me" :> "player" :> "previous" :> AuthorizedRequest :> PostNoContent
  , seek :: route :- "me" :> "player" :> "seek" :> QueryParam' '[Required, Strict] "position_ms" Int :> AuthorizedRequest :> PutNoContent
  , searchTracks :: route :- Search SearchTracksResponse
  , searchAlbums :: route :- Search SearchAlbumsResponse
  }
  deriving stock (Generic)

routes :: Routes (AsClientT ClientM)
routes = client (Proxy :: Proxy (NamedRoutes Routes))

data ResponseType = ResponseType

instance Show ResponseType where
  show = const "code"

wrap :: String -> (String -> a) -> [(a, String)]
wrap str convert = [(convert str, "")]

instance Read ResponseType where
  readsPrec _ str = wrap str (const ResponseType)

instance ToHttpApiData ResponseType where
  toQueryParam = Text.pack . show

instance FromHttpApiData ResponseType where
  parseQueryParam _ = Right ResponseType

newtype State = State
  { getState :: String
  }

instance Show State where
  show = getState

instance Read State where
  readsPrec _ str =
    wrap str (\s -> State {getState = s})

instance FromHttpApiData State where
  parseQueryParam param = Right (read (Text.unpack param) :: State)

instance ToHttpApiData State where
  toQueryParam = Text.pack . show

newtype Scope = Scope
  {getScopes :: [Text]}

instance Show Scope where
  show s = Prelude.unwords $ Prelude.map Text.unpack (getScopes s)

instance ToHttpApiData Scope where
  toQueryParam = Text.pack . show

instance FromJSON Scope where
  parseJSON = withText "Scope" $ \scopes -> do
    let parsedScope = scopeFromList (Text.words scopes)
    return parsedScope

instance ToJSON Scope where
  toJSON = toJSON . show

scopeFromList :: [Text] -> Scope
scopeFromList = Scope

data TokenGrantType
  = AuthorizationCodeGrantType
  | RefreshTokenGrantType
  | ClientCredentials

instance ToHttpApiData TokenGrantType where
  toQueryParam AuthorizationCodeGrantType = "authorization_code"
  toQueryParam RefreshTokenGrantType = "refresh_token"
  toQueryParam ClientCredentials = "client_credentials"

data TokenRequest = TokenRequest
  { grant_type :: TokenGrantType
  , code :: Maybe Text
  , redirect_uri :: Maybe Text
  , refresh_token :: Maybe Text
  }
  deriving stock (Generic)

instance ToForm TokenRequest

data TokenAuthorization = TokenAuthorization
  { clientId :: Text
  , clientSecretKey :: Text
  }
  deriving stock (Generic, Eq)

newtype Authorization = Authorization
  {accessToken :: Text}
  deriving stock (Generic, Eq)

instance FromJSON Authorization
instance ToJSON Authorization

instance Show Authorization where
  show a = "Bearer " <> Text.unpack (accessToken a)

instance ToHttpApiData Authorization where
  toQueryParam = Text.pack . show

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

--
accountsBaseUrl :: BaseUrl
accountsBaseUrl =
  BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "accounts.spotify.com"
    , baseUrlPath = ""
    , baseUrlPort = 443
    }

accountsApiBaseUrl :: BaseUrl
accountsApiBaseUrl =
  BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "accounts.spotify.com"
    , baseUrlPath = "/api"
    , baseUrlPort = 443
    }

accountsEnv :: (MonadIO m) => m ClientEnv
accountsEnv = do
  manager <- TLS.newTlsManager
  pure $ mkClientEnv manager accountsApiBaseUrl

mainBaseUrl :: BaseUrl
mainBaseUrl =
  BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "api.spotify.com"
    , baseUrlPath = "/v1"
    , baseUrlPort = 443
    }

mainEnv :: (MonadIO m) => m ClientEnv
mainEnv = do
  manager <- TLS.newTlsManager
  pure $ mkClientEnv manager mainBaseUrl
