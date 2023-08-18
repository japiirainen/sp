module Spotify.UserConfig (
  UserConfig (..),
  ConfigFile (..),
) where

import Dhall

data ConfigFile = BaseConfig | RefreshToken | AccessToken

instance Show ConfigFile where
  show BaseConfig = "config.dhall"
  show RefreshToken = "refresh_token"
  show AccessToken = "access_token"

data UserConfig = UserConfig
  { clientId :: Text
  , clientSecret :: Text
  }
  deriving stock (Generic, Show)

instance FromDhall UserConfig
