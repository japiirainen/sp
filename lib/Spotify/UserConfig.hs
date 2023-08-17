module Spotify.UserConfig (
  UserConfig (..),
  ConfigFile (..),
) where

import Dhall

data ConfigFile = BaseConfig | TokenFile

instance Show ConfigFile where
  show BaseConfig = "config.dhall"
  show TokenFile = "token"

data UserConfig = UserConfig
  { clientId :: Text
  , clientSecret :: Text
  }
  deriving stock (Generic, Show)

instance FromDhall UserConfig
