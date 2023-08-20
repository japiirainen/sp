module Sp.UserConfig (
  UserConfig (..),
  ConfigFile (..),
) where

import Dhall

data ConfigFile = ConfigFile

instance Show ConfigFile where
  show ConfigFile = "config.dhall"

data UserConfig = UserConfig
  { clientId :: Text
  , clientSecret :: Text
  , refreshToken :: Text
  , accessToken :: Text
  }
  deriving stock (Generic, Show)

instance FromDhall UserConfig
instance ToDhall UserConfig
