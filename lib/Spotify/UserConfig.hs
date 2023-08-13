module Spotify.UserConfig (
  UserConfig (..),
) where

import Dhall

data UserConfig = UserConfig
  { clientId :: Text
  , clientSecret :: Text
  }
  deriving stock (Generic, Show)

instance FromDhall UserConfig
