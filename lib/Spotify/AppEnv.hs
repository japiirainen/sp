module Spotify.AppEnv (AppEnv (..)) where

import Servant.Client (ClientEnv)

data AppEnv = AppEnv
  { accountsApiEnv :: ClientEnv
  , mainApiEnv :: ClientEnv
  }
