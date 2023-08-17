module Spotify.AppEnv (AppEnv (..), LogLevel (..)) where

import Servant.Client (ClientEnv)

data LogLevel
  = LevelDebug
  | LevelInfo
  | LevelError
  deriving stock (Eq, Ord)

instance Show LogLevel where
  show LevelDebug = "DEBUG"
  show LevelError = "ERROR"
  show LevelInfo = "INFO"

data AppEnv = AppEnv
  { accountsApiEnv :: ClientEnv
  , mainApiEnv :: ClientEnv
  , logLevel :: LogLevel
  }

instance Show AppEnv where
  show AppEnv {logLevel} = "{ debug = " <> show logLevel <> "\n}"
