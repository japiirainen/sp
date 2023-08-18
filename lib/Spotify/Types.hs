module Spotify.Types where

import Data.Kind (Type)
import Effectful (Eff, IOE)
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)
import Servant (ServerError)

import Effectful.Concurrent (Concurrent)
import Spotify.AppEnv (AppEnv)
import Spotify.Effect.Browser (Browser)
import Spotify.Effect.Config (Config)
import Spotify.Effect.Console (Console)
import Spotify.Effect.FileSystem (FileSystem)
import Spotify.Effect.Log (Log)
import Spotify.Effect.Spotify (Spotify)
import Spotify.Errors (SpotifyError)

type Program :: Type -> Type
type Program =
  Eff
    '[ Browser
     , Spotify
     , Config
     , FileSystem
     , Log
     , Console
     , Reader AppEnv
     , Error SpotifyError
     , Concurrent
     , IOE
     ]

type CBServer :: Type -> Type
type CBServer =
  Eff
    '[ Config
     , FileSystem
     , Log
     , Reader AppEnv
     , Error ServerError
     , Error SpotifyError
     , Concurrent
     , IOE
     ]
