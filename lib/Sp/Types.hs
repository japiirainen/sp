module Sp.Types where

import Data.Kind (Type)
import Effectful (Eff, IOE)
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)
import Servant (ServerError)

import Effectful.Concurrent (Concurrent)
import Sp.AppEnv (AppEnv)
import Sp.Effect.Browser (Browser)
import Sp.Effect.Config (Config)
import Sp.Effect.Console (Console)
import Sp.Effect.FileSystem (FileSystem)
import Sp.Effect.Log (Log)
import Sp.Effect.Spotify (Spotify)
import Sp.Errors (SpError)

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
     , Error SpError
     , Concurrent
     , IOE
     ]

type CBServer :: Type -> Type
type CBServer =
  Eff
    '[ Error ServerError
     , Concurrent
     , IOE
     ]
