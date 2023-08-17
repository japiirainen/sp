{-# LANGUAGE TemplateHaskell #-}

module Spotify.Effect.Log (
  Log,
  info,
  debug,
  error,
  runLogIO,
)
where

import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO as Text.IO
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static (Reader, asks)
import Effectful.TH
import Prelude hiding (error)

import Spotify.AppEnv

data Log :: Effect where
  Info :: Text -> Log m ()
  Debug :: Text -> Log m ()
  Error :: Text -> Log m ()

makeEffect ''Log

prefix :: Text -> Text -> Text
prefix p msg = "[" <> p <> "]: " <> msg

runLogIO :: (IOE :> es, Reader AppEnv :> es) => Eff (Log : es) a -> Eff es a
runLogIO = interpret $ \_ -> \case
  Debug msg -> adapt LevelDebug msg
  Info msg -> adapt LevelInfo msg
  Error msg -> adapt LevelError msg
  where
    adapt lvl m = do
      curLvl <- asks logLevel
      when (lvl >= curLvl) $
        liftIO (Text.IO.putStrLn (prefix (Text.pack (show lvl)) m))
