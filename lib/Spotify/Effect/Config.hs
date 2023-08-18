{-# LANGUAGE TemplateHaskell #-}

module Spotify.Effect.Config (
  readConfig,
  readToken,
  writeToken,
  runConfigIO,
  Config (..),
) where

import Data.ByteString.Lazy (ByteString)
import Dhall
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH

import Spotify.Effect.FileSystem
import Spotify.UserConfig

data Config :: Effect where
  ReadConfig :: ConfigFile -> Config m UserConfig
  ReadToken :: ConfigFile -> Config m Text
  WriteToken :: ByteString -> Config m ()

makeEffect ''Config

runConfigIO :: (IOE :> es, FileSystem :> es) => Eff (Config : es) a -> Eff es a
runConfigIO = interpret $ \_ -> \case
  ReadConfig path -> do
    fileContents <- readConfigFile ("spotify/" <> show path)
    liftIO $ input auto fileContents
  ReadToken path -> readConfigFile ("spotify/" <> show path)
  WriteToken tok -> writeConfigFile ("spotify/" <> show TokenFile) tok
