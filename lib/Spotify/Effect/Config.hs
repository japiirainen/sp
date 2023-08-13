{-# LANGUAGE TemplateHaskell #-}

module Spotify.Effect.Config (
  readConfig,
  runConfigIO,
  Config (..),
) where

import Dhall
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH

import Spotify.Effect.FileSystem
import Spotify.UserConfig

data Config :: Effect where
  ReadConfig :: Config m UserConfig

makeEffect ''Config

runConfigIO :: (IOE :> es, FileSystem :> es) => Eff (Config : es) a -> Eff es a
runConfigIO = interpret $ \_ -> \case
  ReadConfig -> do
    fileContents <- readConfigFile "Spotify.dhall"
    liftIO $ input auto fileContents
