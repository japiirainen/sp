{-# LANGUAGE TemplateHaskell #-}

module Spotify.Effect.Config (
  readConfig,
  readToken,
  writeConfig,
  runConfigIO,
  Config (..),
) where

import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Dhall qualified
import Dhall.Pretty (CharacterSet (..))
import Dhall.Pretty qualified
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

import Spotify.Effect.FileSystem (FileSystem)
import Spotify.Effect.FileSystem qualified as FS
import Spotify.Effect.Log (Log)
import Spotify.Effect.Log qualified as Log
import Spotify.UserConfig

data Config :: Effect where
  ReadConfig :: ConfigFile -> Config m UserConfig
  ReadToken :: ConfigFile -> Config m Text
  WriteConfig :: ConfigFile -> UserConfig -> Config m ()

makeEffect ''Config

runConfigIO :: (IOE :> es, FileSystem :> es, Log :> es) => Eff (Config : es) a -> Eff es a
runConfigIO = interpret $ \_ -> \case
  ReadConfig path -> do
    fileContents <- FS.readConfigFile ("spotify/" <> show path)
    liftIO $ Dhall.input Dhall.auto fileContents
  ReadToken path -> FS.readConfigFile ("spotify/" <> show path)
  WriteConfig path config -> do
    let expr = Dhall.embed Dhall.inject config
    let doc = Dhall.Pretty.prettyCharacterSet Unicode expr
    let config' = renderStrict (layoutPretty defaultLayoutOptions doc)
    let fp = "spotify/" <> show path
    Log.debug ("Writing config with contents : " <> config')
    FS.writeConfigFile fp (fromStrict (encodeUtf8 config'))
    dh <- FS.getConfigHome
    Log.info ("Config file upated at : " <> (Text.pack dh <> Text.pack fp))
