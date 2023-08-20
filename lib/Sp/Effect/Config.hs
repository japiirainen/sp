{-# LANGUAGE TemplateHaskell #-}

module Sp.Effect.Config (
  readConfig,
  readToken,
  writeConfig,
  runConfigIO,
  Config (..),
) where

import Control.Monad.Extra (unlessM)
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
import System.Directory qualified as Dir

import Sp.Effect.FileSystem (FileSystem)
import Sp.Effect.FileSystem qualified as FS
import Sp.Effect.Log (Log)
import Sp.Effect.Log qualified as Log
import Sp.UserConfig

data Config :: Effect where
  ReadConfig :: ConfigFile -> Config m UserConfig
  ReadToken :: ConfigFile -> Config m Text
  WriteConfig :: ConfigFile -> UserConfig -> Config m ()

makeEffect ''Config

spDir :: String
spDir = "sp/"

configPath :: String -> String
configPath p = spDir <> p

runConfigIO :: (IOE :> es, FileSystem :> es, Log :> es) => Eff (Config : es) a -> Eff es a
runConfigIO = interpret $ \_ -> \case
  ReadConfig path -> do
    fileContents <- FS.readConfigFile (configPath (show path))
    liftIO $ Dhall.input Dhall.auto fileContents
  ReadToken path -> FS.readConfigFile (configPath (show path))
  WriteConfig path config -> do
    let expr = Dhall.embed Dhall.inject config
    let doc = Dhall.Pretty.prettyCharacterSet Unicode expr
    let config' = renderStrict (layoutPretty defaultLayoutOptions doc)
    let fp = configPath (show path)

    Log.debug ("Writing config with contents : " <> config')

    dh <- FS.getConfigHome

    unlessM (liftIO (Dir.doesDirectoryExist (dh <> spDir))) do
      liftIO (Dir.createDirectory (dh <> spDir))

    FS.writeConfigFile fp (fromStrict (encodeUtf8 config'))

    Log.info ("Config file upated at : " <> (Text.pack dh <> Text.pack fp))
