{-# LANGUAGE TemplateHaskell #-}

module Sp.Effect.FileSystem (
  readFile,
  readConfigFile,
  writeConfigFile,
  getConfigHome,
  runFileSystemIO,
  FileSystem,
) where

import Control.Exception (IOException)
import Control.Monad.Catch (catch)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Effectful (Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.TH (makeEffect)
import System.XDG qualified as XDG
import Prelude hiding (readFile)

import Sp.Effect.Log (Log)
import Sp.Effect.Log qualified as Log
import Sp.Errors (SpError (..))

data FileSystem :: Effect where
  ReadFile :: FilePath -> FileSystem m Text
  ReadConfigFile :: FilePath -> FileSystem m Text
  WriteConfigFile :: FilePath -> ByteString -> FileSystem m ()
  GetConfigHome :: FileSystem m FilePath

makeEffect ''FileSystem

runFileSystemIO :: (IOE :> es, Log :> es, Error SpError :> es) => Eff (FileSystem : es) a -> Eff es a
runFileSystemIO = interpret $ \_ -> \case
  ReadFile path -> adapt (TIO.readFile path)
  ReadConfigFile path ->
    liftIO (XDG.readConfigFile path) >>= \case
      Nothing -> throwError ConfigNotFound
      Just c -> pure $ toStrict $ decodeUtf8 c
  WriteConfigFile path content -> liftIO $ XDG.writeConfigFile path content
  GetConfigHome -> adapt XDG.getConfigHome
  where
    adapt m =
      liftIO m `catch` \(e :: IOException) -> do
        Log.error $ Text.pack $ show e
        throwError GenericApiError
