{-# LANGUAGE TemplateHaskell #-}

module Spotify.Effect.FileSystem (
  readFile,
  readConfigFile,
  FsError,
  runFileSystemIO,
  FileSystem,
) where

import Control.Exception (IOException)
import Control.Monad.Catch (catch)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Effectful (Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.TH (makeEffect)
import System.XDG qualified as XDG
import Prelude hiding (readFile)

import Data.Text.IO qualified as TIO

data FileSystem :: Effect where
  ReadFile :: FilePath -> FileSystem m Text
  ReadConfigFile :: FilePath -> FileSystem m Text

makeEffect ''FileSystem

newtype FsError = FsError String deriving stock (Show)

runFileSystemIO :: (IOE :> es, Error FsError :> es) => Eff (FileSystem : es) a -> Eff es a
runFileSystemIO = interpret $ \_ -> \case
  ReadFile path -> adapt (TIO.readFile path)
  ReadConfigFile path ->
    liftIO (XDG.readConfigFile path) >>= \case
      Nothing -> throwError $ FsError $ show path <> " not found."
      Just c -> pure $ toStrict $ decodeUtf8 c
  where
    adapt m = liftIO m `catch` \(e :: IOException) -> throwError . FsError $ show e
