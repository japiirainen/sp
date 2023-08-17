{-# LANGUAGE TemplateHaskell #-}

module Spotify.Effect.Browser (
  Browser,
  open,
  runBrowserIO,
)
where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Web.Browser (openBrowser)

import Spotify.Effect.Log (Log)
import Spotify.Effect.Log qualified as Log

data Browser :: Effect where
  Open :: String -> Browser m Bool

makeEffect ''Browser

runBrowserIO :: (IOE :> es, Log :> es) => Eff (Browser : es) a -> Eff es a
runBrowserIO = interpret $ \_ -> \case
  Open path -> do
    successful <- liftIO $ openBrowser path
    if successful
      then pure True
      else do
        Log.error "Could not open web browser."
        pure False
