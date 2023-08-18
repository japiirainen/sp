{-# LANGUAGE TemplateHaskell #-}

module Spotify.Effect.Console (
  Console,
  readLine,
  runConsole,
)
where

import Data.Text (Text)
import Data.Text.IO as Text.IO
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Prelude hiding (error)

data Console :: Effect where
  ReadLine :: Console m Text

makeEffect ''Console

runConsole :: (IOE :> es) => Eff (Console : es) a -> Eff es a
runConsole = interpret $ \_ -> \case
  ReadLine -> liftIO Text.IO.getLine
