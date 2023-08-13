{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Spotify.Effect.Spotify (
  runSpotifyServant,
  SpotifyAPI (..),
  SpotifyAPIError (..),
  makeTokenRequest,
  module Spotify.Effect.Spotify.Servant,
)
where

import Effectful (Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, asks)
import Effectful.TH (makeEffect)
import Servant.Client (runClientM, (//))

import Spotify.AppEnv
import Spotify.Effect.Spotify.Servant

data SpotifyAPI :: Effect where
  MakeTokenRequest :: Maybe TokenAuthorization -> TokenRequest -> SpotifyAPI m TokenResponse

makeEffect ''SpotifyAPI

newtype SpotifyAPIError = SpotifyAPIError String
  deriving newtype (Show)

runSpotifyServant ::
  (IOE :> es, Error SpotifyAPIError :> es, Reader AppEnv :> es) => Eff (SpotifyAPI : es) a -> Eff es a
runSpotifyServant = interpret $ \_ -> \case
  MakeTokenRequest mauth tokReq -> do
    env <- asks accountsApiEnv
    let route = (routes // token) mauth tokReq
    adapt (runClientM route env)
  where
    adapt m =
      liftIO m >>= \case
        Left err -> throwError $ SpotifyAPIError $ show err
        Right res -> pure res
