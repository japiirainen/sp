{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Spotify.Effect.Spotify (
  runSpotifyServant,
  Spotify (..),
  makeTokenRequest,
  makePlayRequest,
  makePauseRequest,
  module Spotify.Effect.Spotify.Servant,
)
where

import Data.Text qualified as Text
import Effectful (Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, asks)
import Effectful.TH (makeEffect)
import Servant (NoContent)
import Servant.Client (runClientM, (//))

import Spotify.AppEnv
import Spotify.Effect.Log (Log)
import Spotify.Effect.Log qualified as Log
import Spotify.Effect.Spotify.Servant
import Spotify.Errors

data Spotify :: Effect where
  MakeTokenRequest :: Maybe TokenAuthorization -> TokenRequest -> Spotify m TokenResponse
  MakePlayRequest :: Authorization -> PlayRequest -> Spotify m NoContent
  MakePauseRequest :: Authorization -> Spotify m NoContent

makeEffect ''Spotify

runSpotifyServant ::
  (IOE :> es, Error SpotifyError :> es, Reader AppEnv :> es, Log :> es) => Eff (Spotify : es) a -> Eff es a
runSpotifyServant = interpret $ \_ -> \case
  MakeTokenRequest mauth tokReq -> do
    env <- asks accountsApiEnv
    let route = (accountRoutes // token) mauth tokReq
    adapt TokenRequestError (runClientM route env)
  MakePlayRequest mauth playReq -> do
    env <- asks mainApiEnv
    let route = (routes // play) playReq mauth
    adapt GenericApiError (runClientM route env)
  MakePauseRequest mauth -> do
    env <- asks mainApiEnv
    let route = (routes // pause) mauth
    adapt GenericApiError (runClientM route env)
  where
    adapt errCon m =
      liftIO m >>= \case
        Left msg -> do
          Log.error $ Text.pack $ show msg
          throwError errCon
        Right res -> pure res
