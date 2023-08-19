{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Spotify.Effect.Spotify (
  runSpotifyServant,
  Spotify (..),
  makeTokenRequest,
  makePlayRequest,
  makePauseRequest,
  makeNextRequest,
  makePrevRequest,
  makeSeekRequest,
  module Spotify.Effect.Spotify.Servant,
)
where

import Data.Text (Text)
import Data.Text qualified as Text
import Effectful (Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, asks)
import Effectful.TH (makeEffect)
import Network.HTTP.Types (Status (..))
import Servant (NoContent)
import Servant.Client (ClientError (..), ResponseF (..), runClientM, (//))

import Spotify.AppEnv
import Spotify.Effect.Log (Log)
import Spotify.Effect.Log qualified as Log
import Spotify.Effect.Spotify.Servant
import Spotify.Errors

data Spotify :: Effect where
  MakeTokenRequest :: Maybe TokenAuthorization -> TokenRequest -> Spotify m TokenResponse
  MakePlayRequest :: Authorization -> PlayRequest -> Spotify m NoContent
  MakePauseRequest :: Authorization -> Spotify m NoContent
  MakeNextRequest :: Authorization -> Spotify m NoContent
  MakePrevRequest :: Authorization -> Spotify m NoContent
  MakeSeekRequest :: Authorization -> Int -> Spotify m NoContent

makeEffect ''Spotify

ppClientError :: ClientError -> Text
ppClientError = \case
  DecodeFailure msg _ -> "Decode failure : " <> msg
  UnsupportedContentType m _ -> "Unsupported content type : " <> Text.pack (show m)
  InvalidContentTypeHeader _ -> "Invalid content-type header."
  FailureResponse _ resp -> Text.pack (ppResp resp)
  ConnectionError ex -> "Connection error : " <> Text.pack (show ex)
  where
    ppResp resp =
      "{ status = "
        <> show (statusCode (responseStatusCode resp))
        <> "\n"
        <> "         , responseBody = "
        <> show (responseBody resp)
        <> "\n         }"

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
  MakeNextRequest mauth -> do
    env <- asks mainApiEnv
    let route = (routes // next) mauth
    adapt GenericApiError (runClientM route env)
  MakePrevRequest mauth -> do
    env <- asks mainApiEnv
    let route = (routes // prev) mauth
    adapt GenericApiError (runClientM route env)
  MakeSeekRequest mauth seekMs -> do
    env <- asks mainApiEnv
    let route = (routes // seek) seekMs mauth
    adapt GenericApiError (runClientM route env)
  where
    adapt errCon m =
      liftIO m >>= \case
        Left msg -> do
          case msg of
            FailureResponse _ resp ->
              case statusCode (responseStatusCode resp) of
                401 -> throwError InvalidTokenError
                _ -> Log.error (ppClientError msg) >> throwError errCon
            _ -> Log.error (ppClientError msg) >> throwError errCon
        Right res -> pure res
