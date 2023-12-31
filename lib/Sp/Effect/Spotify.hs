{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Sp.Effect.Spotify (
  runSpotifyServant,
  Spotify (..),
  makeTokenRequest,
  makePlayRequest,
  makePauseRequest,
  makeNextRequest,
  makePrevRequest,
  makeSeekRequest,
  makeSearchTracksRequest,
  makeSearchAlbumsRequest,
  module Sp.Effect.Spotify.Servant,
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

import Sp.AppEnv
import Sp.Effect.Log (Log)
import Sp.Effect.Log qualified as Log
import Sp.Effect.Spotify.Servant
import Sp.Errors

data Spotify :: Effect where
  MakeTokenRequest :: Maybe TokenAuthorization -> TokenRequest -> Spotify m TokenResponse
  MakePlayRequest :: Authorization -> PlayRequest -> Spotify m NoContent
  MakePauseRequest :: Authorization -> Spotify m NoContent
  MakeNextRequest :: Authorization -> Spotify m NoContent
  MakePrevRequest :: Authorization -> Spotify m NoContent
  MakeSeekRequest :: Authorization -> Int -> Spotify m NoContent
  MakeSearchTracksRequest :: Authorization -> SearchParams -> Spotify m SearchTracksResponse
  MakeSearchAlbumsRequest :: Authorization -> SearchParams -> Spotify m SearchAlbumsResponse

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
  (IOE :> es, Error SpError :> es, Reader AppEnv :> es, Log :> es) => Eff (Spotify : es) a -> Eff es a
runSpotifyServant = interpret $ \_ -> \case
  MakeTokenRequest mauth tokReq -> do
    env <- asks accountsApiEnv
    let route = (accountRoutes // token) mauth tokReq
    adapt TokenRequestError (runClientM route env)
  MakePlayRequest auth playReq -> do
    env <- asks mainApiEnv
    let route = (routes // play) playReq auth
    adapt GenericApiError (runClientM route env)
  MakePauseRequest auth -> do
    env <- asks mainApiEnv
    let route = (routes // pause) auth
    adapt GenericApiError (runClientM route env)
  MakeNextRequest auth -> do
    env <- asks mainApiEnv
    let route = (routes // next_) auth
    adapt GenericApiError (runClientM route env)
  MakePrevRequest auth -> do
    env <- asks mainApiEnv
    let route = (routes // prev) auth
    adapt GenericApiError (runClientM route env)
  MakeSeekRequest auth seekMs -> do
    env <- asks mainApiEnv
    let route = (routes // seek) seekMs auth
    adapt GenericApiError (runClientM route env)
  MakeSearchTracksRequest auth SearchParams {..} -> do
    env <- asks mainApiEnv
    let route = (routes // searchTracks) auth q type_ market limit offset include_external
    adapt GenericApiError (runClientM route env)
  MakeSearchAlbumsRequest auth SearchParams {..} -> do
    env <- asks mainApiEnv
    let route = (routes // searchAlbums) auth q type_ market limit offset include_external
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
