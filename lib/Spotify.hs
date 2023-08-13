module Spotify (main) where

import Effectful (Eff, runEff, (:>))
import Effectful.Error.Static (runError)
import Effectful.Reader.Static (runReader)

import Spotify.AppEnv
import Spotify.Config
import Spotify.Effect.FileSystem
import Spotify.Effect.Spotify

prog :: (Config :> es, SpotifyAPI :> es) => Eff es TokenResponse
prog = do
  c <- readConfig
  let auth = Just $ TokenAuthorization (userClientId c) (userClientSecret c)
  let tokReq =
        TokenRequest
          { grant_type = ClientCredentials
          , code = Nothing
          , redirectUri = Nothing
          , refreshToken = Nothing
          }
  makeTokenRequest auth tokReq

main :: IO ()
main = do
  env <- AppEnv <$> accountsEnv <*> mainEnv
  res <-
    runEff
      . runError @SpotifyAPIError
      . runError @FsError
      . runReader @AppEnv env
      . runFileSystemIO
      . runSpotifyServant
      . runConfigIO
      $ prog
  print res
