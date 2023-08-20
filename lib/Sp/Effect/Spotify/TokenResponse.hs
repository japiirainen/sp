module Sp.Effect.Spotify.TokenResponse (TokenResponse (..)) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Web.FormUrlEncoded (FromForm)

data TokenResponse = TokenResponse
  { access_token :: Text
  , refresh_token :: Maybe Text
  , token_type :: Text
  , expires_in :: Int
  }
  deriving stock (Generic, Show)

instance FromJSON TokenResponse
instance FromForm TokenResponse
