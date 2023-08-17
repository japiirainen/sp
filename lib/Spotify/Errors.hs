module Spotify.Errors (SpotifyError (..)) where

import Data.Text (Text)
import Data.Text qualified as Text

data SpotifyError
  = UnexpectedError Text
  | ConfigNotFound
  | TokenRequestError
  | GenericApiError
  | AuthorizationError

instance Show SpotifyError where
  show = \case
    UnexpectedError msg -> "Unexpected error: " <> Text.unpack msg
    ConfigNotFound -> "Necessary configuration file not found."
    TokenRequestError -> "Token request failed. Try to run authorize flow."
    GenericApiError -> "Something bad happened. Please open an issue at https://github.com/japiirainen/spotify"
    AuthorizationError -> "Error during the authorization flow."
