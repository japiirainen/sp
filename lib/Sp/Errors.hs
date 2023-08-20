module Sp.Errors (SpError (..)) where

import Data.Text (Text)
import Data.Text qualified as Text

data SpError
  = UnexpectedError Text
  | ConfigNotFound
  | TokenRequestError
  | GenericApiError
  | AuthorizationError
  | InvalidTokenError

instance Show SpError where
  show = \case
    UnexpectedError msg -> "Unexpected error: " <> Text.unpack msg
    ConfigNotFound -> "Necessary configuration file not found."
    TokenRequestError -> "Token request failed. Try to run authorize flow."
    GenericApiError -> "Something bad happened. Please open an issue at https://github.com/japiirainen/sp"
    AuthorizationError -> "Error during the authorization flow."
    InvalidTokenError -> "Invalid access/refresh token."
