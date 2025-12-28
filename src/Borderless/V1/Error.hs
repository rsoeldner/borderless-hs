module Borderless.V1.Error
    ( -- * Error Types
      BorderlessError(..)
    , TokenError(..)
    ) where

import Borderless.Prelude
import Control.Exception (Exception)
import Servant.Client (ClientError)

-- | Errors that can occur when using the Borderless API
data BorderlessError
    = HttpError ClientError
      -- ^ Network or HTTP-level error from Servant
    | TokenError TokenError
      -- ^ Error related to token management
    | ApiError
        { statusCode :: Int
        , errorMessage :: Text
        }
      -- ^ API returned an error response
    deriving stock (Show)

instance Exception BorderlessError

-- | Errors specific to token management
data TokenError
    = TokenFetchFailed Text
      -- ^ Failed to fetch initial token
    | TokenRefreshFailed Text
      -- ^ Failed to refresh token
    | TokenExpired
      -- ^ Token has expired and refresh failed
    | TokenManagerShutdown
      -- ^ Token manager has been shut down
    deriving stock (Eq, Show)

instance Exception TokenError
