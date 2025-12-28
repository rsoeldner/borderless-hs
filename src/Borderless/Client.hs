module Borderless.Client
    ( -- * Configuration
      BorderlessConfig(..)
    , Environment(..)
    , defaultConfig
      -- * Token Manager
    , TokenManager(..)
    , TokenState(..)
    , startTokenManager
    , stopTokenManager
    , withTokenManager
    , getValidToken
      -- * Client Environment
    , getClientEnv
    ) where

import Borderless.Prelude
import Borderless.V1.Auth (AuthM2MSignInDto(..), AuthM2MTokenEntity(..), API)
import Borderless.V1.Error

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, throwIO, try)
import Control.Monad (forever)
import Data.Proxy (Proxy(..))
import Data.Time.Clock (addUTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import System.Random (randomRIO)

import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP.Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified Servant.Client as Client

-- | Borderless API environment
data Environment
    = Sandbox     -- ^ Sandbox environment for testing
    | Production  -- ^ Production environment
    deriving stock (Eq, Show)

-- | Get the base URL for an environment
environmentBaseUrl :: Environment -> Text
environmentBaseUrl Sandbox    = "https://sandbox-api.borderless.xyz"
environmentBaseUrl Production = "https://api.borderless.xyz"

-- | Configuration for the Borderless client
data BorderlessConfig = BorderlessConfig
    { _borderlessConfigClientId        :: Text
      -- ^ Client ID for M2M authentication
    , _borderlessConfigClientSecret    :: Text
      -- ^ Client secret for M2M authentication
    , _borderlessConfigEnvironment     :: Environment
      -- ^ API environment (Sandbox or Production)
    , _borderlessConfigRefreshBufferSec :: Natural
      -- ^ Seconds before expiry to refresh token (default: 300 = 5 minutes)
    , _borderlessConfigMaxRetries      :: Natural
      -- ^ Maximum retries for token refresh on failure (default: 5)
    , _borderlessConfigInitialBackoffMs :: Natural
      -- ^ Initial backoff in milliseconds for retries (default: 1000)
    , _borderlessConfigMaxBackoffMs    :: Natural
      -- ^ Maximum backoff in milliseconds (default: 60000 = 1 minute)
    }

-- | Default configuration (requires clientId and clientSecret to be set)
defaultConfig :: BorderlessConfig
defaultConfig = BorderlessConfig
    { _borderlessConfigClientId         = ""
    , _borderlessConfigClientSecret     = ""
    , _borderlessConfigEnvironment      = Sandbox
    , _borderlessConfigRefreshBufferSec = 300      -- 5 minutes before expiry
    , _borderlessConfigMaxRetries       = 5
    , _borderlessConfigInitialBackoffMs = 1000     -- 1 second
    , _borderlessConfigMaxBackoffMs     = 60000    -- 1 minute
    }

-- | Token state
data TokenState
    = TokenValid
        { _tokenStateToken     :: Text
        , _tokenStateExpiresAt :: UTCTime
        }
      -- ^ Token is valid and can be used
    | TokenRefreshing
        { _tokenStatePreviousToken  :: Maybe Text
        , _tokenStatePreviousExpiry :: Maybe UTCTime
        }
      -- ^ Token is being refreshed (may have previous token to use)
    | TokenFailed
        { _tokenStateLastError  :: Text
        , _tokenStateRetryCount :: Natural
        , _tokenStateRetryAt    :: UTCTime
        }
      -- ^ Token fetch/refresh failed
    | TokenShutdown
      -- ^ Token manager has been shut down
    deriving stock (Eq, Show)

-- | Token manager that handles automatic token refresh
data TokenManager = TokenManager
    { _tokenManagerStateVar   :: TVar TokenState
      -- ^ Current token state (thread-safe)
    , _tokenManagerThreadId   :: ThreadId
      -- ^ Background refresh thread
    , _tokenManagerConfig     :: BorderlessConfig
      -- ^ Configuration
    , _tokenManagerClientEnv  :: Client.ClientEnv
      -- ^ HTTP client environment for auth requests
    }

-- | Create an HTTP client environment for the given base URL
getClientEnv :: Text -> IO Client.ClientEnv
getClientEnv baseUrlText = do
    baseUrl <- Client.parseBaseUrl (T.unpack baseUrlText)
    let managerSettings = TLS.tlsManagerSettings
            { HTTP.Client.managerResponseTimeout =
                HTTP.Client.responseTimeoutMicro (120 * 1000000)  -- 2 minutes
            }
    manager <- TLS.newTlsManagerWith managerSettings
    pure (Client.mkClientEnv manager baseUrl)

-- | Start the token manager with automatic background refresh
startTokenManager :: BorderlessConfig -> IO TokenManager
startTokenManager config = do
    -- Create client environment for auth requests
    clientEnv <- getClientEnv (environmentBaseUrl (_borderlessConfigEnvironment config))

    -- Initialize state as refreshing (will be populated by first fetch)
    stateVar <- newTVarIO TokenRefreshing
        { _tokenStatePreviousToken  = Nothing
        , _tokenStatePreviousExpiry = Nothing
        }

    -- Start background thread
    threadId <- forkIO $ tokenRefreshLoop config clientEnv stateVar

    pure TokenManager
        { _tokenManagerStateVar  = stateVar
        , _tokenManagerThreadId  = threadId
        , _tokenManagerConfig    = config
        , _tokenManagerClientEnv = clientEnv
        }

-- | Stop the token manager gracefully
stopTokenManager :: TokenManager -> IO ()
stopTokenManager TokenManager{..} = do
    -- Signal shutdown
    atomically $ writeTVar _tokenManagerStateVar TokenShutdown
    -- Kill the background thread
    killThread _tokenManagerThreadId

-- | Bracket-style resource management for token manager
withTokenManager :: BorderlessConfig -> (TokenManager -> IO a) -> IO a
withTokenManager config action = do
    tm <- startTokenManager config
    result <- action tm `catch` \(e :: SomeException) -> do
        stopTokenManager tm
        throwIO e
    stopTokenManager tm
    pure result

-- | Get a valid token, waiting if necessary
--
-- This function will:
-- 1. Return immediately if a valid token is available
-- 2. Wait for refresh to complete if one is in progress
-- 3. Throw TokenError if the token manager has failed or shut down
getValidToken :: TokenManager -> IO Text
getValidToken TokenManager{..} = do
    now <- getCurrentTime
    state <- atomically $ readTVar _tokenManagerStateVar

    case state of
        TokenValid{_tokenStateToken, _tokenStateExpiresAt}
            | now < _tokenStateExpiresAt -> pure _tokenStateToken
            | otherwise -> waitForValidToken _tokenManagerStateVar now

        TokenRefreshing{_tokenStatePreviousToken = Just tok, _tokenStatePreviousExpiry = Just expiry}
            | now < expiry -> pure tok
            | otherwise -> waitForValidToken _tokenManagerStateVar now

        TokenRefreshing{} ->
            waitForValidToken _tokenManagerStateVar now

        TokenFailed{_tokenStateLastError} ->
            throwIO $ TokenRefreshFailed _tokenStateLastError

        TokenShutdown ->
            throwIO TokenManagerShutdown

-- | Wait for a valid token to become available
waitForValidToken :: TVar TokenState -> UTCTime -> IO Text
waitForValidToken stateVar now = do
    -- Wait for state change using STM retry
    state <- atomically $ do
        s <- readTVar stateVar
        case s of
            TokenRefreshing{} -> retry  -- Wait for refresh to complete
            _                 -> pure s

    case state of
        TokenValid{_tokenStateToken, _tokenStateExpiresAt}
            | now < _tokenStateExpiresAt -> pure _tokenStateToken
            | otherwise -> throwIO TokenExpired

        TokenFailed{_tokenStateLastError} ->
            throwIO $ TokenRefreshFailed _tokenStateLastError

        TokenShutdown ->
            throwIO TokenManagerShutdown

        TokenRefreshing{} ->
            -- Should not happen due to retry above, but handle it
            waitForValidToken stateVar now

-- | Background loop that manages token refresh
tokenRefreshLoop :: BorderlessConfig -> Client.ClientEnv -> TVar TokenState -> IO ()
tokenRefreshLoop config clientEnv stateVar = do
    -- Initial token fetch
    fetchTokenWithRetry config clientEnv stateVar 0

    -- Main refresh loop
    forever $ do
        -- Check current state
        state <- atomically $ readTVar stateVar

        case state of
            TokenShutdown -> do
                -- Exit the loop
                pure ()

            TokenValid{_tokenStateExpiresAt} -> do
                -- Calculate when to refresh (before expiry)
                now <- getCurrentTime
                let bufferSec = fromIntegral (_borderlessConfigRefreshBufferSec config)
                    refreshAt = addUTCTime (negate bufferSec) _tokenStateExpiresAt
                    delaySeconds = max 1 (diffUTCTime refreshAt now)
                    delayMicros = floor (nominalDiffTimeToSeconds delaySeconds * 1000000)

                -- Sleep until refresh time
                threadDelay delayMicros

                -- Check if we're still supposed to run
                currentState <- atomically $ readTVar stateVar
                case currentState of
                    TokenShutdown -> pure ()
                    _ -> do
                        -- Mark as refreshing but keep old token
                        atomically $ writeTVar stateVar TokenRefreshing
                            { _tokenStatePreviousToken  = Just (_tokenStateToken state)
                            , _tokenStatePreviousExpiry = Just _tokenStateExpiresAt
                            }
                        -- Fetch new token
                        fetchTokenWithRetry config clientEnv stateVar 0

            TokenFailed{_tokenStateRetryAt, _tokenStateRetryCount} -> do
                -- Wait until retry time
                now <- getCurrentTime
                let delaySeconds = max 1 (diffUTCTime _tokenStateRetryAt now)
                    delayMicros = floor (nominalDiffTimeToSeconds delaySeconds * 1000000)
                threadDelay delayMicros

                -- Retry
                fetchTokenWithRetry config clientEnv stateVar _tokenStateRetryCount

            TokenRefreshing{} -> do
                -- Should not normally be in this state in the loop
                -- Wait a bit and try again
                threadDelay 1000000  -- 1 second

-- | Fetch token with retry logic
fetchTokenWithRetry
    :: BorderlessConfig
    -> Client.ClientEnv
    -> TVar TokenState
    -> Natural  -- Current retry count
    -> IO ()
fetchTokenWithRetry config clientEnv stateVar retryCount = do
    result <- try $ fetchToken config clientEnv

    case result of
        Right tokenEntity -> do
            now <- getCurrentTime
            let expiresAt = addUTCTime (fromIntegral (_authM2MTokenEntityExpiresIn tokenEntity)) now
            atomically $ writeTVar stateVar TokenValid
                { _tokenStateToken     = _authM2MTokenEntityAccessToken tokenEntity
                , _tokenStateExpiresAt = expiresAt
                }

        Left (e :: SomeException) -> do
            let newRetryCount = retryCount + 1

            if newRetryCount > _borderlessConfigMaxRetries config
                then do
                    -- Max retries exceeded, mark as failed permanently
                    atomically $ writeTVar stateVar TokenFailed
                        { _tokenStateLastError  = T.pack (show e)
                        , _tokenStateRetryCount = newRetryCount
                        , _tokenStateRetryAt    = error "Max retries exceeded"  -- Will not be used
                        }
                else do
                    -- Calculate backoff with jitter
                    now <- getCurrentTime
                    backoffMs <- calculateBackoff config newRetryCount
                    let backoffSec = fromIntegral backoffMs / 1000
                        retryAtTime = addUTCTime backoffSec now

                    atomically $ writeTVar stateVar TokenFailed
                        { _tokenStateLastError  = T.pack (show e)
                        , _tokenStateRetryCount = newRetryCount
                        , _tokenStateRetryAt    = retryAtTime
                        }

-- | Fetch a new token from the API
fetchToken :: BorderlessConfig -> Client.ClientEnv -> IO AuthM2MTokenEntity
fetchToken BorderlessConfig{_borderlessConfigClientId = cid, _borderlessConfigClientSecret = csec} clientEnv = do
    let request = AuthM2MSignInDto
            { _authM2MSignInDtoClientId     = cid
            , _authM2MSignInDtoClientSecret = csec
            }

    -- Create the client function
    let authClient = Client.client (Proxy :: Proxy ("v1" :> API))

    -- Execute the request
    result <- Client.runClientM (authClient request) clientEnv

    case result of
        Left err -> throwIO $ HttpError err
        Right tokenEntity -> pure tokenEntity

-- | Calculate exponential backoff with jitter
calculateBackoff :: BorderlessConfig -> Natural -> IO Int
calculateBackoff config retryCount = do
    let baseBackoff = _borderlessConfigInitialBackoffMs config * (2 ^ (retryCount - 1))
        cappedBackoff = min baseBackoff (_borderlessConfigMaxBackoffMs config)

    -- Add jitter (0-25% of backoff)
    jitter <- randomRIO (0, fromIntegral cappedBackoff `div` 4)
    pure $ fromIntegral cappedBackoff + jitter
