module Borderless.V1.Auth
    ( -- * Request Types
      AuthM2MSignInDto(..)
      -- * Response Types
    , AuthM2MTokenEntity(..)
      -- * Servant API
    , API
    ) where

import Borderless.Prelude

-- | Request body for M2M authentication
data AuthM2MSignInDto = AuthM2MSignInDto
    { _authM2MSignInDtoClientId     :: Text
    , _authM2MSignInDtoClientSecret :: Text
    } deriving stock (Eq, Generic, Show)

instance FromJSON AuthM2MSignInDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_authM2MSignInDto" }

instance ToJSON AuthM2MSignInDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_authM2MSignInDto" }

-- | Response from M2M token endpoint
data AuthM2MTokenEntity = AuthM2MTokenEntity
    { _authM2MTokenEntityAccessToken :: Text
    , _authM2MTokenEntityTokenType   :: Text
    , _authM2MTokenEntityExpiresIn   :: Int  -- seconds until expiration
    } deriving stock (Eq, Generic, Show)

instance FromJSON AuthM2MTokenEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_authM2MTokenEntity" }

instance ToJSON AuthM2MTokenEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_authM2MTokenEntity" }

-- | Authentication API (returns 201 Created)
type API =
    "auth" :> "m2m" :> "token"
        :> ReqBody '[JSON] AuthM2MSignInDto
        :> Post201 '[JSON] AuthM2MTokenEntity
