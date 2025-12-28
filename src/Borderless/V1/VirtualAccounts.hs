module Borderless.V1.VirtualAccounts
    ( -- * Entity Types
      VirtualAccountEntity(..)
    , VirtualAccountStatus(..)
      -- * Request Types
    , VirtualAccountCreateDto(..)
    , _VirtualAccountCreateDto
      -- * Servant API
    , API
    ) where

import Borderless.Prelude
import Borderless.V1.Types.Common
import Borderless.V1.Types.Enums
import Borderless.V1.Types.Pagination

import qualified Data.Aeson as A

-- | Virtual account status
data VirtualAccountStatus
    = VirtualAccountActive
    | VirtualAccountNotActive
    | VirtualAccountRejected
    deriving stock (Eq, Generic, Show)

instance FromJSON VirtualAccountStatus where
    parseJSON = A.withText "VirtualAccountStatus" $ \case
        "Active"    -> pure VirtualAccountActive
        "NotActive" -> pure VirtualAccountNotActive
        "Rejected"  -> pure VirtualAccountRejected
        other       -> fail $ "Unknown VirtualAccountStatus: " <> show other

instance ToJSON VirtualAccountStatus where
    toJSON VirtualAccountActive    = A.String "Active"
    toJSON VirtualAccountNotActive = A.String "NotActive"
    toJSON VirtualAccountRejected  = A.String "Rejected"

-- | Virtual account entity
data VirtualAccountEntity = VirtualAccountEntity
    { _virtualAccountEntityId                   :: VirtualAccountId
    , _virtualAccountEntityAccountId            :: AccountId
    , _virtualAccountEntityCounterPartyIdentityId :: Maybe IdentityId
    , _virtualAccountEntityStatus               :: VirtualAccountStatus
    , _virtualAccountEntityAsset                :: AssetId
    , _virtualAccountEntityFiat                 :: FiatCurrencyId
    , _virtualAccountEntityInstructions         :: Maybe A.Value  -- Banking details
    , _virtualAccountEntityCreatedAt            :: UTCTime
    , _virtualAccountEntityFailureReason        :: Maybe A.Value
    } deriving stock (Eq, Generic, Show)

instance FromJSON VirtualAccountEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_virtualAccountEntity" }

instance ToJSON VirtualAccountEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_virtualAccountEntity" }

-- | Request to create a virtual account
data VirtualAccountCreateDto = VirtualAccountCreateDto
    { _virtualAccountCreateDtoFiat                  :: FiatCurrencyId
    , _virtualAccountCreateDtoCountry               :: CountryCode
    , _virtualAccountCreateDtoAsset                 :: AssetId
    , _virtualAccountCreateDtoCounterPartyIdentityId :: Maybe IdentityId
    , _virtualAccountCreateDtoDeveloperFeePercent   :: Maybe Double
    } deriving stock (Eq, Generic, Show)

instance FromJSON VirtualAccountCreateDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_virtualAccountCreateDto" }

instance ToJSON VirtualAccountCreateDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_virtualAccountCreateDto" }

-- | Default virtual account create request
_VirtualAccountCreateDto :: VirtualAccountCreateDto
_VirtualAccountCreateDto = VirtualAccountCreateDto
    { _virtualAccountCreateDtoFiat                  = USD
    , _virtualAccountCreateDtoCountry               = CountryCode "US"
    , _virtualAccountCreateDtoAsset                 = USDC_POLYGON
    , _virtualAccountCreateDtoCounterPartyIdentityId = Nothing
    , _virtualAccountCreateDtoDeveloperFeePercent   = Nothing
    }

-- | Virtual Accounts API (under /v1/accounts/{id}/virtual-accounts)
type API =
    "accounts" :> Capture "accountId" AccountId :> "virtual-accounts" :>
        (    -- GET /v1/accounts/{id}/virtual-accounts - List virtual accounts
             QueryParam "startingAfter" Text
          :> QueryParam "limit" Natural
          :> Get '[JSON] (PageResponse VirtualAccountEntity)

        :<|> -- POST /v1/accounts/{id}/virtual-accounts - Create virtual account
             Header' '[Required, Strict] "idempotency-key" IdempotencyKey
          :> ReqBody '[JSON] VirtualAccountCreateDto
          :> Post201 '[JSON] VirtualAccountEntity

        :<|> -- GET /v1/accounts/{id}/virtual-accounts/{virtualAccountId} - Get by ID
             Capture "virtualAccountId" VirtualAccountId
          :> Get '[JSON] VirtualAccountEntity

        :<|> -- DELETE /v1/accounts/{id}/virtual-accounts/{virtualAccountId} - Delete
             Capture "virtualAccountId" VirtualAccountId
          :> Delete '[JSON] VirtualAccountEntity
        )
