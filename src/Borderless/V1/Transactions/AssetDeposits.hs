module Borderless.V1.Transactions.AssetDeposits
    ( -- * Request Types
      AssetDepositCreateDto(..)
    , _AssetDepositCreateDto
      -- * Servant API
    , API
    ) where

import Borderless.Prelude
import Borderless.V1.Types.Common
import Borderless.V1.Types.Enums
import Borderless.V1.Transactions (TransactionEntity)

-- | Request to create an asset (crypto) deposit
data AssetDepositCreateDto = AssetDepositCreateDto
    { _assetDepositCreateDtoAccountId  :: AccountId
    , _assetDepositCreateDtoIdentityId :: IdentityId
    , _assetDepositCreateDtoAsset      :: AssetId
    , _assetDepositCreateDtoAmount     :: DecimalNumber
    , _assetDepositCreateDtoTxHash     :: Text
    } deriving stock (Eq, Generic, Show)

instance FromJSON AssetDepositCreateDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_assetDepositCreateDto" }

instance ToJSON AssetDepositCreateDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_assetDepositCreateDto" }

-- | Default asset deposit create request
_AssetDepositCreateDto :: AssetDepositCreateDto
_AssetDepositCreateDto = AssetDepositCreateDto
    { _assetDepositCreateDtoAccountId  = ""
    , _assetDepositCreateDtoIdentityId = ""
    , _assetDepositCreateDtoAsset      = USDC_ETHEREUM
    , _assetDepositCreateDtoAmount     = "0"
    , _assetDepositCreateDtoTxHash     = ""
    }

-- | Asset Deposits API
type API =
    "asset-deposits" :>
        -- POST /v1/asset-deposits - Create asset deposit
        ReqBody '[JSON] AssetDepositCreateDto
     :> Post201 '[JSON] TransactionEntity
