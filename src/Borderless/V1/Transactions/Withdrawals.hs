module Borderless.V1.Transactions.Withdrawals
    ( -- * Request Types
      WithdrawalCreateDto(..)
    , _WithdrawalCreateDto
    , WithdrawalQuoteDto(..)
    , _WithdrawalQuoteDto
      -- * Response Types
    , WithdrawalQuoteEntity(..)
      -- * Servant API
    , API
    ) where

import Borderless.Prelude
import Borderless.V1.Types.Common
import Borderless.V1.Types.Enums
import Borderless.V1.Transactions (TransactionEntity)

-- | Request to create a fiat withdrawal
data WithdrawalCreateDto = WithdrawalCreateDto
    { _withdrawalCreateDtoAccountId            :: AccountId
    , _withdrawalCreateDtoIdentityId           :: IdentityId
    , _withdrawalCreateDtoAsset                :: AssetId
    , _withdrawalCreateDtoAssetAmount          :: DecimalNumber
    , _withdrawalCreateDtoPaymentInstructionId :: PaymentInstructionId
    } deriving stock (Eq, Generic, Show)

instance FromJSON WithdrawalCreateDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_withdrawalCreateDto" }

instance ToJSON WithdrawalCreateDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_withdrawalCreateDto" }

-- | Default withdrawal create request
_WithdrawalCreateDto :: WithdrawalCreateDto
_WithdrawalCreateDto = WithdrawalCreateDto
    { _withdrawalCreateDtoAccountId            = ""
    , _withdrawalCreateDtoIdentityId           = ""
    , _withdrawalCreateDtoAsset                = USDC_ETHEREUM
    , _withdrawalCreateDtoAssetAmount          = "0"
    , _withdrawalCreateDtoPaymentInstructionId = ""
    }

-- | Request for a withdrawal quote
data WithdrawalQuoteDto = WithdrawalQuoteDto
    { _withdrawalQuoteDtoAccountId            :: AccountId
    , _withdrawalQuoteDtoIdentityId           :: IdentityId
    , _withdrawalQuoteDtoAsset                :: AssetId
    , _withdrawalQuoteDtoAssetAmount          :: DecimalNumber
    , _withdrawalQuoteDtoPaymentInstructionId :: PaymentInstructionId
    } deriving stock (Eq, Generic, Show)

instance FromJSON WithdrawalQuoteDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_withdrawalQuoteDto" }

instance ToJSON WithdrawalQuoteDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_withdrawalQuoteDto" }

-- | Default withdrawal quote request
_WithdrawalQuoteDto :: WithdrawalQuoteDto
_WithdrawalQuoteDto = WithdrawalQuoteDto
    { _withdrawalQuoteDtoAccountId            = ""
    , _withdrawalQuoteDtoIdentityId           = ""
    , _withdrawalQuoteDtoAsset                = USDC_ETHEREUM
    , _withdrawalQuoteDtoAssetAmount          = "0"
    , _withdrawalQuoteDtoPaymentInstructionId = ""
    }

-- | Withdrawal quote response
data WithdrawalQuoteEntity = WithdrawalQuoteEntity
    { _withdrawalQuoteEntityAsset           :: AssetId
    , _withdrawalQuoteEntityAssetAmount     :: DecimalNumber
    , _withdrawalQuoteEntityFiatCurrency    :: FiatCurrencyId
    , _withdrawalQuoteEntityFiatAmount      :: DecimalNumber
    , _withdrawalQuoteEntityAssetToFiatRate :: DecimalNumber
    , _withdrawalQuoteEntityFeeAmount       :: DecimalNumber
    , _withdrawalQuoteEntityFeeCurrency     :: AssetId
    , _withdrawalQuoteEntityExpiresAt       :: UTCTime
    } deriving stock (Eq, Generic, Show)

instance FromJSON WithdrawalQuoteEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_withdrawalQuoteEntity" }

instance ToJSON WithdrawalQuoteEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_withdrawalQuoteEntity" }

-- | Withdrawals API
type API =
    "withdrawals" :>
        (    -- POST /v1/withdrawals - Create withdrawal
             ReqBody '[JSON] WithdrawalCreateDto
          :> Post201 '[JSON] TransactionEntity

        :<|> -- POST /v1/withdrawals/quote - Get withdrawal quote
             "quote"
          :> ReqBody '[JSON] WithdrawalQuoteDto
          :> Post201 '[JSON] WithdrawalQuoteEntity

        :<|> -- POST /v1/withdrawals/{id}/confirm - Confirm withdrawal
             Capture "id" TransactionId
          :> "confirm"
          :> Post201 '[JSON] TransactionEntity

        :<|> -- POST /v1/withdrawals/{id}/cancel - Cancel withdrawal
             Capture "id" TransactionId
          :> "cancel"
          :> Post201 '[JSON] TransactionEntity
        )
