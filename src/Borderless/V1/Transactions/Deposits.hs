module Borderless.V1.Transactions.Deposits
    ( -- * Request Types
      DepositCreateDto(..)
    , _DepositCreateDto
    , DepositQuoteDto(..)
    , _DepositQuoteDto
      -- * Response Types
    , DepositQuoteEntity(..)
    , DepositLimits(..)
    , TransactionLimits(..)
      -- * Servant API
    , API
    ) where

import Borderless.Prelude
import Borderless.V1.Types.Common
import Borderless.V1.Types.Enums
import Borderless.V1.Transactions (TransactionEntity)

-- | Request to create a fiat deposit
data DepositCreateDto = DepositCreateDto
    { _depositCreateDtoAccountId     :: AccountId
    , _depositCreateDtoAsset         :: AssetId
    , _depositCreateDtoFiat          :: FiatCurrencyId
    , _depositCreateDtoAmount        :: DecimalNumber
    , _depositCreateDtoCountry       :: CountryCode
    , _depositCreateDtoPaymentMethod :: Maybe PaymentMethod
    , _depositCreateDtoDeveloperFee  :: Maybe DecimalNumber
    } deriving stock (Eq, Generic, Show)

instance FromJSON DepositCreateDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_depositCreateDto" }

instance ToJSON DepositCreateDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_depositCreateDto" }

-- | Default deposit create request
_DepositCreateDto :: DepositCreateDto
_DepositCreateDto = DepositCreateDto
    { _depositCreateDtoAccountId     = ""
    , _depositCreateDtoAsset         = USDC_ETHEREUM
    , _depositCreateDtoFiat          = USD
    , _depositCreateDtoAmount        = "0"
    , _depositCreateDtoCountry       = CountryCode "US"
    , _depositCreateDtoPaymentMethod = Nothing
    , _depositCreateDtoDeveloperFee  = Nothing
    }

-- | Request for a deposit quote
data DepositQuoteDto = DepositQuoteDto
    { _depositQuoteDtoAccountId     :: AccountId
    , _depositQuoteDtoIdentityId    :: IdentityId
    , _depositQuoteDtoAsset         :: AssetId
    , _depositQuoteDtoPaymentMethod :: PaymentMethod
    , _depositQuoteDtoFiatCurrency  :: FiatCurrencyId
    , _depositQuoteDtoFiatAmount    :: DecimalNumber
    , _depositQuoteDtoCountry       :: CountryCode
    } deriving stock (Eq, Generic, Show)

instance FromJSON DepositQuoteDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_depositQuoteDto" }

instance ToJSON DepositQuoteDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_depositQuoteDto" }

-- | Default deposit quote request
_DepositQuoteDto :: DepositQuoteDto
_DepositQuoteDto = DepositQuoteDto
    { _depositQuoteDtoAccountId     = ""
    , _depositQuoteDtoIdentityId    = ""
    , _depositQuoteDtoAsset         = USDC_ETHEREUM
    , _depositQuoteDtoPaymentMethod = ACH
    , _depositQuoteDtoFiatCurrency  = USD
    , _depositQuoteDtoFiatAmount    = "0"
    , _depositQuoteDtoCountry       = CountryCode "US"
    }

-- | Transaction limits within a quote
data TransactionLimits = TransactionLimits
    { _transactionLimitsMinAmount :: DecimalNumber
    , _transactionLimitsMaxAmount :: Maybe DecimalNumber
    , _transactionLimitsFiat      :: FiatCurrencyId
    } deriving stock (Eq, Generic, Show)

instance FromJSON TransactionLimits where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_transactionLimits" }

instance ToJSON TransactionLimits where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_transactionLimits" }

-- | Deposit limits
data DepositLimits = DepositLimits
    { _depositLimitsTransaction :: TransactionLimits
    } deriving stock (Eq, Generic, Show)

instance FromJSON DepositLimits where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_depositLimits" }

instance ToJSON DepositLimits where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_depositLimits" }

-- | Deposit quote response
data DepositQuoteEntity = DepositQuoteEntity
    { _depositQuoteEntityLimits           :: DepositLimits
    , _depositQuoteEntityFiat             :: FiatCurrencyId
    , _depositQuoteEntityCountry          :: CountryCode
    , _depositQuoteEntityAsset            :: AssetId
    , _depositQuoteEntityPaymentMethod    :: PaymentMethod
    , _depositQuoteEntityFromAmount       :: DecimalNumber
    , _depositQuoteEntityToAmount         :: DecimalNumber
    , _depositQuoteEntityExchangeRate     :: DecimalNumber
    , _depositQuoteEntityTotalFee         :: DecimalNumber
    , _depositQuoteEntityOrchestrationFee :: DecimalNumber
    } deriving stock (Eq, Generic, Show)

instance FromJSON DepositQuoteEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_depositQuoteEntity" }

instance ToJSON DepositQuoteEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_depositQuoteEntity" }

-- | Deposits API
type API =
    "deposits" :>
        (    -- POST /v1/deposits - Create deposit
             Header' '[Required, Strict] "idempotency-key" IdempotencyKey
          :> ReqBody '[JSON] DepositCreateDto
          :> Post201 '[JSON] TransactionEntity

        :<|> -- GET /v1/deposits/quotes - Get deposit quote
             "quotes"
          :> QueryParam' '[Required, Strict] "country" CountryCode
          :> QueryParam' '[Required, Strict] "fiat" FiatCurrencyId
          :> QueryParam' '[Required, Strict] "asset" AssetId
          :> QueryParam "paymentMethod" PaymentMethod
          :> QueryParam "fromAmount" DecimalNumber
          :> QueryParam "toAmount" DecimalNumber
          :> Get '[JSON] DepositQuoteEntity
        )
