module Borderless.V1.Transactions.Swaps
    ( -- * Request Types
      SwapCreateDto(..)
    , _SwapCreateDto
    , SwapQuoteDto(..)
    , _SwapQuoteDto
      -- * Response Types
    , SwapQuoteEntity(..)
      -- * Servant API
    , API
    ) where

import Borderless.Prelude
import Borderless.V1.Types.Common
import Borderless.V1.Types.Enums
import Borderless.V1.Transactions (TransactionEntity)

-- | Request to create a crypto swap
data SwapCreateDto = SwapCreateDto
    { _swapCreateDtoAccountId        :: AccountId
    , _swapCreateDtoIdentityId       :: IdentityId
    , _swapCreateDtoSourceAsset      :: AssetId
    , _swapCreateDtoSourceAmount     :: DecimalNumber
    , _swapCreateDtoDestinationAsset :: AssetId
    } deriving stock (Eq, Generic, Show)

instance FromJSON SwapCreateDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_swapCreateDto" }

instance ToJSON SwapCreateDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_swapCreateDto" }

-- | Default swap create request
_SwapCreateDto :: SwapCreateDto
_SwapCreateDto = SwapCreateDto
    { _swapCreateDtoAccountId        = ""
    , _swapCreateDtoIdentityId       = ""
    , _swapCreateDtoSourceAsset      = USDC_ETHEREUM
    , _swapCreateDtoSourceAmount     = "0"
    , _swapCreateDtoDestinationAsset = ETH
    }

-- | Request for a swap quote
data SwapQuoteDto = SwapQuoteDto
    { _swapQuoteDtoAccountId        :: AccountId
    , _swapQuoteDtoIdentityId       :: IdentityId
    , _swapQuoteDtoSourceAsset      :: AssetId
    , _swapQuoteDtoSourceAmount     :: DecimalNumber
    , _swapQuoteDtoDestinationAsset :: AssetId
    } deriving stock (Eq, Generic, Show)

instance FromJSON SwapQuoteDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_swapQuoteDto" }

instance ToJSON SwapQuoteDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_swapQuoteDto" }

-- | Default swap quote request
_SwapQuoteDto :: SwapQuoteDto
_SwapQuoteDto = SwapQuoteDto
    { _swapQuoteDtoAccountId        = ""
    , _swapQuoteDtoIdentityId       = ""
    , _swapQuoteDtoSourceAsset      = USDC_ETHEREUM
    , _swapQuoteDtoSourceAmount     = "0"
    , _swapQuoteDtoDestinationAsset = ETH
    }

-- | Swap quote response
data SwapQuoteEntity = SwapQuoteEntity
    { _swapQuoteEntitySourceAsset       :: AssetId
    , _swapQuoteEntitySourceAmount      :: DecimalNumber
    , _swapQuoteEntityDestinationAsset  :: AssetId
    , _swapQuoteEntityDestinationAmount :: DecimalNumber
    , _swapQuoteEntityExchangeRate      :: DecimalNumber
    , _swapQuoteEntityFeeAmount         :: DecimalNumber
    , _swapQuoteEntityFeeCurrency       :: AssetId
    , _swapQuoteEntityExpiresAt         :: UTCTime
    } deriving stock (Eq, Generic, Show)

instance FromJSON SwapQuoteEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_swapQuoteEntity" }

instance ToJSON SwapQuoteEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_swapQuoteEntity" }

-- | Swaps API
type API =
    "swaps" :>
        (    -- POST /v1/swaps - Create swap
             ReqBody '[JSON] SwapCreateDto
          :> Post201 '[JSON] TransactionEntity

        :<|> -- POST /v1/swaps/quote - Get swap quote
             "quote"
          :> ReqBody '[JSON] SwapQuoteDto
          :> Post201 '[JSON] SwapQuoteEntity

        :<|> -- POST /v1/swaps/{id}/confirm - Confirm swap
             Capture "id" TransactionId
          :> "confirm"
          :> Post201 '[JSON] TransactionEntity

        :<|> -- POST /v1/swaps/{id}/cancel - Cancel swap
             Capture "id" TransactionId
          :> "cancel"
          :> Post201 '[JSON] TransactionEntity
        )
