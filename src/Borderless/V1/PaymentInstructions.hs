module Borderless.V1.PaymentInstructions
    ( -- * Entity Types
      PaymentInstructionEntity(..)
    , BankDetailsEntity(..)
      -- * Request Types
    , PaymentInstructionCreateDto(..)
    , _PaymentInstructionCreateDto
    , BankDetailsDto(..)
    , _BankDetailsDto
      -- * Servant API
    , API
    ) where

import Borderless.Prelude
import Borderless.V1.Types.Common
import Borderless.V1.Types.Enums
import Borderless.V1.Types.Pagination

import qualified Data.Aeson as A

-- | Bank details for payment instructions
data BankDetailsEntity = BankDetailsEntity
    { _bankDetailsEntityBankName               :: Maybe Text
    , _bankDetailsEntityBankAccountNumberLast4 :: Maybe Text
    , _bankDetailsEntityBankRoutingNumber      :: Maybe Text
    , _bankDetailsEntityBankCode               :: Maybe Text
    , _bankDetailsEntityPhone                  :: Maybe Text
    , _bankDetailsEntityTaxId                  :: Maybe Text
    , _bankDetailsEntityAccountHolderName      :: Maybe Text
    , _bankDetailsEntityBankAccountType        :: Maybe BankAccountType
    , _bankDetailsEntityAddress                :: Maybe PostalAddress
    } deriving stock (Eq, Generic, Show)

instance FromJSON BankDetailsEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_bankDetailsEntity" }

instance ToJSON BankDetailsEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_bankDetailsEntity" }

-- | Payment instruction entity
data PaymentInstructionEntity = PaymentInstructionEntity
    { _paymentInstructionEntityId            :: PaymentInstructionId
    , _paymentInstructionEntityName          :: Text
    , _paymentInstructionEntityPaymentMethod :: PaymentMethod
    , _paymentInstructionEntityCurrency      :: Text
    , _paymentInstructionEntityCountry       :: CountryCode
    , _paymentInstructionEntityDeleted       :: Bool
    , _paymentInstructionEntityDetails       :: Maybe A.Value  -- Polymorphic details
    } deriving stock (Eq, Generic, Show)

instance FromJSON PaymentInstructionEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_paymentInstructionEntity" }

instance ToJSON PaymentInstructionEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_paymentInstructionEntity" }

-- | Bank details for creating payment instructions
data BankDetailsDto = BankDetailsDto
    { _bankDetailsDtoBankName          :: Text
    , _bankDetailsDtoBankAccountNumber :: Text
    , _bankDetailsDtoBankRoutingNumber :: Maybe Text
    , _bankDetailsDtoBankCode          :: Maybe Text
    , _bankDetailsDtoAccountHolderName :: Text
    , _bankDetailsDtoBankAccountType   :: Maybe BankAccountType
    , _bankDetailsDtoAddress           :: Maybe PostalAddressDto
    } deriving stock (Eq, Generic, Show)

instance FromJSON BankDetailsDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_bankDetailsDto" }

instance ToJSON BankDetailsDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_bankDetailsDto" }

-- | Default bank details DTO
_BankDetailsDto :: BankDetailsDto
_BankDetailsDto = BankDetailsDto
    { _bankDetailsDtoBankName          = ""
    , _bankDetailsDtoBankAccountNumber = ""
    , _bankDetailsDtoBankRoutingNumber = Nothing
    , _bankDetailsDtoBankCode          = Nothing
    , _bankDetailsDtoAccountHolderName = ""
    , _bankDetailsDtoBankAccountType   = Nothing
    , _bankDetailsDtoAddress           = Nothing
    }

-- | Request to create a payment instruction
data PaymentInstructionCreateDto = PaymentInstructionCreateDto
    { _paymentInstructionCreateDtoName          :: Text
    , _paymentInstructionCreateDtoIdentityId    :: IdentityId
    , _paymentInstructionCreateDtoPaymentMethod :: PaymentMethod
    , _paymentInstructionCreateDtoCurrency      :: FiatCurrencyId
    , _paymentInstructionCreateDtoCountry       :: CountryCode
    , _paymentInstructionCreateDtoDetails       :: A.Value  -- Bank details or other payment method details
    } deriving stock (Eq, Generic, Show)

instance FromJSON PaymentInstructionCreateDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_paymentInstructionCreateDto" }

instance ToJSON PaymentInstructionCreateDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_paymentInstructionCreateDto" }

-- | Default payment instruction create request
_PaymentInstructionCreateDto :: PaymentInstructionCreateDto
_PaymentInstructionCreateDto = PaymentInstructionCreateDto
    { _paymentInstructionCreateDtoName          = ""
    , _paymentInstructionCreateDtoIdentityId    = ""
    , _paymentInstructionCreateDtoPaymentMethod = ACH
    , _paymentInstructionCreateDtoCurrency      = USD
    , _paymentInstructionCreateDtoCountry       = CountryCode "US"
    , _paymentInstructionCreateDtoDetails       = A.object []
    }

-- | Payment Instructions API
type API =
    "payment-instructions" :>
        (    -- GET /v1/payment-instructions - List payment instructions
             QueryParam "startingAfter" Text
          :> QueryParam "limit" Natural
          :> Get '[JSON] (PageResponse PaymentInstructionEntity)

        :<|> -- POST /v1/payment-instructions - Create payment instruction
             ReqBody '[JSON] PaymentInstructionCreateDto
          :> Post201 '[JSON] PaymentInstructionEntity

        :<|> -- GET /v1/payment-instructions/{id} - Get by ID
             Capture "id" PaymentInstructionId
          :> Get '[JSON] PaymentInstructionEntity

        :<|> -- DELETE /v1/payment-instructions/{id} - Delete
             Capture "id" PaymentInstructionId
          :> Delete '[JSON] PaymentInstructionEntity
        )
