module Borderless.V1.Types.Common
    ( -- * ID Types
      UserId(..)
    , IdentityId(..)
    , AccountId(..)
    , VirtualAccountId(..)
    , TransactionId(..)
    , PaymentInstructionId(..)
    , PermissionGroupId(..)
    , ComplianceCheckId(..)
    , WebhookId(..)
    , DocumentId(..)
      -- * Common Entities
    , CountryCode(..)
    , PostalAddress(..)
    , PostalAddressDto(..)
    , IdentityDocument(..)
    , DecimalNumber(..)
    ) where

import Borderless.Prelude
import Borderless.V1.Types.Enums

-- | User ID
newtype UserId = UserId { unUserId :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Identity ID
newtype IdentityId = IdentityId { unIdentityId :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Account ID
newtype AccountId = AccountId { unAccountId :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Virtual Account ID
newtype VirtualAccountId = VirtualAccountId { unVirtualAccountId :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Transaction ID
newtype TransactionId = TransactionId { unTransactionId :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Payment Instruction ID
newtype PaymentInstructionId = PaymentInstructionId { unPaymentInstructionId :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Permission Group ID
newtype PermissionGroupId = PermissionGroupId { unPermissionGroupId :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Compliance Check ID
newtype ComplianceCheckId = ComplianceCheckId { unComplianceCheckId :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Webhook ID
newtype WebhookId = WebhookId { unWebhookId :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Document ID
newtype DocumentId = DocumentId { unDocumentId :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | ISO 3166-1 alpha-2 country code
newtype CountryCode = CountryCode { unCountryCode :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Decimal number (represented as Text for precision)
newtype DecimalNumber = DecimalNumber { unDecimalNumber :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Postal address entity (response)
data PostalAddress = PostalAddress
    { _postalAddressId         :: Text
    , _postalAddressStreet1    :: Text
    , _postalAddressStreet2    :: Maybe Text
    , _postalAddressCity       :: Text
    , _postalAddressState      :: Maybe Text
    , _postalAddressCountry    :: CountryCode
    , _postalAddressPostalCode :: Text
    } deriving stock (Eq, Generic, Show)

instance FromJSON PostalAddress where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_postalAddress" }

instance ToJSON PostalAddress where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_postalAddress" }

-- | Postal address DTO (request)
data PostalAddressDto = PostalAddressDto
    { _postalAddressDtoStreet1    :: Text
    , _postalAddressDtoStreet2    :: Maybe Text
    , _postalAddressDtoCity       :: Text
    , _postalAddressDtoState      :: Maybe Text
    , _postalAddressDtoCountry    :: CountryCode
    , _postalAddressDtoPostalCode :: Text
    } deriving stock (Eq, Generic, Show)

instance FromJSON PostalAddressDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_postalAddressDto" }

instance ToJSON PostalAddressDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_postalAddressDto" }

-- | Default postal address DTO
_PostalAddressDto :: PostalAddressDto
_PostalAddressDto = PostalAddressDto
    { _postalAddressDtoStreet1    = ""
    , _postalAddressDtoStreet2    = Nothing
    , _postalAddressDtoCity       = ""
    , _postalAddressDtoState      = Nothing
    , _postalAddressDtoCountry    = CountryCode "US"
    , _postalAddressDtoPostalCode = ""
    }

-- | Identity document
data IdentityDocument = IdentityDocument
    { _identityDocumentId             :: Text
    , _identityDocumentIssuingCountry :: CountryCode
    , _identityDocumentType_          :: IdentityDocumentType
    , _identityDocumentIdNumber       :: Maybe Text
    , _identityDocumentIssuedDate     :: Maybe Text  -- YYYY-MM-DD
    , _identityDocumentExpiryDate     :: Maybe Text  -- YYYY-MM-DD
    } deriving stock (Eq, Generic, Show)

instance FromJSON IdentityDocument where
    parseJSON = withObject "IdentityDocument" $ \v -> IdentityDocument
        <$> v .:  "id"
        <*> v .:  "issuingCountry"
        <*> v .:  "type"
        <*> v .:? "idNumber"
        <*> v .:? "issuedDate"
        <*> v .:? "expiryDate"

instance ToJSON IdentityDocument where
    toJSON IdentityDocument{..} = object
        [ "id"             .= _identityDocumentId
        , "issuingCountry" .= _identityDocumentIssuingCountry
        , "type"           .= _identityDocumentType_
        , "idNumber"       .= _identityDocumentIdNumber
        , "issuedDate"     .= _identityDocumentIssuedDate
        , "expiryDate"     .= _identityDocumentExpiryDate
        ]
