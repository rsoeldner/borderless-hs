module Borderless.V1.ComplianceChecks
    ( -- * Entity Types
      ComplianceCheckEntity(..)
    , ComplianceCheckStartedEntity(..)
    , ComplianceCoverageEntity(..)
    , ComplianceRequirementsEntity(..)
    , RequiredDocument(..)
    , RequiredExtras(..)
    , TermsOfServiceRequirement(..)
    , YellowcardCredentials(..)
      -- * Query Parameters
    , ComplianceCheckQuery(..)
    , emptyComplianceCheckQuery
      -- * Type Aliases
    , ComplianceSlug(..)
      -- * Servant API
    , API
    , CoverageAPI
    ) where

import Borderless.Prelude
import Borderless.V1.Types.Common
import Borderless.V1.Types.Enums

import qualified Data.Aeson as A

-- | Compliance check slug identifier
newtype ComplianceSlug = ComplianceSlug { unComplianceSlug :: Text }
    deriving stock (Eq, Show)
    deriving newtype (FromJSON, ToJSON, IsString, ToHttpApiData, FromHttpApiData)

-- | Coverage entry (payment method/asset/country/fiat combination)
data ComplianceCoverageEntity = ComplianceCoverageEntity
    { _complianceCoverageEntityMethod        :: Maybe PaymentMethod
    , _complianceCoverageEntityFiat          :: Maybe FiatCurrencyId
    , _complianceCoverageEntityCountry       :: Maybe CountryCode
    , _complianceCoverageEntityAsset         :: Maybe AssetId
    , _complianceCoverageEntityType_         :: Maybe OperationType
    , _complianceCoverageEntityOperationType :: Maybe OperationType
    } deriving stock (Eq, Generic, Show)

instance FromJSON ComplianceCoverageEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_complianceCoverageEntity" }

instance ToJSON ComplianceCoverageEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_complianceCoverageEntity" }

-- | Required document specification
data RequiredDocument = RequiredDocument
    { _requiredDocumentMode      :: Text  -- "oneOf", "allOf", etc.
    , _requiredDocumentDocuments :: Vector Text
    } deriving stock (Eq, Generic, Show)

instance FromJSON RequiredDocument where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_requiredDocument" }

instance ToJSON RequiredDocument where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_requiredDocument" }

-- | Terms of service requirement
data TermsOfServiceRequirement = TermsOfServiceRequirement
    { _termsOfServiceRequirementLink :: Maybe Text
    } deriving stock (Eq, Generic, Show)

instance FromJSON TermsOfServiceRequirement where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_termsOfServiceRequirement" }

instance ToJSON TermsOfServiceRequirement where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_termsOfServiceRequirement" }

-- | Yellowcard credentials status
data YellowcardCredentials = YellowcardCredentials
    { _yellowcardCredentialsProvided :: Maybe Bool
    , _yellowcardCredentialsValid    :: Maybe Bool
    } deriving stock (Eq, Generic, Show)

instance FromJSON YellowcardCredentials where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_yellowcardCredentials" }

instance ToJSON YellowcardCredentials where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_yellowcardCredentials" }

-- | Required extras for compliance
data RequiredExtras = RequiredExtras
    { _requiredExtrasAcceptTermsOfService  :: Maybe TermsOfServiceRequirement
    , _requiredExtrasYellowcardCredentials :: Maybe YellowcardCredentials
    } deriving stock (Eq, Generic, Show)

instance FromJSON RequiredExtras where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_requiredExtras" }

instance ToJSON RequiredExtras where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_requiredExtras" }

-- | Compliance check entity (returned from GET endpoints)
data ComplianceCheckEntity = ComplianceCheckEntity
    { _complianceCheckEntitySlug         :: ComplianceSlug
    , _complianceCheckEntityIncorporates :: Maybe (Vector ComplianceSlug)
    , _complianceCheckEntityCoverage     :: Vector ComplianceCoverageEntity
    , _complianceCheckEntitySchema       :: Maybe A.Value
    , _complianceCheckEntityDocuments    :: Maybe (Vector RequiredDocument)
    , _complianceCheckEntityId           :: Maybe ComplianceCheckId  -- Deprecated
    , _complianceCheckEntityKycStatus    :: Maybe ComplianceStatus   -- Deprecated
    , _complianceCheckEntityStatus       :: ComplianceStatus
    , _complianceCheckEntityMetadata     :: Maybe A.Value
    } deriving stock (Eq, Generic, Show)

instance FromJSON ComplianceCheckEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_complianceCheckEntity" }

instance ToJSON ComplianceCheckEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_complianceCheckEntity" }

-- | Compliance check started entity (returned from POST /start endpoint)
-- This has a simpler structure than ComplianceCheckEntity
data ComplianceCheckStartedEntity = ComplianceCheckStartedEntity
    { _complianceCheckStartedEntitySlug     :: ComplianceSlug
    , _complianceCheckStartedEntityStatus   :: ComplianceStatus
    , _complianceCheckStartedEntityMetadata :: Maybe A.Value
    } deriving stock (Eq, Generic, Show)

instance FromJSON ComplianceCheckStartedEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_complianceCheckStartedEntity" }

instance ToJSON ComplianceCheckStartedEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_complianceCheckStartedEntity" }

-- | Compliance requirements entity
data ComplianceRequirementsEntity = ComplianceRequirementsEntity
    { _complianceRequirementsEntitySlug              :: ComplianceSlug
    , _complianceRequirementsEntitySchema            :: Maybe A.Value
    , _complianceRequirementsEntityMissingFields     :: Vector Text
    , _complianceRequirementsEntityRequiredDocuments :: Vector RequiredDocument
    , _complianceRequirementsEntityRequiredExtras    :: Maybe RequiredExtras
    , _complianceRequirementsEntityType_             :: Maybe A.Value
    } deriving stock (Eq, Generic, Show)

instance FromJSON ComplianceRequirementsEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_complianceRequirementsEntity" }

instance ToJSON ComplianceRequirementsEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_complianceRequirementsEntity" }

-- | Query parameters for compliance check endpoints
data ComplianceCheckQuery = ComplianceCheckQuery
    { _complianceCheckQueryCountry            :: Maybe CountryCode
    , _complianceCheckQueryAsset              :: Maybe AssetId
    , _complianceCheckQueryFiat               :: Maybe FiatCurrencyId
    , _complianceCheckQueryPaymentMethod      :: Maybe PaymentMethod
    , _complianceCheckQueryType_              :: Maybe OperationType
    , _complianceCheckQueryIdentitySchemaType :: Maybe IdentityType
    } deriving stock (Eq, Show)

-- | Empty query (no filters)
emptyComplianceCheckQuery :: ComplianceCheckQuery
emptyComplianceCheckQuery = ComplianceCheckQuery
    { _complianceCheckQueryCountry            = Nothing
    , _complianceCheckQueryAsset              = Nothing
    , _complianceCheckQueryFiat               = Nothing
    , _complianceCheckQueryPaymentMethod      = Nothing
    , _complianceCheckQueryType_              = Nothing
    , _complianceCheckQueryIdentitySchemaType = Nothing
    }

-- | Coverage API (not identity-specific)
type CoverageAPI =
         -- GET /v1/compliance-checks/coverage - Get compliance coverage
         "compliance-checks"
      :> "coverage"
      :> QueryParam "country" CountryCode
      :> QueryParam "asset" AssetId
      :> QueryParam "fiat" FiatCurrencyId
      :> QueryParam "paymentMethod" PaymentMethod
      :> QueryParam "type" OperationType
      :> QueryParam "identitySchemaType" IdentityType
      :> Get '[JSON] ComplianceCheckEntity

-- | Identity Compliance Checks API
type API =
    (    -- GET /v1/identities/{id}/compliance-checks - Get compliance checks for identity
         "identities"
      :> Capture "id" IdentityId
      :> "compliance-checks"
      :> QueryParam "country" CountryCode
      :> QueryParam "asset" AssetId
      :> QueryParam "fiat" FiatCurrencyId
      :> QueryParam "paymentMethod" PaymentMethod
      :> QueryParam "type" OperationType
      :> Get '[JSON] (Vector ComplianceCheckEntity)

    :<|> -- GET /v1/identities/{id}/compliance-checks/{slug}/requirements - Get requirements
         "identities"
      :> Capture "id" IdentityId
      :> "compliance-checks"
      :> Capture "slug" ComplianceSlug
      :> "requirements"
      :> Get '[JSON] ComplianceRequirementsEntity

    :<|> -- POST /v1/identities/{id}/compliance-checks/{slug} - Start compliance check
         "identities"
      :> Capture "id" IdentityId
      :> "compliance-checks"
      :> Capture "slug" ComplianceSlug
      :> Header' '[Required, Strict] "idempotency-key" IdempotencyKey
      :> Post201 '[JSON] ComplianceCheckStartedEntity
    )
