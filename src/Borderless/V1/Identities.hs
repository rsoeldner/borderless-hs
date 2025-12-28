module Borderless.V1.Identities
    ( -- * Entity Types
      IdentityEntity(..)
    , PersonalDataEntity(..)
    , BusinessDataEntity(..)
    , BeneficialOwnerEntity(..)
      -- * Request Types
    , PersonalIdentityCreateDto(..)
    , _PersonalIdentityCreateDto
    , BusinessIdentityCreateDto(..)
    , _BusinessIdentityCreateDto
    , PersonalIdentityUpdateDto(..)
    , _PersonalIdentityUpdateDto
    , BusinessIdentityUpdateDto(..)
    , _BusinessIdentityUpdateDto
    , IdentityDocumentCreateDto(..)
    , _IdentityDocumentCreateDto
      -- * Servant API
    , API
    ) where

import Borderless.Prelude
import Borderless.V1.Types.Common
import Borderless.V1.Types.Enums
import Borderless.V1.Types.Pagination

-- | Beneficial owner for business identities
data BeneficialOwnerEntity = BeneficialOwnerEntity
    { _beneficialOwnerEntityPersonalIdentityId        :: IdentityId
    , _beneficialOwnerEntityHasOwnership              :: Bool
    , _beneficialOwnerEntityHasControl                :: Bool
    , _beneficialOwnerEntityIsSigner                  :: Bool
    , _beneficialOwnerEntityRelationshipEstablishedAt :: Maybe Text  -- YYYY-MM-DD
    } deriving stock (Eq, Generic, Show)

instance FromJSON BeneficialOwnerEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_beneficialOwnerEntity" }

instance ToJSON BeneficialOwnerEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_beneficialOwnerEntity" }

-- | Personal identity data
data PersonalDataEntity = PersonalDataEntity
    { _personalDataEntityFirstName      :: Text
    , _personalDataEntityLastName       :: Text
    , _personalDataEntitySecondLastName :: Maybe Text
    , _personalDataEntityMiddleName     :: Maybe Text
    , _personalDataEntityTaxId          :: Maybe Text
    , _personalDataEntityDateOfBirth    :: Text  -- YYYY-MM-DD
    , _personalDataEntityEmail          :: Maybe Text
    , _personalDataEntityPhone          :: Maybe Text
    , _personalDataEntityActivity       :: Maybe Text
    , _personalDataEntitySex            :: Maybe Sex
    , _personalDataEntityAddress        :: Maybe PostalAddress
    , _personalDataEntityDocuments      :: Vector IdentityDocument
    } deriving stock (Eq, Generic, Show)

instance FromJSON PersonalDataEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_personalDataEntity" }

instance ToJSON PersonalDataEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_personalDataEntity" }

-- | Business identity data
data BusinessDataEntity = BusinessDataEntity
    { _businessDataEntityEmail                            :: Text
    , _businessDataEntityPhone                            :: Maybe Text
    , _businessDataEntityTaxId                            :: Text
    , _businessDataEntityName                             :: Text
    , _businessDataEntityDateOfIncorporation              :: Maybe Text  -- YYYY-MM-DD
    , _businessDataEntityDescription                      :: Maybe Text
    , _businessDataEntitySourceOfFunds                    :: Maybe Text
    , _businessDataEntityWebsite                          :: Maybe Text
    , _businessDataEntityBusinessType                     :: BusinessType
    , _businessDataEntityBusinessIndustryCode             :: Maybe Text
    , _businessDataEntityIsDao                            :: Maybe Bool
    , _businessDataEntityHasMaterialIntermediaryOwnership :: Maybe Bool
    , _businessDataEntityBusinessTradeName                :: Maybe Text
    , _businessDataEntityAccountPurpose                   :: Maybe AccountPurpose
    , _businessDataEntityUltimateBeneficialOwners         :: Vector BeneficialOwnerEntity
    , _businessDataEntityAddress                          :: Maybe PostalAddress
    , _businessDataEntityDocuments                        :: Vector IdentityDocument
    } deriving stock (Eq, Generic, Show)

instance FromJSON BusinessDataEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_businessDataEntity" }

instance ToJSON BusinessDataEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_businessDataEntity" }

-- | Identity entity (can be Personal or Business)
data IdentityEntity = IdentityEntity
    { _identityEntityId                  :: IdentityId
    , _identityEntityType_               :: IdentityType
    , _identityEntityStatus              :: IdentityStatus
    , _identityEntitySuspensionReason    :: Maybe Text
    , _identityEntityDeleted             :: Bool
    , _identityEntityHasComplianceChecks :: Bool
    , _identityEntityPersonalData        :: Maybe PersonalDataEntity
    , _identityEntityBusinessData        :: Maybe BusinessDataEntity
    } deriving stock (Eq, Generic, Show)

instance FromJSON IdentityEntity where
    parseJSON = withObject "IdentityEntity" $ \v -> do
        _identityEntityId <- v .: "id"
        _identityEntityType_ <- v .: "type"
        _identityEntityStatus <- v .: "status"
        _identityEntitySuspensionReason <- v .:? "suspensionReason"
        _identityEntityDeleted <- v .: "deleted"
        _identityEntityHasComplianceChecks <- v .: "hasComplianceChecks"

        (_identityEntityPersonalData, _identityEntityBusinessData) <- case _identityEntityType_ of
            Personal -> do
                pd <- v .:? "data"
                pure (pd, Nothing)
            Business -> do
                bd <- v .:? "data"
                pure (Nothing, bd)

        pure IdentityEntity{..}

instance ToJSON IdentityEntity where
    toJSON IdentityEntity{..} = object $
        [ "id"                  .= _identityEntityId
        , "type"                .= _identityEntityType_
        , "status"              .= _identityEntityStatus
        , "deleted"             .= _identityEntityDeleted
        , "hasComplianceChecks" .= _identityEntityHasComplianceChecks
        ] ++
        [ "suspensionReason" .= sr | Just sr <- [_identityEntitySuspensionReason] ] ++
        case (_identityEntityPersonalData, _identityEntityBusinessData) of
            (Just pd, _) -> [ "data" .= pd ]
            (_, Just bd) -> [ "data" .= bd ]
            _            -> []

-- | Request to create a personal identity
data PersonalIdentityCreateDto = PersonalIdentityCreateDto
    { _personalIdentityCreateDtoFirstName      :: Text
    , _personalIdentityCreateDtoLastName       :: Text
    , _personalIdentityCreateDtoSecondLastName :: Maybe Text
    , _personalIdentityCreateDtoMiddleName     :: Maybe Text
    , _personalIdentityCreateDtoTaxId          :: Maybe Text
    , _personalIdentityCreateDtoDateOfBirth    :: Text  -- YYYY-MM-DD
    , _personalIdentityCreateDtoEmail          :: Maybe Text
    , _personalIdentityCreateDtoPhone          :: Maybe Text
    , _personalIdentityCreateDtoActivity       :: Maybe Text
    , _personalIdentityCreateDtoSex            :: Maybe Sex
    , _personalIdentityCreateDtoAddress        :: Maybe PostalAddressDto
    } deriving stock (Eq, Generic, Show)

instance FromJSON PersonalIdentityCreateDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_personalIdentityCreateDto" }

instance ToJSON PersonalIdentityCreateDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_personalIdentityCreateDto" }

-- | Default personal identity create request
_PersonalIdentityCreateDto :: PersonalIdentityCreateDto
_PersonalIdentityCreateDto = PersonalIdentityCreateDto
    { _personalIdentityCreateDtoFirstName      = ""
    , _personalIdentityCreateDtoLastName       = ""
    , _personalIdentityCreateDtoSecondLastName = Nothing
    , _personalIdentityCreateDtoMiddleName     = Nothing
    , _personalIdentityCreateDtoTaxId          = Nothing
    , _personalIdentityCreateDtoDateOfBirth    = ""
    , _personalIdentityCreateDtoEmail          = Nothing
    , _personalIdentityCreateDtoPhone          = Nothing
    , _personalIdentityCreateDtoActivity       = Nothing
    , _personalIdentityCreateDtoSex            = Nothing
    , _personalIdentityCreateDtoAddress        = Nothing
    }

-- | Beneficial owner DTO for creating business identities
data BeneficialOwnerDto = BeneficialOwnerDto
    { _beneficialOwnerDtoPersonalIdentityId        :: IdentityId
    , _beneficialOwnerDtoHasOwnership              :: Bool
    , _beneficialOwnerDtoHasControl                :: Bool
    , _beneficialOwnerDtoIsSigner                  :: Bool
    , _beneficialOwnerDtoRelationshipEstablishedAt :: Maybe Text
    } deriving stock (Eq, Generic, Show)

instance FromJSON BeneficialOwnerDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_beneficialOwnerDto" }

instance ToJSON BeneficialOwnerDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_beneficialOwnerDto" }

-- | Request to create a business identity
data BusinessIdentityCreateDto = BusinessIdentityCreateDto
    { _businessIdentityCreateDtoEmail                            :: Text
    , _businessIdentityCreateDtoPhone                            :: Maybe Text
    , _businessIdentityCreateDtoTaxId                            :: Text
    , _businessIdentityCreateDtoName                             :: Text
    , _businessIdentityCreateDtoDateOfIncorporation              :: Maybe Text
    , _businessIdentityCreateDtoDescription                      :: Maybe Text
    , _businessIdentityCreateDtoSourceOfFunds                    :: Maybe Text
    , _businessIdentityCreateDtoWebsite                          :: Maybe Text
    , _businessIdentityCreateDtoBusinessType                     :: BusinessType
    , _businessIdentityCreateDtoBusinessIndustryCode             :: Maybe Text
    , _businessIdentityCreateDtoIsDao                            :: Maybe Bool
    , _businessIdentityCreateDtoHasMaterialIntermediaryOwnership :: Maybe Bool
    , _businessIdentityCreateDtoBusinessTradeName                :: Maybe Text
    , _businessIdentityCreateDtoAccountPurpose                   :: Maybe AccountPurpose
    , _businessIdentityCreateDtoUltimateBeneficialOwners         :: Maybe (Vector BeneficialOwnerDto)
    , _businessIdentityCreateDtoAddress                          :: Maybe PostalAddressDto
    } deriving stock (Eq, Generic, Show)

instance FromJSON BusinessIdentityCreateDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_businessIdentityCreateDto" }

instance ToJSON BusinessIdentityCreateDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_businessIdentityCreateDto" }

-- | Default business identity create request
_BusinessIdentityCreateDto :: BusinessIdentityCreateDto
_BusinessIdentityCreateDto = BusinessIdentityCreateDto
    { _businessIdentityCreateDtoEmail                            = ""
    , _businessIdentityCreateDtoPhone                            = Nothing
    , _businessIdentityCreateDtoTaxId                            = ""
    , _businessIdentityCreateDtoName                             = ""
    , _businessIdentityCreateDtoDateOfIncorporation              = Nothing
    , _businessIdentityCreateDtoDescription                      = Nothing
    , _businessIdentityCreateDtoSourceOfFunds                    = Nothing
    , _businessIdentityCreateDtoWebsite                          = Nothing
    , _businessIdentityCreateDtoBusinessType                     = Corporation
    , _businessIdentityCreateDtoBusinessIndustryCode             = Nothing
    , _businessIdentityCreateDtoIsDao                            = Nothing
    , _businessIdentityCreateDtoHasMaterialIntermediaryOwnership = Nothing
    , _businessIdentityCreateDtoBusinessTradeName                = Nothing
    , _businessIdentityCreateDtoAccountPurpose                   = Nothing
    , _businessIdentityCreateDtoUltimateBeneficialOwners         = Nothing
    , _businessIdentityCreateDtoAddress                          = Nothing
    }

-- | Request to update a personal identity
data PersonalIdentityUpdateDto = PersonalIdentityUpdateDto
    { _personalIdentityUpdateDtoFirstName      :: Maybe Text
    , _personalIdentityUpdateDtoLastName       :: Maybe Text
    , _personalIdentityUpdateDtoSecondLastName :: Maybe Text
    , _personalIdentityUpdateDtoMiddleName     :: Maybe Text
    , _personalIdentityUpdateDtoTaxId          :: Maybe Text
    , _personalIdentityUpdateDtoDateOfBirth    :: Maybe Text
    , _personalIdentityUpdateDtoEmail          :: Maybe Text
    , _personalIdentityUpdateDtoPhone          :: Maybe Text
    , _personalIdentityUpdateDtoActivity       :: Maybe Text
    , _personalIdentityUpdateDtoSex            :: Maybe Sex
    , _personalIdentityUpdateDtoAddress        :: Maybe PostalAddressDto
    } deriving stock (Eq, Generic, Show)

instance FromJSON PersonalIdentityUpdateDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_personalIdentityUpdateDto" }

instance ToJSON PersonalIdentityUpdateDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_personalIdentityUpdateDto" }

-- | Default personal identity update request
_PersonalIdentityUpdateDto :: PersonalIdentityUpdateDto
_PersonalIdentityUpdateDto = PersonalIdentityUpdateDto
    { _personalIdentityUpdateDtoFirstName      = Nothing
    , _personalIdentityUpdateDtoLastName       = Nothing
    , _personalIdentityUpdateDtoSecondLastName = Nothing
    , _personalIdentityUpdateDtoMiddleName     = Nothing
    , _personalIdentityUpdateDtoTaxId          = Nothing
    , _personalIdentityUpdateDtoDateOfBirth    = Nothing
    , _personalIdentityUpdateDtoEmail          = Nothing
    , _personalIdentityUpdateDtoPhone          = Nothing
    , _personalIdentityUpdateDtoActivity       = Nothing
    , _personalIdentityUpdateDtoSex            = Nothing
    , _personalIdentityUpdateDtoAddress        = Nothing
    }

-- | Request to update a business identity
data BusinessIdentityUpdateDto = BusinessIdentityUpdateDto
    { _businessIdentityUpdateDtoEmail                            :: Maybe Text
    , _businessIdentityUpdateDtoPhone                            :: Maybe Text
    , _businessIdentityUpdateDtoTaxId                            :: Maybe Text
    , _businessIdentityUpdateDtoName                             :: Maybe Text
    , _businessIdentityUpdateDtoDateOfIncorporation              :: Maybe Text
    , _businessIdentityUpdateDtoDescription                      :: Maybe Text
    , _businessIdentityUpdateDtoSourceOfFunds                    :: Maybe Text
    , _businessIdentityUpdateDtoWebsite                          :: Maybe Text
    , _businessIdentityUpdateDtoBusinessType                     :: Maybe BusinessType
    , _businessIdentityUpdateDtoBusinessIndustryCode             :: Maybe Text
    , _businessIdentityUpdateDtoIsDao                            :: Maybe Bool
    , _businessIdentityUpdateDtoHasMaterialIntermediaryOwnership :: Maybe Bool
    , _businessIdentityUpdateDtoBusinessTradeName                :: Maybe Text
    , _businessIdentityUpdateDtoAccountPurpose                   :: Maybe AccountPurpose
    , _businessIdentityUpdateDtoUltimateBeneficialOwners         :: Maybe (Vector BeneficialOwnerDto)
    , _businessIdentityUpdateDtoAddress                          :: Maybe PostalAddressDto
    } deriving stock (Eq, Generic, Show)

instance FromJSON BusinessIdentityUpdateDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_businessIdentityUpdateDto" }

instance ToJSON BusinessIdentityUpdateDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_businessIdentityUpdateDto" }

-- | Default business identity update request
_BusinessIdentityUpdateDto :: BusinessIdentityUpdateDto
_BusinessIdentityUpdateDto = BusinessIdentityUpdateDto
    { _businessIdentityUpdateDtoEmail                            = Nothing
    , _businessIdentityUpdateDtoPhone                            = Nothing
    , _businessIdentityUpdateDtoTaxId                            = Nothing
    , _businessIdentityUpdateDtoName                             = Nothing
    , _businessIdentityUpdateDtoDateOfIncorporation              = Nothing
    , _businessIdentityUpdateDtoDescription                      = Nothing
    , _businessIdentityUpdateDtoSourceOfFunds                    = Nothing
    , _businessIdentityUpdateDtoWebsite                          = Nothing
    , _businessIdentityUpdateDtoBusinessType                     = Nothing
    , _businessIdentityUpdateDtoBusinessIndustryCode             = Nothing
    , _businessIdentityUpdateDtoIsDao                            = Nothing
    , _businessIdentityUpdateDtoHasMaterialIntermediaryOwnership = Nothing
    , _businessIdentityUpdateDtoBusinessTradeName                = Nothing
    , _businessIdentityUpdateDtoAccountPurpose                   = Nothing
    , _businessIdentityUpdateDtoUltimateBeneficialOwners         = Nothing
    , _businessIdentityUpdateDtoAddress                          = Nothing
    }

-- | Request to upload a document
data IdentityDocumentCreateDto = IdentityDocumentCreateDto
    { _identityDocumentCreateDtoIssuingCountry :: CountryCode
    , _identityDocumentCreateDtoType_          :: IdentityDocumentType
    , _identityDocumentCreateDtoIdNumber       :: Maybe Text
    , _identityDocumentCreateDtoIssuedDate     :: Text  -- YYYY-MM-DD
    , _identityDocumentCreateDtoExpiryDate     :: Maybe Text  -- YYYY-MM-DD
    , _identityDocumentCreateDtoImageFront     :: Maybe Text  -- Base64 encoded
    , _identityDocumentCreateDtoImageBack      :: Maybe Text  -- Base64 encoded
    } deriving stock (Eq, Generic, Show)

instance FromJSON IdentityDocumentCreateDto where
    parseJSON = withObject "IdentityDocumentCreateDto" $ \v -> IdentityDocumentCreateDto
        <$> v .:  "issuingCountry"
        <*> v .:  "type"
        <*> v .:? "idNumber"
        <*> v .:  "issuedDate"
        <*> v .:? "expiryDate"
        <*> v .:? "imageFront"
        <*> v .:? "imageBack"

instance ToJSON IdentityDocumentCreateDto where
    toJSON IdentityDocumentCreateDto{..} = object
        [ "issuingCountry" .= _identityDocumentCreateDtoIssuingCountry
        , "type"           .= _identityDocumentCreateDtoType_
        , "idNumber"       .= _identityDocumentCreateDtoIdNumber
        , "issuedDate"     .= _identityDocumentCreateDtoIssuedDate
        , "expiryDate"     .= _identityDocumentCreateDtoExpiryDate
        , "imageFront"     .= _identityDocumentCreateDtoImageFront
        , "imageBack"      .= _identityDocumentCreateDtoImageBack
        ]

-- | Default document create request
_IdentityDocumentCreateDto :: IdentityDocumentCreateDto
_IdentityDocumentCreateDto = IdentityDocumentCreateDto
    { _identityDocumentCreateDtoIssuingCountry = "US"
    , _identityDocumentCreateDtoType_          = Passport
    , _identityDocumentCreateDtoIdNumber       = Nothing
    , _identityDocumentCreateDtoIssuedDate     = "2020-01-01"
    , _identityDocumentCreateDtoExpiryDate     = Nothing
    , _identityDocumentCreateDtoImageFront     = Nothing
    , _identityDocumentCreateDtoImageBack      = Nothing
    }

-- | Identities API
type API =
    "identities" :>
        (    -- GET /v1/identities - List identities
             QueryParam "startingAfter" Text
          :> QueryParam "limit" Natural
          :> Get '[JSON] (PageResponse IdentityEntity)

        :<|> -- GET /v1/identities/{id} - Get by ID
             Capture "id" IdentityId
          :> Get '[JSON] IdentityEntity

        :<|> -- POST /v1/identities/personal - Create personal identity
             "personal"
          :> ReqBody '[JSON] PersonalIdentityCreateDto
          :> Post201 '[JSON] IdentityEntity

        :<|> -- POST /v1/identities/business - Create business identity
             "business"
          :> ReqBody '[JSON] BusinessIdentityCreateDto
          :> Post201 '[JSON] IdentityEntity

        :<|> -- PATCH /v1/identities/personal/{id} - Update personal identity
             "personal"
          :> Capture "id" IdentityId
          :> ReqBody '[JSON] PersonalIdentityUpdateDto
          :> Patch '[JSON] IdentityEntity

        :<|> -- PATCH /v1/identities/business/{id} - Update business identity
             "business"
          :> Capture "id" IdentityId
          :> ReqBody '[JSON] BusinessIdentityUpdateDto
          :> Patch '[JSON] IdentityEntity

        :<|> -- DELETE /v1/identities/{id} - Delete identity
             Capture "id" IdentityId
          :> Delete '[JSON] IdentityEntity

        :<|> -- PUT /v1/identities/{id}/documents - Upload document
             Capture "id" IdentityId
          :> "documents"
          :> ReqBody '[JSON] IdentityDocumentCreateDto
          :> Put '[JSON] IdentityEntity
        )
