module Borderless.V1.IntegrationsConfig
    ( -- * Entity Types
      IntegrationConfigEntity(..)
      -- * Request Types
    , DfnsSetupDto(..)
    , UtilaSetupDto(..)
    , YellowcardSetupDto(..)
    , BitsoSetupDto(..)
      -- * Servant API
    , API
    ) where

import Borderless.Prelude
import Borderless.V1.Types.Common

-- | Integration configuration entity
data IntegrationConfigEntity = IntegrationConfigEntity
    { _integrationConfigEntityId                      :: Text
    , _integrationConfigEntityProvider                :: Text
    , _integrationConfigEntityDfnsAppId               :: Maybe Text
    , _integrationConfigEntityDfnsAuthToken           :: Maybe Text
    , _integrationConfigEntityManualDepositAddress    :: Maybe Text
    , _integrationConfigEntityManualSlackChannelName  :: Maybe Text
    , _integrationConfigEntityPfiApiCredentials       :: Maybe (Vector Text)
    } deriving stock (Eq, Generic, Show)

instance FromJSON IntegrationConfigEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_integrationConfigEntity" }

instance ToJSON IntegrationConfigEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_integrationConfigEntity" }

-- | Dfns integration setup request
data DfnsSetupDto = DfnsSetupDto
    { _dfnsSetupDtoDfnsAppId     :: Text
    , _dfnsSetupDtoDfnsAuthToken :: Text
    } deriving stock (Eq, Generic, Show)

instance FromJSON DfnsSetupDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_dfnsSetupDto" }

instance ToJSON DfnsSetupDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_dfnsSetupDto" }

-- | Utila integration setup request
data UtilaSetupDto = UtilaSetupDto
    { _utilaSetupDtoUtilaServiceAccount :: Text
    , _utilaSetupDtoUtilaPrivateRSAKey  :: Text
    } deriving stock (Eq, Generic, Show)

instance FromJSON UtilaSetupDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_utilaSetupDto" }

instance ToJSON UtilaSetupDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_utilaSetupDto" }

-- | Yellowcard integration setup request
data YellowcardSetupDto = YellowcardSetupDto
    { _yellowcardSetupDtoApiKey    :: Text
    , _yellowcardSetupDtoApiSecret :: Text
    } deriving stock (Eq, Generic, Show)

instance FromJSON YellowcardSetupDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_yellowcardSetupDto" }

instance ToJSON YellowcardSetupDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_yellowcardSetupDto" }

-- | Bitso integration setup request
data BitsoSetupDto = BitsoSetupDto
    { _bitsoSetupDtoApiKey          :: Text
    , _bitsoSetupDtoApiSecret       :: Text
    , _bitsoSetupDtoOwnerIdentityId :: IdentityId
    } deriving stock (Eq, Generic, Show)

instance FromJSON BitsoSetupDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_bitsoSetupDto" }

instance ToJSON BitsoSetupDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_bitsoSetupDto" }

-- | Integrations Config API
type API =
    "organizations" :> "integrations" :>
        (    -- PUT /v1/organizations/integrations/dfns - Setup Dfns
             "dfns"
          :> ReqBody '[JSON] DfnsSetupDto
          :> Put '[JSON] NoContent

        :<|> -- PUT /v1/organizations/integrations/utila - Setup Utila
             "utila"
          :> ReqBody '[JSON] UtilaSetupDto
          :> Put '[JSON] NoContent

        :<|> -- PUT /v1/organizations/integrations/yellowcard - Setup Yellowcard
             "yellowcard"
          :> ReqBody '[JSON] YellowcardSetupDto
          :> Put '[JSON] NoContent

        :<|> -- PUT /v1/organizations/integrations/bitso - Setup Bitso
             "bitso"
          :> ReqBody '[JSON] BitsoSetupDto
          :> Put '[JSON] IntegrationConfigEntity
        )
