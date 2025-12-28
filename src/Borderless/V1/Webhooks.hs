module Borderless.V1.Webhooks
    ( -- * Entity Types
      WebhookSettingsEntity(..)
      -- * Request Types
    , WebhookSettingsCreateDto(..)
    , _WebhookSettingsCreateDto
    , WebhookSettingsUpdateDto(..)
    , _WebhookSettingsUpdateDto
      -- * Servant API
    , API
    ) where

import Borderless.Prelude
import Borderless.V1.Types.Common
import Borderless.V1.Types.Enums

-- | Webhook settings entity
data WebhookSettingsEntity = WebhookSettingsEntity
    { _webhookSettingsEntityId       :: WebhookId
    , _webhookSettingsEntityUrl      :: Text
    , _webhookSettingsEntityEvents   :: Vector WebhookEvent
    , _webhookSettingsEntityIsActive :: Bool
    } deriving stock (Eq, Generic, Show)

instance FromJSON WebhookSettingsEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_webhookSettingsEntity" }

instance ToJSON WebhookSettingsEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_webhookSettingsEntity" }

-- | Request to create webhook settings
data WebhookSettingsCreateDto = WebhookSettingsCreateDto
    { _webhookSettingsCreateDtoUrl    :: Text
    , _webhookSettingsCreateDtoEvents :: Vector WebhookEvent
    } deriving stock (Eq, Generic, Show)

instance FromJSON WebhookSettingsCreateDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_webhookSettingsCreateDto" }

instance ToJSON WebhookSettingsCreateDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_webhookSettingsCreateDto" }

-- | Default webhook settings create request
_WebhookSettingsCreateDto :: WebhookSettingsCreateDto
_WebhookSettingsCreateDto = WebhookSettingsCreateDto
    { _webhookSettingsCreateDtoUrl    = ""
    , _webhookSettingsCreateDtoEvents = mempty
    }

-- | Request to update webhook settings
data WebhookSettingsUpdateDto = WebhookSettingsUpdateDto
    { _webhookSettingsUpdateDtoUrl      :: Maybe Text
    , _webhookSettingsUpdateDtoEvents   :: Maybe (Vector WebhookEvent)
    , _webhookSettingsUpdateDtoIsActive :: Maybe Bool
    } deriving stock (Eq, Generic, Show)

instance FromJSON WebhookSettingsUpdateDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_webhookSettingsUpdateDto" }

instance ToJSON WebhookSettingsUpdateDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_webhookSettingsUpdateDto" }

-- | Default webhook settings update request
_WebhookSettingsUpdateDto :: WebhookSettingsUpdateDto
_WebhookSettingsUpdateDto = WebhookSettingsUpdateDto
    { _webhookSettingsUpdateDtoUrl      = Nothing
    , _webhookSettingsUpdateDtoEvents   = Nothing
    , _webhookSettingsUpdateDtoIsActive = Nothing
    }

-- | Webhooks API
type API =
    "notifications" :> "webhooks" :> "settings" :>
        (    -- GET /v1/notifications/webhooks/settings - List webhooks
             Get '[JSON] (Vector WebhookSettingsEntity)

        :<|> -- POST /v1/notifications/webhooks/settings - Create webhook
             ReqBody '[JSON] WebhookSettingsCreateDto
          :> Post201 '[JSON] WebhookSettingsEntity

        :<|> -- GET /v1/notifications/webhooks/settings/{id} - Get by ID
             Capture "id" WebhookId
          :> Get '[JSON] WebhookSettingsEntity

        :<|> -- PATCH /v1/notifications/webhooks/settings/{id} - Update
             Capture "id" WebhookId
          :> ReqBody '[JSON] WebhookSettingsUpdateDto
          :> Patch '[JSON] WebhookSettingsEntity

        :<|> -- DELETE /v1/notifications/webhooks/settings/{id} - Delete
             Capture "id" WebhookId
          :> Delete '[JSON] WebhookSettingsEntity

        :<|> -- GET /v1/notifications/webhooks/settings/public-key - Get public key
             "public-key"
          :> Get '[JSON] Text

        :<|> -- POST /v1/notifications/webhooks/settings/public-key - Regenerate public key
             "public-key"
          :> Post201 '[JSON] Text
        )
