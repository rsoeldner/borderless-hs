module Borderless.V1.PermissionGroups
    ( -- * Entity Types
      PermissionGroupEntity(..)
      -- * Request Types
    , PermissionGroupCreateDto(..)
    , _PermissionGroupCreateDto
    , PermissionGroupUpdateDto(..)
    , _PermissionGroupUpdateDto
    , PermissionGroupUsersDto(..)
      -- * Servant API
    , API
    ) where

import Borderless.Prelude
import Borderless.V1.Types.Common
import Borderless.V1.Types.Enums
import Borderless.V1.Types.Pagination
import Borderless.V1.Users (UserEntity)

-- | Permission group entity
data PermissionGroupEntity = PermissionGroupEntity
    { _permissionGroupEntityId                   :: PermissionGroupId
    , _permissionGroupEntityName                 :: Text
    , _permissionGroupEntityDescription          :: Maybe Text
    , _permissionGroupEntityDashboardPermissions :: Vector DashboardPermission
    , _permissionGroupEntityDashboardUsers       :: Maybe (Vector UserEntity)
    } deriving stock (Eq, Generic, Show)

instance FromJSON PermissionGroupEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_permissionGroupEntity" }

instance ToJSON PermissionGroupEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_permissionGroupEntity" }

-- | Request to create a permission group
data PermissionGroupCreateDto = PermissionGroupCreateDto
    { _permissionGroupCreateDtoName                 :: Text
    , _permissionGroupCreateDtoDescription          :: Maybe Text
    , _permissionGroupCreateDtoDashboardPermissions :: Vector DashboardPermission
    } deriving stock (Eq, Generic, Show)

instance FromJSON PermissionGroupCreateDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_permissionGroupCreateDto" }

instance ToJSON PermissionGroupCreateDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_permissionGroupCreateDto" }

-- | Default permission group create request
_PermissionGroupCreateDto :: PermissionGroupCreateDto
_PermissionGroupCreateDto = PermissionGroupCreateDto
    { _permissionGroupCreateDtoName                 = ""
    , _permissionGroupCreateDtoDescription          = Nothing
    , _permissionGroupCreateDtoDashboardPermissions = mempty
    }

-- | Request to update a permission group
data PermissionGroupUpdateDto = PermissionGroupUpdateDto
    { _permissionGroupUpdateDtoName                 :: Maybe Text
    , _permissionGroupUpdateDtoDescription          :: Maybe Text
    , _permissionGroupUpdateDtoDashboardPermissions :: Maybe (Vector DashboardPermission)
    } deriving stock (Eq, Generic, Show)

instance FromJSON PermissionGroupUpdateDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_permissionGroupUpdateDto" }

instance ToJSON PermissionGroupUpdateDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_permissionGroupUpdateDto" }

-- | Default permission group update request
_PermissionGroupUpdateDto :: PermissionGroupUpdateDto
_PermissionGroupUpdateDto = PermissionGroupUpdateDto
    { _permissionGroupUpdateDtoName                 = Nothing
    , _permissionGroupUpdateDtoDescription          = Nothing
    , _permissionGroupUpdateDtoDashboardPermissions = Nothing
    }

-- | Request to add/remove users from a permission group
data PermissionGroupUsersDto = PermissionGroupUsersDto
    { _permissionGroupUsersDtoUsers :: Vector UserId
    } deriving stock (Eq, Generic, Show)

instance FromJSON PermissionGroupUsersDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_permissionGroupUsersDto" }

instance ToJSON PermissionGroupUsersDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_permissionGroupUsersDto" }

-- | Permission Groups API
type API =
    "permission-groups" :>
        (    -- GET /v1/permission-groups - List permission groups
             QueryParam "namePrefix" Text
          :> QueryParam "startingAfter" Text
          :> QueryParam "limit" Natural
          :> Get '[JSON] (PageResponse PermissionGroupEntity)

        :<|> -- POST /v1/permission-groups - Create permission group
             ReqBody '[JSON] PermissionGroupCreateDto
          :> Post201 '[JSON] PermissionGroupEntity

        :<|> -- GET /v1/permission-groups/{id} - Get by ID
             Capture "id" PermissionGroupId
          :> Get '[JSON] PermissionGroupEntity

        :<|> -- PATCH /v1/permission-groups/{id} - Update
             Capture "id" PermissionGroupId
          :> ReqBody '[JSON] PermissionGroupUpdateDto
          :> Patch '[JSON] PermissionGroupEntity

        :<|> -- DELETE /v1/permission-groups/{id} - Delete
             Capture "id" PermissionGroupId
          :> Delete '[JSON] PermissionGroupEntity

        :<|> -- POST /v1/permission-groups/{id}/users - Add users
             Capture "id" PermissionGroupId
          :> "users"
          :> ReqBody '[JSON] PermissionGroupUsersDto
          :> Post201 '[JSON] PermissionGroupEntity

        :<|> -- DELETE /v1/permission-groups/{id}/users - Remove users
             Capture "id" PermissionGroupId
          :> "users"
          :> ReqBody '[JSON] PermissionGroupUsersDto
          :> Delete '[JSON] PermissionGroupEntity
        )
