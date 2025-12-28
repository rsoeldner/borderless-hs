module Borderless.V1.Users
    ( -- * Entity Types
      UserEntity(..)
    , UserInviteEntity(..)
      -- * Request Types
    , UserInviteDto(..)
    , _UserInviteDto
    , UserUpdateDto(..)
    , _UserUpdateDto
    , UserRoleUpdateDto(..)
      -- * Servant API
    , API
    ) where

import Borderless.Prelude
import Borderless.V1.Types.Common
import Borderless.V1.Types.Enums
import Borderless.V1.Types.Pagination

-- | User entity returned from API
data UserEntity = UserEntity
    { _userEntityId                   :: UserId
    , _userEntityEmail                :: Text
    , _userEntityFirstName            :: Maybe Text
    , _userEntityLastName             :: Maybe Text
    , _userEntityBusinessTitle        :: Maybe Text
    , _userEntityRole                 :: DashboardRole
    , _userEntityDeleted              :: Bool
    , _userEntityDeletedAt            :: Maybe UTCTime
    , _userEntityMfaEnabled           :: Bool
    , _userEntityDashboardPermissions :: Maybe (Vector DashboardPermission)
    } deriving stock (Eq, Generic, Show)

instance FromJSON UserEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_userEntity" }

instance ToJSON UserEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_userEntity" }

-- | User invite response
data UserInviteEntity = UserInviteEntity
    { _userInviteEntityUser          :: UserEntity
    , _userInviteEntityUserInviteUrl :: Text
    } deriving stock (Eq, Generic, Show)

instance FromJSON UserInviteEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_userInviteEntity" }

instance ToJSON UserInviteEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_userInviteEntity" }

-- | Request to invite a new user
data UserInviteDto = UserInviteDto
    { _userInviteDtoEmail         :: Text
    , _userInviteDtoFirstName     :: Maybe Text
    , _userInviteDtoLastName      :: Maybe Text
    , _userInviteDtoBusinessTitle :: Maybe Text
    , _userInviteDtoRole          :: Maybe DashboardRole
    } deriving stock (Eq, Generic, Show)

instance FromJSON UserInviteDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_userInviteDto" }

instance ToJSON UserInviteDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_userInviteDto" }

-- | Default user invite request
_UserInviteDto :: UserInviteDto
_UserInviteDto = UserInviteDto
    { _userInviteDtoEmail         = ""
    , _userInviteDtoFirstName     = Nothing
    , _userInviteDtoLastName      = Nothing
    , _userInviteDtoBusinessTitle = Nothing
    , _userInviteDtoRole          = Nothing
    }

-- | Request to update a user
data UserUpdateDto = UserUpdateDto
    { _userUpdateDtoFirstName     :: Maybe Text
    , _userUpdateDtoLastName      :: Maybe Text
    , _userUpdateDtoBusinessTitle :: Maybe Text
    } deriving stock (Eq, Generic, Show)

instance FromJSON UserUpdateDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_userUpdateDto" }

instance ToJSON UserUpdateDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_userUpdateDto" }

-- | Default user update request
_UserUpdateDto :: UserUpdateDto
_UserUpdateDto = UserUpdateDto
    { _userUpdateDtoFirstName     = Nothing
    , _userUpdateDtoLastName      = Nothing
    , _userUpdateDtoBusinessTitle = Nothing
    }

-- | Request to update user role
data UserRoleUpdateDto = UserRoleUpdateDto
    { _userRoleUpdateDtoRole :: Maybe DashboardRole
    } deriving stock (Eq, Generic, Show)

instance FromJSON UserRoleUpdateDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_userRoleUpdateDto" }

instance ToJSON UserRoleUpdateDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_userRoleUpdateDto" }

-- | Users API
type API =
    "users" :>
        (    -- GET /v1/users - List users
             QueryParam "startingAfter" Text
          :> QueryParam "limit" Natural
          :> Get '[JSON] (PageResponse UserEntity)

        :<|> -- GET /v1/users/{userId} - Get user by ID
             Capture "userId" UserId
          :> Get '[JSON] UserEntity

        :<|> -- POST /v1/users/invites - Invite user
             "invites"
          :> ReqBody '[JSON] UserInviteDto
          :> Post201 '[JSON] UserInviteEntity

        :<|> -- DELETE /v1/users/{id} - Delete user
             Capture "id" UserId
          :> Delete '[JSON] UserEntity

        :<|> -- PATCH /v1/users/{id} - Update user
             Capture "id" UserId
          :> ReqBody '[JSON] UserUpdateDto
          :> Patch '[JSON] UserEntity

        :<|> -- PATCH /v1/users/{id}/roles - Update user role
             Capture "id" UserId
          :> "roles"
          :> ReqBody '[JSON] UserRoleUpdateDto
          :> Patch '[JSON] UserEntity

        :<|> -- PUT /v1/users/{id}/mfa - Enable MFA
             Capture "id" UserId
          :> "mfa"
          :> Put '[JSON] UserEntity
        )
