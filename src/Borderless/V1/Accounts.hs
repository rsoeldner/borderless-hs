module Borderless.V1.Accounts
    ( -- * Entity Types
      AccountEntity(..)
    , AccountAssetAddressEntity(..)
    , AccountBalanceEntity(..)
      -- * Request Types
    , AccountCreateDto(..)
    , _AccountCreateDto
    , AccountAssetDto(..)
    , SubaccountCreateDto(..)
    , _SubaccountCreateDto
      -- * Servant API
    , API
    ) where

import Borderless.Prelude
import Borderless.V1.Types.Common
import Borderless.V1.Types.Enums
import Borderless.V1.Types.Pagination

-- | Asset address associated with an account
data AccountAssetAddressEntity = AccountAssetAddressEntity
    { _accountAssetAddressEntityId      :: Text
    , _accountAssetAddressEntityAsset   :: AssetId
    , _accountAssetAddressEntityAddress :: Text
    } deriving stock (Eq, Generic, Show)

instance FromJSON AccountAssetAddressEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_accountAssetAddressEntity" }

instance ToJSON AccountAssetAddressEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_accountAssetAddressEntity" }

-- | Account entity
data AccountEntity = AccountEntity
    { _accountEntityId         :: AccountId
    , _accountEntityName       :: Text
    , _accountEntityAddresses  :: Vector AccountAssetAddressEntity
    , _accountEntityIdentityId :: IdentityId
    , _accountEntityCreatedAt  :: UTCTime
    } deriving stock (Eq, Generic, Show)

instance FromJSON AccountEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_accountEntity" }

instance ToJSON AccountEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_accountEntity" }

-- | Account balance for an asset
data AccountBalanceEntity = AccountBalanceEntity
    { _accountBalanceEntityAsset :: AssetId
    , _accountBalanceEntityTotal :: Text  -- Decimal string
    } deriving stock (Eq, Generic, Show)

instance FromJSON AccountBalanceEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_accountBalanceEntity" }

instance ToJSON AccountBalanceEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_accountBalanceEntity" }

-- | Asset configuration for account creation
data AccountAssetDto = AccountAssetDto
    { _accountAssetDtoAsset       :: AssetId
    , _accountAssetDtoAddress     :: Maybe Text       -- For Standalone
    , _accountAssetDtoDfnsWalletId :: Maybe Text      -- For Dfns
    } deriving stock (Eq, Generic, Show)

instance FromJSON AccountAssetDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_accountAssetDto" }

instance ToJSON AccountAssetDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_accountAssetDto" }

-- | Request to create an account
data AccountCreateDto = AccountCreateDto
    { _accountCreateDtoName         :: Text
    , _accountCreateDtoIdentityId   :: IdentityId
    , _accountCreateDtoWeb3Provider :: Maybe Web3Provider  -- Optional, omit for default
    , _accountCreateDtoAssets       :: Maybe (Vector AccountAssetDto)
    , _accountCreateDtoPassphrase   :: Maybe Text       -- For Fireblocks
    , _accountCreateDtoPassphraseId :: Maybe Text       -- For Fireblocks (UUID)
    , _accountCreateDtoVaultId      :: Maybe Text       -- For Utila
    , _accountCreateDtoWalletId     :: Maybe Text       -- For Utila
    } deriving stock (Eq, Generic, Show)

instance FromJSON AccountCreateDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_accountCreateDto" }

instance ToJSON AccountCreateDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_accountCreateDto" }

-- | Default account create request
_AccountCreateDto :: AccountCreateDto
_AccountCreateDto = AccountCreateDto
    { _accountCreateDtoName         = ""
    , _accountCreateDtoIdentityId   = ""
    , _accountCreateDtoWeb3Provider = Nothing
    , _accountCreateDtoAssets       = Nothing
    , _accountCreateDtoPassphrase   = Nothing
    , _accountCreateDtoPassphraseId = Nothing
    , _accountCreateDtoVaultId      = Nothing
    , _accountCreateDtoWalletId     = Nothing
    }

-- | Request to create a subaccount
data SubaccountCreateDto = SubaccountCreateDto
    { _subaccountCreateDtoName       :: Text
    , _subaccountCreateDtoIdentityId :: IdentityId
    , _subaccountCreateDtoAssets     :: Maybe (Vector AccountAssetDto)
    , _subaccountCreateDtoWalletId   :: Maybe Text  -- For Utila
    } deriving stock (Eq, Generic, Show)

instance FromJSON SubaccountCreateDto where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_subaccountCreateDto" }

instance ToJSON SubaccountCreateDto where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_subaccountCreateDto" }

-- | Default subaccount create request
_SubaccountCreateDto :: SubaccountCreateDto
_SubaccountCreateDto = SubaccountCreateDto
    { _subaccountCreateDtoName       = ""
    , _subaccountCreateDtoIdentityId = ""
    , _subaccountCreateDtoAssets     = Nothing
    , _subaccountCreateDtoWalletId   = Nothing
    }

-- | Accounts API
type API =
    "accounts" :>
        (    -- GET /v1/accounts - List accounts
             QueryParam "namePrefix" Text
          :> QueryParam "startingAfter" Text
          :> QueryParam "limit" Natural
          :> Get '[JSON] (PageResponse AccountEntity)

        :<|> -- POST /v1/accounts - Create account
             ReqBody '[JSON] AccountCreateDto
          :> Post201 '[JSON] AccountEntity

        :<|> -- GET /v1/accounts/{id} - Get by ID
             Capture "id" AccountId
          :> Get '[JSON] AccountEntity

        :<|> -- GET /v1/accounts/{id}/balances - Get balances
             Capture "id" AccountId
          :> "balances"
          :> Get '[JSON] (Vector AccountBalanceEntity)

        :<|> -- POST /v1/accounts/{id}/subaccounts - Create subaccount
             Capture "id" AccountId
          :> "subaccounts"
          :> ReqBody '[JSON] SubaccountCreateDto
          :> Post201 '[JSON] AccountEntity

        :<|> -- POST /v1/accounts/{id}/assets - Add asset to account
             Capture "id" AccountId
          :> "assets"
          :> ReqBody '[JSON] (Vector AccountAssetDto)
          :> Post201 '[JSON] (Vector AccountAssetAddressEntity)
        )
