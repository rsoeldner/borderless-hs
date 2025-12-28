module Borderless.V1.Transactions
    ( -- * Entity Types
      TransactionEntity(..)
    , TransactionSourceEntity(..)
    , TransactionDestinationEntity(..)
    , DepositInstructionEntity(..)
      -- * Filter Types
    , TransactionFilter(..)
    , _TransactionFilter
      -- * Servant API
    , API
    ) where

import Borderless.Prelude
import Borderless.V1.Types.Common
import Borderless.V1.Types.Enums
import Borderless.V1.Types.Pagination

import qualified Data.Aeson as A

-- | Transaction source
data TransactionSourceEntity = TransactionSourceEntity
    { _transactionSourceEntityAsset           :: Maybe AssetId
    , _transactionSourceEntityAmount          :: Maybe DecimalNumber
    , _transactionSourceEntityAccountId       :: Maybe AccountId
    , _transactionSourceEntityPaymentMethod   :: Maybe PaymentMethod
    , _transactionSourceEntityFiatCurrency    :: Maybe FiatCurrencyId
    , _transactionSourceEntityFiatAmount      :: Maybe DecimalNumber
    , _transactionSourceEntityFiatToAssetRate :: Maybe DecimalNumber
    } deriving stock (Eq, Generic, Show)

instance FromJSON TransactionSourceEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_transactionSourceEntity" }

instance ToJSON TransactionSourceEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_transactionSourceEntity" }

-- | Transaction destination
data TransactionDestinationEntity = TransactionDestinationEntity
    { _transactionDestinationEntityAsset                :: Maybe AssetId
    , _transactionDestinationEntityAmount               :: Maybe DecimalNumber
    , _transactionDestinationEntityAccountId            :: Maybe AccountId
    , _transactionDestinationEntityPaymentInstructionId :: Maybe PaymentInstructionId
    , _transactionDestinationEntityPaymentMethod        :: Maybe PaymentMethod
    , _transactionDestinationEntityFiatCurrency         :: Maybe FiatCurrencyId
    , _transactionDestinationEntityFiatAmount           :: Maybe DecimalNumber
    , _transactionDestinationEntityAssetToFiatRate      :: Maybe DecimalNumber
    } deriving stock (Eq, Generic, Show)

instance FromJSON TransactionDestinationEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_transactionDestinationEntity" }

instance ToJSON TransactionDestinationEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_transactionDestinationEntity" }

-- | Deposit instruction details
data DepositInstructionEntity = DepositInstructionEntity
    { _depositInstructionEntityPaymentMethod :: PaymentMethod
    , _depositInstructionEntityDetails       :: A.Value  -- Polymorphic deposit details
    } deriving stock (Eq, Generic, Show)

instance FromJSON DepositInstructionEntity where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_depositInstructionEntity" }

instance ToJSON DepositInstructionEntity where
    toJSON = genericToJSON aesonOptions
        { fieldLabelModifier = labelModifier . stripPrefix "_depositInstructionEntity" }

-- | Transaction entity
data TransactionEntity = TransactionEntity
    { _transactionEntityId                     :: TransactionId
    , _transactionEntityType_                  :: TransactionType
    , _transactionEntityStatus                 :: TransactionStatus
    , _transactionEntitySource                 :: TransactionSourceEntity
    , _transactionEntityDestination            :: TransactionDestinationEntity
    , _transactionEntityInstructions           :: Maybe A.Value
    , _transactionEntityCreatedAt              :: UTCTime
    , _transactionEntityUpdatedAt              :: UTCTime
    , _transactionEntityTxHash                 :: Vector (Maybe Text)
    , _transactionEntityFeeAmount              :: Maybe DecimalNumber
    , _transactionEntityFailureReason          :: Maybe A.Value
    , _transactionEntityDepositInstruction     :: Maybe DepositInstructionEntity
    , _transactionEntityCounterPartyIdentityId :: Maybe IdentityId
    , _transactionEntityDeveloperFeeAmount     :: Maybe DecimalNumber
    } deriving stock (Eq, Generic, Show)

instance FromJSON TransactionEntity where
    parseJSON = withObject "TransactionEntity" $ \v -> TransactionEntity
        <$> v .:  "id"
        <*> v .:  "type"
        <*> v .:  "status"
        <*> v .:  "source"
        <*> v .:  "destination"
        <*> v .:? "instructions"
        <*> v .:  "createdAt"
        <*> v .:  "updatedAt"
        <*> v .:  "txHash"
        <*> v .:? "feeAmount"
        <*> v .:? "failureReason"
        <*> v .:? "depositInstruction"
        <*> v .:? "counterPartyIdentityId"
        <*> v .:? "developerFeeAmount"

instance ToJSON TransactionEntity where
    toJSON TransactionEntity{..} = object
        [ "id"                     .= _transactionEntityId
        , "type"                   .= _transactionEntityType_
        , "status"                 .= _transactionEntityStatus
        , "source"                 .= _transactionEntitySource
        , "destination"            .= _transactionEntityDestination
        , "instructions"           .= _transactionEntityInstructions
        , "createdAt"              .= _transactionEntityCreatedAt
        , "updatedAt"              .= _transactionEntityUpdatedAt
        , "txHash"                 .= _transactionEntityTxHash
        , "feeAmount"              .= _transactionEntityFeeAmount
        , "failureReason"          .= _transactionEntityFailureReason
        , "depositInstruction"     .= _transactionEntityDepositInstruction
        , "counterPartyIdentityId" .= _transactionEntityCounterPartyIdentityId
        , "developerFeeAmount"     .= _transactionEntityDeveloperFeeAmount
        ]

-- | Filter for listing transactions
data TransactionFilter = TransactionFilter
    { _transactionFilterAccountId     :: Maybe AccountId
    , _transactionFilterIdentityId    :: Maybe IdentityId
    , _transactionFilterType_         :: Maybe TransactionType
    , _transactionFilterStatus        :: Maybe TransactionStatus
    , _transactionFilterStartingAfter :: Maybe Text
    , _transactionFilterLimit         :: Maybe Natural
    } deriving stock (Eq, Generic, Show)

-- | Default transaction filter
_TransactionFilter :: TransactionFilter
_TransactionFilter = TransactionFilter
    { _transactionFilterAccountId     = Nothing
    , _transactionFilterIdentityId    = Nothing
    , _transactionFilterType_         = Nothing
    , _transactionFilterStatus        = Nothing
    , _transactionFilterStartingAfter = Nothing
    , _transactionFilterLimit         = Nothing
    }

-- | Transactions API
type API =
    "transactions" :>
        (    -- GET /v1/transactions - List transactions
             QueryParam "accountId" AccountId
          :> QueryParam "identityId" IdentityId
          :> QueryParam "type" TransactionType
          :> QueryParam "status" TransactionStatus
          :> QueryParam "startingAfter" Text
          :> QueryParam "limit" Natural
          :> Get '[JSON] (PageResponse TransactionEntity)

        :<|> -- GET /v1/transactions/{id} - Get by ID
             Capture "id" TransactionId
          :> Get '[JSON] TransactionEntity

        :<|> -- POST /v1/transactions/{id}/cancel - Cancel transaction
             Capture "id" TransactionId
          :> "cancel"
          :> Post201 '[JSON] TransactionEntity
        )
