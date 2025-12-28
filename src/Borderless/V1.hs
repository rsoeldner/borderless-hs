{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Borderless.V1
    ( -- * Client Configuration
      BorderlessConfig(..)
    , Environment(..)
    , defaultConfig
      -- * Client Initialization
    , withBorderless
    , makeMethods
    , getClientEnv
      -- * Token Management
    , TokenManager
    , startTokenManager
    , stopTokenManager
    , withTokenManager
    , getValidToken
      -- * Methods Record
    , Methods(..)
      -- * Idempotency Key
    , IdempotencyKey(..)
    , newIdempotencyKey
      -- * Re-exports
    , module Borderless.V1.Auth
    , module Borderless.V1.Users
    , module Borderless.V1.PermissionGroups
    , module Borderless.V1.Identities
    , module Borderless.V1.Accounts
    , module Borderless.V1.VirtualAccounts
    , module Borderless.V1.PaymentInstructions
    , module Borderless.V1.Transactions
    , module Borderless.V1.Transactions.Deposits
    , module Borderless.V1.Transactions.Withdrawals
    , module Borderless.V1.Transactions.Swaps
    , module Borderless.V1.Transactions.AssetDeposits
    , module Borderless.V1.Webhooks
    , module Borderless.V1.ComplianceChecks
    , module Borderless.V1.IntegrationsConfig
    , module Borderless.V1.Error
    , module Borderless.V1.Types.Common
    , module Borderless.V1.Types.Enums
    , module Borderless.V1.Types.Pagination
    ) where

import Borderless.Prelude
import Borderless.Client
import Borderless.V1.Auth hiding (API)
import Borderless.V1.Users hiding (API)
import Borderless.V1.PermissionGroups hiding (API)
import Borderless.V1.Identities hiding (API)
import Borderless.V1.Accounts hiding (API)
import Borderless.V1.VirtualAccounts hiding (API)
import Borderless.V1.PaymentInstructions hiding (API)
import Borderless.V1.Transactions hiding (API)
import Borderless.V1.Transactions.Deposits hiding (API)
import Borderless.V1.Transactions.Withdrawals hiding (API)
import Borderless.V1.Transactions.Swaps hiding (API)
import Borderless.V1.Transactions.AssetDeposits hiding (API)
import Borderless.V1.Webhooks hiding (API)
import Borderless.V1.ComplianceChecks hiding (API, CoverageAPI)
import Borderless.V1.IntegrationsConfig hiding (API)
import Borderless.V1.Error
import Borderless.V1.Types.Common
import Borderless.V1.Types.Enums
import Borderless.V1.Types.Pagination

import qualified Control.Exception as Exception
import Data.Proxy (Proxy(..))
import qualified Data.Vector as Vector

import qualified Borderless.V1.Users as Users
import qualified Borderless.V1.PermissionGroups as PermissionGroups
import qualified Borderless.V1.Identities as Identities
import qualified Borderless.V1.Accounts as Accounts
import qualified Borderless.V1.VirtualAccounts as VirtualAccounts
import qualified Borderless.V1.PaymentInstructions as PaymentInstructions
import qualified Borderless.V1.Transactions as Transactions
import qualified Borderless.V1.Transactions.Deposits as Deposits
import qualified Borderless.V1.Transactions.Withdrawals as Withdrawals
import qualified Borderless.V1.Transactions.Swaps as Swaps
import qualified Borderless.V1.Transactions.AssetDeposits as AssetDeposits
import qualified Borderless.V1.Webhooks as Webhooks
import qualified Borderless.V1.ComplianceChecks as ComplianceChecks
import qualified Borderless.V1.IntegrationsConfig as IntegrationsConfig
import qualified Servant.Client as Client

-- | All API methods bundled together
data Methods = Methods
    { -- Users
      getUsers          :: Maybe Text -> Maybe Natural -> IO (PageResponse UserEntity)
    , getUser           :: UserId -> IO UserEntity
    , inviteUser        :: UserInviteDto -> IO UserInviteEntity
    , deleteUser        :: UserId -> IO UserEntity
    , updateUser        :: UserId -> UserUpdateDto -> IO UserEntity
    , updateUserRole    :: UserId -> UserRoleUpdateDto -> IO UserEntity
    , enableUserMfa     :: UserId -> IO UserEntity

      -- Permission Groups
    , getPermissionGroups     :: Maybe Text -> Maybe Text -> Maybe Natural -> IO (PageResponse PermissionGroupEntity)
    , createPermissionGroup   :: PermissionGroupCreateDto -> IO PermissionGroupEntity
    , getPermissionGroup      :: PermissionGroupId -> IO PermissionGroupEntity
    , updatePermissionGroup   :: PermissionGroupId -> PermissionGroupUpdateDto -> IO PermissionGroupEntity
    , deletePermissionGroup   :: PermissionGroupId -> IO PermissionGroupEntity
    , addUsersToPermissionGroup     :: PermissionGroupId -> PermissionGroupUsersDto -> IO PermissionGroupEntity
    , removeUsersFromPermissionGroup :: PermissionGroupId -> PermissionGroupUsersDto -> IO PermissionGroupEntity

      -- Identities
    , getIdentities           :: Maybe Text -> Maybe Natural -> IO (PageResponse IdentityEntity)
    , getIdentity             :: IdentityId -> IO IdentityEntity
    , createPersonalIdentity  :: PersonalIdentityCreateDto -> IO IdentityEntity
    , createBusinessIdentity  :: BusinessIdentityCreateDto -> IO IdentityEntity
    , updatePersonalIdentity  :: IdentityId -> PersonalIdentityUpdateDto -> IO IdentityEntity
    , updateBusinessIdentity  :: IdentityId -> BusinessIdentityUpdateDto -> IO IdentityEntity
    , deleteIdentity          :: IdentityId -> IO IdentityEntity
    , uploadDocument          :: IdentityId -> IdentityDocumentCreateDto -> IO IdentityEntity

      -- Accounts
    , getAccounts        :: Maybe Text -> Maybe Text -> Maybe Natural -> IO (PageResponse AccountEntity)
    , createAccount      :: AccountCreateDto -> IO AccountEntity
    , getAccount         :: AccountId -> IO AccountEntity
    , getAccountBalances :: AccountId -> IO (Vector AccountBalanceEntity)
    , createSubaccount   :: AccountId -> SubaccountCreateDto -> IO AccountEntity
    , addAssetToAccount  :: AccountId -> AccountAssetDto -> IO (Vector AccountAssetAddressEntity)

      -- Virtual Accounts (under /v1/accounts/{id}/virtual-accounts)
    , getVirtualAccounts    :: AccountId -> Maybe Text -> Maybe Natural -> IO (PageResponse VirtualAccountEntity)
    , createVirtualAccount  :: AccountId -> IdempotencyKey -> VirtualAccountCreateDto -> IO VirtualAccountEntity
    , getVirtualAccount     :: AccountId -> VirtualAccountId -> IO VirtualAccountEntity
    , deleteVirtualAccount  :: AccountId -> VirtualAccountId -> IO VirtualAccountEntity

      -- Payment Instructions
    , getPaymentInstructions   :: Maybe Text -> Maybe Natural -> IO (PageResponse PaymentInstructionEntity)
    , createPaymentInstruction :: PaymentInstructionCreateDto -> IO PaymentInstructionEntity
    , getPaymentInstruction    :: PaymentInstructionId -> IO PaymentInstructionEntity
    , deletePaymentInstruction :: PaymentInstructionId -> IO PaymentInstructionEntity

      -- Transactions
    , getTransactions   :: Maybe AccountId -> Maybe IdentityId -> Maybe TransactionType -> Maybe TransactionStatus -> Maybe Text -> Maybe Natural -> IO (PageResponse TransactionEntity)
    , getTransaction    :: TransactionId -> IO TransactionEntity
    , cancelTransaction :: TransactionId -> IO TransactionEntity

      -- Deposits
    , createDeposit   :: IdempotencyKey -> DepositCreateDto -> IO TransactionEntity
    , getDepositQuote :: CountryCode -> FiatCurrencyId -> AssetId -> Maybe PaymentMethod -> Maybe DecimalNumber -> Maybe DecimalNumber -> IO DepositQuoteEntity

      -- Withdrawals
    , createWithdrawal   :: WithdrawalCreateDto -> IO TransactionEntity
    , getWithdrawalQuote :: WithdrawalQuoteDto -> IO WithdrawalQuoteEntity
    , confirmWithdrawal  :: TransactionId -> IO TransactionEntity
    , cancelWithdrawal   :: TransactionId -> IO TransactionEntity

      -- Swaps
    , createSwap   :: SwapCreateDto -> IO TransactionEntity
    , getSwapQuote :: SwapQuoteDto -> IO SwapQuoteEntity
    , confirmSwap  :: TransactionId -> IO TransactionEntity
    , cancelSwap   :: TransactionId -> IO TransactionEntity

      -- Asset Deposits
    , createAssetDeposit :: AssetDepositCreateDto -> IO TransactionEntity

      -- Webhooks
    , getWebhooks            :: IO (Vector WebhookSettingsEntity)
    , createWebhook          :: WebhookSettingsCreateDto -> IO WebhookSettingsEntity
    , getWebhook             :: WebhookId -> IO WebhookSettingsEntity
    , updateWebhook          :: WebhookId -> WebhookSettingsUpdateDto -> IO WebhookSettingsEntity
    , deleteWebhook          :: WebhookId -> IO WebhookSettingsEntity
    , getWebhookPublicKey    :: IO Text
    , regenerateWebhookPublicKey :: IO Text

      -- Compliance Checks
    , getComplianceCoverage      :: Maybe CountryCode -> Maybe AssetId -> Maybe FiatCurrencyId -> Maybe PaymentMethod -> Maybe OperationType -> Maybe IdentityType -> IO ComplianceCheckEntity
    , getIdentityComplianceChecks :: IdentityId -> Maybe CountryCode -> Maybe AssetId -> Maybe FiatCurrencyId -> Maybe PaymentMethod -> Maybe OperationType -> IO (Vector ComplianceCheckEntity)
    , getComplianceRequirements  :: IdentityId -> ComplianceSlug -> IO ComplianceRequirementsEntity
    , startComplianceCheck       :: IdentityId -> ComplianceSlug -> IdempotencyKey -> IO ComplianceCheckStartedEntity

      -- Integrations Config
    , setupDfnsIntegration       :: DfnsSetupDto -> IO NoContent
    , setupUtilaIntegration      :: UtilaSetupDto -> IO NoContent
    , setupYellowcardIntegration :: YellowcardSetupDto -> IO NoContent
    , setupBitsoIntegration      :: BitsoSetupDto -> IO IntegrationConfigEntity
    }

-- | Sub-API types with Authorization header
type UsersAPI = Header' '[Required, Strict] "Authorization" Text :> "v1" :> Users.API
type PermissionGroupsAPI = Header' '[Required, Strict] "Authorization" Text :> "v1" :> PermissionGroups.API
type IdentitiesAPI = Header' '[Required, Strict] "Authorization" Text :> "v1" :> Identities.API
type AccountsAPI = Header' '[Required, Strict] "Authorization" Text :> "v1" :> Accounts.API
type VirtualAccountsAPI = Header' '[Required, Strict] "Authorization" Text :> "v1" :> VirtualAccounts.API
type PaymentInstructionsAPI = Header' '[Required, Strict] "Authorization" Text :> "v1" :> PaymentInstructions.API
type TransactionsAPI = Header' '[Required, Strict] "Authorization" Text :> "v1" :> Transactions.API
type DepositsAPI = Header' '[Required, Strict] "Authorization" Text :> "v1" :> Deposits.API
type WithdrawalsAPI = Header' '[Required, Strict] "Authorization" Text :> "v1" :> Withdrawals.API
type SwapsAPI = Header' '[Required, Strict] "Authorization" Text :> "v1" :> Swaps.API
type AssetDepositsAPI = Header' '[Required, Strict] "Authorization" Text :> "v1" :> AssetDeposits.API
type WebhooksAPI = Header' '[Required, Strict] "Authorization" Text :> "v1" :> Webhooks.API
type ComplianceCoverageAPI = Header' '[Required, Strict] "Authorization" Text :> "v1" :> ComplianceChecks.CoverageAPI
type ComplianceChecksAPI = Header' '[Required, Strict] "Authorization" Text :> "v1" :> ComplianceChecks.API
type IntegrationsConfigAPI = Header' '[Required, Strict] "Authorization" Text :> "v1" :> IntegrationsConfig.API

-- | Initialize Borderless client with automatic token management
withBorderless :: BorderlessConfig -> (Methods -> IO a) -> IO a
withBorderless config action = do
    withTokenManager config $ \tokenManager -> do
        clientEnv <- getClientEnv (environmentBaseUrl (_borderlessConfigEnvironment config))
        action (makeMethods clientEnv tokenManager)
  where
    environmentBaseUrl Sandbox    = "https://sandbox-api.borderless.xyz"
    environmentBaseUrl Production = "https://api.borderless.xyz"

-- | Create Methods record from ClientEnv and TokenManager
makeMethods :: Client.ClientEnv -> TokenManager -> Methods
makeMethods clientEnv tokenManager = Methods
    { getUsers = \sa lim -> runAuth $ \a -> usersGet a sa lim
    , getUser = \uid -> runAuth $ \a -> userGet a uid
    , inviteUser = \dto -> runAuth $ \a -> userInvite a dto
    , deleteUser = \uid -> runAuth $ \a -> userDelete a uid
    , updateUser = \uid dto -> runAuth $ \a -> userUpdate a uid dto
    , updateUserRole = \uid dto -> runAuth $ \a -> userRoleUpdate a uid dto
    , enableUserMfa = \uid -> runAuth $ \a -> userMfaEnable a uid

    , getPermissionGroups = \np sa lim -> runAuth $ \a -> permGroupsGet a np sa lim
    , createPermissionGroup = \dto -> runAuth $ \a -> permGroupCreate a dto
    , getPermissionGroup = \gid -> runAuth $ \a -> permGroupGet a gid
    , updatePermissionGroup = \gid dto -> runAuth $ \a -> permGroupUpdate a gid dto
    , deletePermissionGroup = \gid -> runAuth $ \a -> permGroupDelete a gid
    , addUsersToPermissionGroup = \gid dto -> runAuth $ \a -> permGroupAddUsers a gid dto
    , removeUsersFromPermissionGroup = \gid dto -> runAuth $ \a -> permGroupRemoveUsers a gid dto

    , getIdentities = \sa lim -> runAuth $ \a -> identitiesGet a sa lim
    , getIdentity = \iid -> runAuth $ \a -> identityGet a iid
    , createPersonalIdentity = \dto -> runAuth $ \a -> personalIdentityCreate a dto
    , createBusinessIdentity = \dto -> runAuth $ \a -> businessIdentityCreate a dto
    , updatePersonalIdentity = \iid dto -> runAuth $ \a -> personalIdentityUpdate a iid dto
    , updateBusinessIdentity = \iid dto -> runAuth $ \a -> businessIdentityUpdate a iid dto
    , deleteIdentity = \iid -> runAuth $ \a -> identityDelete a iid
    , uploadDocument = \iid dto -> runAuth $ \a -> documentUpload a iid dto

    , getAccounts = \np sa lim -> runAuth $ \a -> accountsGet a np sa lim
    , createAccount = \dto -> runAuth $ \a -> accountCreate a dto
    , getAccount = \aid -> runAuth $ \a -> accountGet a aid
    , getAccountBalances = \aid -> runAuth $ \a -> accountBalances a aid
    , createSubaccount = \aid dto -> runAuth $ \a -> subaccountCreate a aid dto
    , addAssetToAccount = \aid dto -> runAuth $ \a -> accountAddAsset a aid (Vector.singleton dto)

    , getVirtualAccounts = \aid sa lim -> runAuth $ \a -> virtualAccountsGet a aid sa lim
    , createVirtualAccount = \aid ikey dto -> runAuth $ \a -> virtualAccountCreate a aid ikey dto
    , getVirtualAccount = \aid vid -> runAuth $ \a -> virtualAccountGet a aid vid
    , deleteVirtualAccount = \aid vid -> runAuth $ \a -> virtualAccountDelete a aid vid

    , getPaymentInstructions = \sa lim -> runAuth $ \a -> paymentInstructionsGet a sa lim
    , createPaymentInstruction = \dto -> runAuth $ \a -> paymentInstructionCreate a dto
    , getPaymentInstruction = \pid -> runAuth $ \a -> paymentInstructionGet a pid
    , deletePaymentInstruction = \pid -> runAuth $ \a -> paymentInstructionDelete a pid

    , getTransactions = \aid iid typ st sa lim -> runAuth $ \a -> transactionsGet a aid iid typ st sa lim
    , getTransaction = \tid -> runAuth $ \a -> transactionGet a tid
    , cancelTransaction = \tid -> runAuth $ \a -> transactionCancel a tid

    , createDeposit = \ikey dto -> runAuth $ \a -> depositCreate a ikey dto
    , getDepositQuote = \country fiat asset pm fromAmt toAmt -> runAuth $ \a -> depositQuote a country fiat asset pm fromAmt toAmt

    , createWithdrawal = \dto -> runAuth $ \a -> withdrawalCreate a dto
    , getWithdrawalQuote = \dto -> runAuth $ \a -> withdrawalQuote a dto
    , confirmWithdrawal = \tid -> runAuth $ \a -> withdrawalConfirm a tid
    , cancelWithdrawal = \tid -> runAuth $ \a -> withdrawalCancel a tid

    , createSwap = \dto -> runAuth $ \a -> swapCreate a dto
    , getSwapQuote = \dto -> runAuth $ \a -> swapQuote a dto
    , confirmSwap = \tid -> runAuth $ \a -> swapConfirm a tid
    , cancelSwap = \tid -> runAuth $ \a -> swapCancel a tid

    , createAssetDeposit = \dto -> runAuth $ \a -> assetDepositCreate a dto

    , getWebhooks = runAuth $ \a -> webhooksGet a
    , createWebhook = \dto -> runAuth $ \a -> webhookCreate a dto
    , getWebhook = \wid -> runAuth $ \a -> webhookGet a wid
    , updateWebhook = \wid dto -> runAuth $ \a -> webhookUpdate a wid dto
    , deleteWebhook = \wid -> runAuth $ \a -> webhookDelete a wid
    , getWebhookPublicKey = runAuth $ \a -> webhookPubKeyGet a
    , regenerateWebhookPublicKey = runAuth $ \a -> webhookPubKeyRegen a

    , getComplianceCoverage = \c a f pm t it -> runAuth $ \auth -> complianceCoverageGet auth c a f pm t it
    , getIdentityComplianceChecks = \iid c a f pm t -> runAuth $ \auth -> identityComplianceChecksGet auth iid c a f pm t
    , getComplianceRequirements = \iid slug -> runAuth $ \auth -> complianceRequirementsGet auth iid slug
    , startComplianceCheck = \iid slug ikey -> runAuth $ \auth -> complianceCheckStart auth iid slug ikey

    , setupDfnsIntegration = \dto -> runAuth $ \a -> dfnsSetup a dto
    , setupUtilaIntegration = \dto -> runAuth $ \a -> utilaSetup a dto
    , setupYellowcardIntegration = \dto -> runAuth $ \a -> yellowcardSetup a dto
    , setupBitsoIntegration = \dto -> runAuth $ \a -> bitsoSetup a dto
    }
  where
    -- Helper to run a ClientM action with current token
    runAuth :: (Text -> Client.ClientM r) -> IO r
    runAuth f = do
        token <- getValidToken tokenManager
        let authorization = "Bearer " <> token
        result <- Client.runClientM (f authorization) clientEnv
        case result of
            Left err -> Exception.throwIO (HttpError err)
            Right r  -> pure r

    -- Client functions that take auth header
    usersClient = Client.client (Proxy @UsersAPI)
    permGroupsClient = Client.client (Proxy @PermissionGroupsAPI)
    identitiesClient = Client.client (Proxy @IdentitiesAPI)
    accountsClient = Client.client (Proxy @AccountsAPI)
    virtualAccountsClient = Client.client (Proxy @VirtualAccountsAPI)
    paymentInstructionsClient = Client.client (Proxy @PaymentInstructionsAPI)
    transactionsClient = Client.client (Proxy @TransactionsAPI)
    depositsClient = Client.client (Proxy @DepositsAPI)
    withdrawalsClient = Client.client (Proxy @WithdrawalsAPI)
    swapsClient = Client.client (Proxy @SwapsAPI)
    assetDepositsClient = Client.client (Proxy @AssetDepositsAPI)
    webhooksClient = Client.client (Proxy @WebhooksAPI)
    complianceChecksClient = Client.client (Proxy @ComplianceChecksAPI)
    integrationsClient = Client.client (Proxy @IntegrationsConfigAPI)

    -- Helper to extract functions from the client (applied with auth)
    usersGet a = let (f :<|> _) = usersClient a in f
    userGet a = let (_ :<|> f :<|> _) = usersClient a in f
    userInvite a = let (_ :<|> _ :<|> f :<|> _) = usersClient a in f
    userDelete a = let (_ :<|> _ :<|> _ :<|> f :<|> _) = usersClient a in f
    userUpdate a = let (_ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = usersClient a in f
    userRoleUpdate a = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = usersClient a in f
    userMfaEnable a = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f) = usersClient a in f

    permGroupsGet a = let (f :<|> _) = permGroupsClient a in f
    permGroupCreate a = let (_ :<|> f :<|> _) = permGroupsClient a in f
    permGroupGet a = let (_ :<|> _ :<|> f :<|> _) = permGroupsClient a in f
    permGroupUpdate a = let (_ :<|> _ :<|> _ :<|> f :<|> _) = permGroupsClient a in f
    permGroupDelete a = let (_ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = permGroupsClient a in f
    permGroupAddUsers a = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = permGroupsClient a in f
    permGroupRemoveUsers a = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f) = permGroupsClient a in f

    identitiesGet a = let (f :<|> _) = identitiesClient a in f
    identityGet a = let (_ :<|> f :<|> _) = identitiesClient a in f
    personalIdentityCreate a = let (_ :<|> _ :<|> f :<|> _) = identitiesClient a in f
    businessIdentityCreate a = let (_ :<|> _ :<|> _ :<|> f :<|> _) = identitiesClient a in f
    personalIdentityUpdate a = let (_ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = identitiesClient a in f
    businessIdentityUpdate a = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = identitiesClient a in f
    identityDelete a = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = identitiesClient a in f
    documentUpload a = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f) = identitiesClient a in f

    accountsGet a = let (f :<|> _) = accountsClient a in f
    accountCreate a = let (_ :<|> f :<|> _) = accountsClient a in f
    accountGet a = let (_ :<|> _ :<|> f :<|> _) = accountsClient a in f
    accountBalances a = let (_ :<|> _ :<|> _ :<|> f :<|> _) = accountsClient a in f
    subaccountCreate a = let (_ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = accountsClient a in f
    accountAddAsset a = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f) = accountsClient a in f

    virtualAccountsGet a aid = let (f :<|> _) = virtualAccountsClient a aid in f
    virtualAccountCreate a aid ikey = let (_ :<|> f :<|> _) = virtualAccountsClient a aid in f ikey
    virtualAccountGet a aid = let (_ :<|> _ :<|> f :<|> _) = virtualAccountsClient a aid in f
    virtualAccountDelete a aid = let (_ :<|> _ :<|> _ :<|> f) = virtualAccountsClient a aid in f

    paymentInstructionsGet a = let (f :<|> _) = paymentInstructionsClient a in f
    paymentInstructionCreate a = let (_ :<|> f :<|> _) = paymentInstructionsClient a in f
    paymentInstructionGet a = let (_ :<|> _ :<|> f :<|> _) = paymentInstructionsClient a in f
    paymentInstructionDelete a = let (_ :<|> _ :<|> _ :<|> f) = paymentInstructionsClient a in f

    transactionsGet a = let (f :<|> _) = transactionsClient a in f
    transactionGet a = let (_ :<|> f :<|> _) = transactionsClient a in f
    transactionCancel a = let (_ :<|> _ :<|> f) = transactionsClient a in f

    depositCreate a = let (f :<|> _) = depositsClient a in f
    depositQuote a = let (_ :<|> f) = depositsClient a in f

    withdrawalCreate a = let (f :<|> _) = withdrawalsClient a in f
    withdrawalQuote a = let (_ :<|> f :<|> _) = withdrawalsClient a in f
    withdrawalConfirm a = let (_ :<|> _ :<|> f :<|> _) = withdrawalsClient a in f
    withdrawalCancel a = let (_ :<|> _ :<|> _ :<|> f) = withdrawalsClient a in f

    swapCreate a = let (f :<|> _) = swapsClient a in f
    swapQuote a = let (_ :<|> f :<|> _) = swapsClient a in f
    swapConfirm a = let (_ :<|> _ :<|> f :<|> _) = swapsClient a in f
    swapCancel a = let (_ :<|> _ :<|> _ :<|> f) = swapsClient a in f

    assetDepositCreate a = assetDepositsClient a

    webhooksGet a = let (f :<|> _) = webhooksClient a in f
    webhookCreate a = let (_ :<|> f :<|> _) = webhooksClient a in f
    webhookGet a = let (_ :<|> _ :<|> f :<|> _) = webhooksClient a in f
    webhookUpdate a = let (_ :<|> _ :<|> _ :<|> f :<|> _) = webhooksClient a in f
    webhookDelete a = let (_ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = webhooksClient a in f
    webhookPubKeyGet a = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = webhooksClient a in f
    webhookPubKeyRegen a = let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f) = webhooksClient a in f

    complianceCoverageClient = Client.client (Proxy @ComplianceCoverageAPI)
    complianceCoverageGet a = complianceCoverageClient a
    identityComplianceChecksGet a = let (f :<|> _) = complianceChecksClient a in f
    complianceRequirementsGet a = let (_ :<|> f :<|> _) = complianceChecksClient a in f
    complianceCheckStart a = let (_ :<|> _ :<|> f) = complianceChecksClient a in f

    dfnsSetup a = let (f :<|> _) = integrationsClient a in f
    utilaSetup a = let (_ :<|> f :<|> _) = integrationsClient a in f
    yellowcardSetup a = let (_ :<|> _ :<|> f :<|> _) = integrationsClient a in f
    bitsoSetup a = let (_ :<|> _ :<|> _ :<|> f) = integrationsClient a in f
