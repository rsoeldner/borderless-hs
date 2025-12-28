module Borderless.V1.Types.Enums
    ( -- * User & Permission Enums
      DashboardRole(..)
    , DashboardPermission(..)
      -- * Identity Enums
    , IdentityType(..)
    , IdentityStatus(..)
    , IdentityDocumentType(..)
    , BusinessType(..)
    , AccountPurpose(..)
    , SourceOfFunds(..)
    , Sex(..)
      -- * Transaction Enums
    , TransactionType(..)
    , TransactionStatus(..)
    , OperationType(..)
      -- * Asset & Currency Enums
    , AssetId(..)
    , FiatCurrencyId(..)
      -- * Payment Enums
    , PaymentMethod(..)
    , BankAccountType(..)
    , BankAccountOwnerType(..)
      -- * Account Enums
    , Web3Provider(..)
      -- * Compliance Enums
    , ComplianceStatus(..)
      -- * Webhook Enums
    , WebhookEvent(..)
    ) where

import Borderless.Prelude

import qualified Data.Aeson as A

-- | Dashboard roles for users
data DashboardRole
    = RoleOwner
    | RoleViewer
    | RoleCustom
    deriving stock (Eq, Generic, Show)

instance FromJSON DashboardRole where
    parseJSON = A.withText "DashboardRole" $ \case
        "OWNER"  -> pure RoleOwner
        "VIEWER" -> pure RoleViewer
        "CUSTOM" -> pure RoleCustom
        other    -> fail $ "Unknown DashboardRole: " <> show other

instance ToJSON DashboardRole where
    toJSON RoleOwner  = A.String "OWNER"
    toJSON RoleViewer = A.String "VIEWER"
    toJSON RoleCustom = A.String "CUSTOM"

-- | Dashboard permissions
data DashboardPermission
    = OrganizationApiKeysGenerate
    | OrganizationApiKeysRegenerate
    | OrganizationMfaEnable
    | PermissionGroupCreate
    | PermissionGroupUpdate
    | PermissionGroupDelete
    | PermissionGroupUserAdd
    | PermissionGroupUserDelete
    | UserCreate
    | UserUpdate
    | UserDelete
    | UserChangeRole
    | UserMfaEnable
    | AccountCreate
    | AccountAssetAddressAdd
    | IdentityCreate
    | IdentityUpdate
    | IdentityDelete
    | IdentityDocumentsUpload
    | ComplianceCheckTosLinkManage
    | ComplianceCheckStart
    | PaymentInstructionCreate
    | PaymentInstructionDelete
    | TransactionAllCreate
    | TransactionAllCancel
    | TransactionAllExport
    | WebhookPublicKeysGenerate
    | WebhookCreate
    | WebhookUpdate
    | WebhookDelete
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

-- | Identity type (Personal or Business)
data IdentityType
    = Personal
    | Business
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance ToHttpApiData IdentityType where
    toUrlPiece Personal = "Personal"
    toUrlPiece Business = "Business"

-- | Identity status
data IdentityStatus
    = Active
    | Inactive
    | Suspended
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

-- | Identity document types
data IdentityDocumentType
    = Passport
    | DriverLicense
    | NationalId
    | ResidencePermit
    | Formation
    | Ownership
    | ProofOfAddress
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

-- | Business types
data BusinessType
    = Cooperative
    | Corporation
    | Llc
    | BusinessOther
    | Partnership
    | SoleProp
    | Trust
    deriving stock (Eq, Generic, Show)

instance FromJSON BusinessType where
    parseJSON = A.withText "BusinessType" $ \case
        "Cooperative"  -> pure Cooperative
        "Corporation"  -> pure Corporation
        "Llc"          -> pure Llc
        "Other"        -> pure BusinessOther
        "Partnership"  -> pure Partnership
        "SoleProp"     -> pure SoleProp
        "Trust"        -> pure Trust
        other          -> fail $ "Unknown BusinessType: " <> show other

instance ToJSON BusinessType where
    toJSON Cooperative   = A.String "Cooperative"
    toJSON Corporation   = A.String "Corporation"
    toJSON Llc           = A.String "Llc"
    toJSON BusinessOther = A.String "Other"
    toJSON Partnership   = A.String "Partnership"
    toJSON SoleProp      = A.String "SoleProp"
    toJSON Trust         = A.String "Trust"

-- | Account purpose
data AccountPurpose
    = CharitableDonations
    | EcommerceRetailPayments
    | InvestmentPurposes
    | PurposeOther
    | PaymentsToFriendsOrFamilyAbroad
    | Payroll
    | PersonalOrLivingExpenses
    | ProtectWealth
    | PurchaseGoodsAndServices
    | ReceivePaymentsForGoodsAndServices
    | TaxOptimization
    | ThirdPartyMoneyTransmission
    | TreasuryManagement
    deriving stock (Eq, Generic, Show)

instance FromJSON AccountPurpose where
    parseJSON = A.withText "AccountPurpose" $ \case
        "charitable_donations"                  -> pure CharitableDonations
        "ecommerce_retail_payments"             -> pure EcommerceRetailPayments
        "investment_purposes"                   -> pure InvestmentPurposes
        "other"                                 -> pure PurposeOther
        "payments_to_friends_or_family_abroad"  -> pure PaymentsToFriendsOrFamilyAbroad
        "payroll"                               -> pure Payroll
        "personal_or_living_expenses"           -> pure PersonalOrLivingExpenses
        "protect_wealth"                        -> pure ProtectWealth
        "purchase_goods_and_services"           -> pure PurchaseGoodsAndServices
        "receive_payments_for_goods_and_services" -> pure ReceivePaymentsForGoodsAndServices
        "tax_optimization"                      -> pure TaxOptimization
        "third_party_money_transmission"        -> pure ThirdPartyMoneyTransmission
        "treasury_management"                   -> pure TreasuryManagement
        other                                   -> fail $ "Unknown AccountPurpose: " <> show other

instance ToJSON AccountPurpose where
    toJSON CharitableDonations                 = A.String "charitable_donations"
    toJSON EcommerceRetailPayments             = A.String "ecommerce_retail_payments"
    toJSON InvestmentPurposes                  = A.String "investment_purposes"
    toJSON PurposeOther                        = A.String "other"
    toJSON PaymentsToFriendsOrFamilyAbroad     = A.String "payments_to_friends_or_family_abroad"
    toJSON Payroll                             = A.String "payroll"
    toJSON PersonalOrLivingExpenses            = A.String "personal_or_living_expenses"
    toJSON ProtectWealth                       = A.String "protect_wealth"
    toJSON PurchaseGoodsAndServices            = A.String "purchase_goods_and_services"
    toJSON ReceivePaymentsForGoodsAndServices  = A.String "receive_payments_for_goods_and_services"
    toJSON TaxOptimization                     = A.String "tax_optimization"
    toJSON ThirdPartyMoneyTransmission         = A.String "third_party_money_transmission"
    toJSON TreasuryManagement                  = A.String "treasury_management"

-- | Source of funds
data SourceOfFunds
    = BusinessLoans
    | BusinessRevenue
    | EquityFunding
    | Grants
    | InterCompanyFunds
    | InvestmentProceeds
    | LegalSettlement
    | LegalSettlementTaxRefund
    | OwnersCapital
    | PensionRetirement
    | PersonalSavings
    | SaleOfAssets
    | SaleOfBusiness
    | SourceOther
    deriving stock (Eq, Generic, Show)

instance FromJSON SourceOfFunds where
    parseJSON = A.withText "SourceOfFunds" $ \case
        "business_loans"               -> pure BusinessLoans
        "business_revenue"             -> pure BusinessRevenue
        "equity_funding"               -> pure EquityFunding
        "grants"                       -> pure Grants
        "inter_company_funds"          -> pure InterCompanyFunds
        "investment_proceeds"          -> pure InvestmentProceeds
        "legal_settlement"             -> pure LegalSettlement
        "legal_settlement_tax_refund"  -> pure LegalSettlementTaxRefund
        "owners_capital"               -> pure OwnersCapital
        "pension_retirement"           -> pure PensionRetirement
        "personal_savings"             -> pure PersonalSavings
        "sale_of_assets"               -> pure SaleOfAssets
        "sale_of_business"             -> pure SaleOfBusiness
        "other"                        -> pure SourceOther
        other                          -> fail $ "Unknown SourceOfFunds: " <> show other

instance ToJSON SourceOfFunds where
    toJSON BusinessLoans             = A.String "business_loans"
    toJSON BusinessRevenue           = A.String "business_revenue"
    toJSON EquityFunding             = A.String "equity_funding"
    toJSON Grants                    = A.String "grants"
    toJSON InterCompanyFunds         = A.String "inter_company_funds"
    toJSON InvestmentProceeds        = A.String "investment_proceeds"
    toJSON LegalSettlement           = A.String "legal_settlement"
    toJSON LegalSettlementTaxRefund  = A.String "legal_settlement_tax_refund"
    toJSON OwnersCapital             = A.String "owners_capital"
    toJSON PensionRetirement         = A.String "pension_retirement"
    toJSON PersonalSavings           = A.String "personal_savings"
    toJSON SaleOfAssets              = A.String "sale_of_assets"
    toJSON SaleOfBusiness            = A.String "sale_of_business"
    toJSON SourceOther               = A.String "other"

-- | Sex
data Sex = Male | Female
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

-- | Transaction types
data TransactionType
    = AssetDeposit
    | Deposit
    | Withdrawal
    | Transfer
    | TransferToExternalWallet
    | Swap
    | Approval
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance ToHttpApiData TransactionType where
    toUrlPiece AssetDeposit            = "AssetDeposit"
    toUrlPiece Deposit                 = "Deposit"
    toUrlPiece Withdrawal              = "Withdrawal"
    toUrlPiece Transfer                = "Transfer"
    toUrlPiece TransferToExternalWallet = "TransferToExternalWallet"
    toUrlPiece Swap                    = "Swap"
    toUrlPiece Approval                = "Approval"

-- | Transaction status
data TransactionStatus
    = Submitted
    | Verifying
    | Orchestrating
    | Pending
    | Processing
    | Completed
    | Failed
    | Cancelled
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance ToHttpApiData TransactionStatus where
    toUrlPiece Submitted     = "Submitted"
    toUrlPiece Verifying     = "Verifying"
    toUrlPiece Orchestrating = "Orchestrating"
    toUrlPiece Pending       = "Pending"
    toUrlPiece Processing    = "Processing"
    toUrlPiece Completed     = "Completed"
    toUrlPiece Failed        = "Failed"
    toUrlPiece Cancelled     = "Cancelled"

-- | Operation type (for compliance checks)
data OperationType
    = OperationDeposit
    | OperationWithdrawal
    deriving stock (Eq, Generic, Show)

instance FromJSON OperationType where
    parseJSON = A.withText "OperationType" $ \case
        "Deposit"    -> pure OperationDeposit
        "Withdrawal" -> pure OperationWithdrawal
        other        -> fail $ "Unknown OperationType: " <> show other

instance ToJSON OperationType where
    toJSON OperationDeposit    = A.String "Deposit"
    toJSON OperationWithdrawal = A.String "Withdrawal"

instance ToHttpApiData OperationType where
    toUrlPiece OperationDeposit    = "Deposit"
    toUrlPiece OperationWithdrawal = "Withdrawal"

-- | Crypto asset identifiers
data AssetId
    = POL
    | USDT_POLYGON
    | USDC_POLYGON
    | USDM_POLYGON
    | ETH
    | USDT_ETHEREUM
    | USDC_ETHEREUM
    | USDM_ETHEREUM
    | TRX
    | USDT_TRON
    | ETH_BASE
    | USDC_BASE
    | USDM_BASE
    | ETH_OPTIMISM
    | USDT_OPTIMISM
    | USDC_OPTIMISM
    | USDM_OPTIMISM
    | BTC
    | CELO
    | CUSD_CELO
    | USDC_CELO
    | SOL
    | USDC_SOLANA
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance ToHttpApiData AssetId where
    toUrlPiece = \case
        POL           -> "POL"
        USDT_POLYGON  -> "USDT_POLYGON"
        USDC_POLYGON  -> "USDC_POLYGON"
        USDM_POLYGON  -> "USDM_POLYGON"
        ETH           -> "ETH"
        USDT_ETHEREUM -> "USDT_ETHEREUM"
        USDC_ETHEREUM -> "USDC_ETHEREUM"
        USDM_ETHEREUM -> "USDM_ETHEREUM"
        TRX           -> "TRX"
        USDT_TRON     -> "USDT_TRON"
        ETH_BASE      -> "ETH_BASE"
        USDC_BASE     -> "USDC_BASE"
        USDM_BASE     -> "USDM_BASE"
        ETH_OPTIMISM  -> "ETH_OPTIMISM"
        USDT_OPTIMISM -> "USDT_OPTIMISM"
        USDC_OPTIMISM -> "USDC_OPTIMISM"
        USDM_OPTIMISM -> "USDM_OPTIMISM"
        BTC           -> "BTC"
        CELO          -> "CELO"
        CUSD_CELO     -> "CUSD_CELO"
        USDC_CELO     -> "USDC_CELO"
        SOL           -> "SOL"
        USDC_SOLANA   -> "USDC_SOLANA"

-- | Fiat currency identifiers
data FiatCurrencyId
    = USD
    | EUR
    | BRL
    | ARS
    | MXN
    | COP
    | CLP
    | PEN
    | BWP
    | CDF
    | GHS
    | KES
    | MWK
    | NGN
    | RWF
    | ZAR
    | TZS
    | UGX
    | ZMW
    | XOF
    | XAF
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance ToHttpApiData FiatCurrencyId where
    toUrlPiece = \case
        USD -> "USD"
        EUR -> "EUR"
        BRL -> "BRL"
        ARS -> "ARS"
        MXN -> "MXN"
        COP -> "COP"
        CLP -> "CLP"
        PEN -> "PEN"
        BWP -> "BWP"
        CDF -> "CDF"
        GHS -> "GHS"
        KES -> "KES"
        MWK -> "MWK"
        NGN -> "NGN"
        RWF -> "RWF"
        ZAR -> "ZAR"
        TZS -> "TZS"
        UGX -> "UGX"
        ZMW -> "ZMW"
        XOF -> "XOF"
        XAF -> "XAF"

-- | Payment methods
data PaymentMethod
    = ACH
    | Wire
    | Sepa
    | Swift
    | Card
    | MobileMoney
    | PIX
    | PSE
    | SPEI
    | COELSA
    | SPAV
    | CCE
    | NIP
    | GhIPSS
    | BankTransfer
    | EFT
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance ToHttpApiData PaymentMethod where
    toUrlPiece = \case
        ACH          -> "ACH"
        Wire         -> "Wire"
        Sepa         -> "Sepa"
        Swift        -> "Swift"
        Card         -> "Card"
        MobileMoney  -> "MobileMoney"
        PIX          -> "PIX"
        PSE          -> "PSE"
        SPEI         -> "SPEI"
        COELSA       -> "COELSA"
        SPAV         -> "SPAV"
        CCE          -> "CCE"
        NIP          -> "NIP"
        GhIPSS       -> "GhIPSS"
        BankTransfer -> "BankTransfer"
        EFT          -> "EFT"

-- | Bank account type
data BankAccountType
    = Checking
    | Savings
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

-- | Bank account owner type
data BankAccountOwnerType
    = IndividualOwner
    | BusinessOwner
    deriving stock (Eq, Generic, Show)

instance FromJSON BankAccountOwnerType where
    parseJSON = A.withText "BankAccountOwnerType" $ \case
        "Individual" -> pure IndividualOwner
        "Business"   -> pure BusinessOwner
        other        -> fail $ "Unknown BankAccountOwnerType: " <> show other

instance ToJSON BankAccountOwnerType where
    toJSON IndividualOwner = A.String "Individual"
    toJSON BusinessOwner   = A.String "Business"

-- | Web3 infrastructure providers
data Web3Provider
    = Fireblocks
    | Utila
    | Standalone
    | Dfns
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

-- | Compliance check status
data ComplianceStatus
    = NotStarted
    | Started
    | Incomplete
    | AwaitingUbo
    | UnderReview
    | Rejected
    | PreApproved
    | Approved
    | ComplianceCancelled
    deriving stock (Eq, Generic, Show)

instance FromJSON ComplianceStatus where
    parseJSON = A.withText "ComplianceStatus" $ \case
        "NotStarted"   -> pure NotStarted
        "Started"      -> pure Started
        "Incomplete"   -> pure Incomplete
        "AwaitingUbo"  -> pure AwaitingUbo
        "UnderReview"  -> pure UnderReview
        "Rejected"     -> pure Rejected
        "PreApproved"  -> pure PreApproved
        "Approved"     -> pure Approved
        "Cancelled"    -> pure ComplianceCancelled
        other          -> fail $ "Unknown ComplianceStatus: " <> show other

instance ToJSON ComplianceStatus where
    toJSON NotStarted          = A.String "NotStarted"
    toJSON Started             = A.String "Started"
    toJSON Incomplete          = A.String "Incomplete"
    toJSON AwaitingUbo         = A.String "AwaitingUbo"
    toJSON UnderReview         = A.String "UnderReview"
    toJSON Rejected            = A.String "Rejected"
    toJSON PreApproved         = A.String "PreApproved"
    toJSON Approved            = A.String "Approved"
    toJSON ComplianceCancelled = A.String "Cancelled"

-- | Webhook events
data WebhookEvent
    = TransactionCreated
    | TransactionUpdated
    | IdentityCreated
    | IdentityUpdated
    | AccountCreated
    | AccountUpdated
    | ComplianceCheckCreated
    | ComplianceCheckUpdated
    deriving stock (Eq, Generic, Show)

instance FromJSON WebhookEvent where
    parseJSON = A.withText "WebhookEvent" $ \case
        "transaction.created"       -> pure TransactionCreated
        "transaction.updated"       -> pure TransactionUpdated
        "identity.created"          -> pure IdentityCreated
        "identity.updated"          -> pure IdentityUpdated
        "account.created"           -> pure AccountCreated
        "account.updated"           -> pure AccountUpdated
        "compliance_check.created"  -> pure ComplianceCheckCreated
        "compliance_check.updated"  -> pure ComplianceCheckUpdated
        other                       -> fail $ "Unknown WebhookEvent: " <> show other

instance ToJSON WebhookEvent where
    toJSON TransactionCreated      = A.String "transaction.created"
    toJSON TransactionUpdated      = A.String "transaction.updated"
    toJSON IdentityCreated         = A.String "identity.created"
    toJSON IdentityUpdated         = A.String "identity.updated"
    toJSON AccountCreated          = A.String "account.created"
    toJSON AccountUpdated          = A.String "account.updated"
    toJSON ComplianceCheckCreated  = A.String "compliance_check.created"
    toJSON ComplianceCheckUpdated  = A.String "compliance_check.updated"
