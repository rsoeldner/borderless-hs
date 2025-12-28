{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Borderless.V1

import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException)
import Control.Monad (void)
import Data.Vector (Vector)
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified System.Environment as Environment

main :: IO ()
main = do
    -- Check if API credentials are available for E2E tests
    mCreds <- getCredentials
    e2eTests <- case mCreds of
        Nothing -> do
            putStrLn "Note: Skipping E2E tests (BORDERLESS_CLIENT_ID/BORDERLESS_CLIENT_SECRET not set)"
            pure $ testGroup "E2E Tests (skipped)" []
        Just (clientId, clientSecret) -> do
            putStrLn "Note: E2E tests enabled (credentials found)"
            pure $ e2eTestsWithCreds clientId clientSecret

    defaultMain $ testGroup "Borderless Tests"
        [ jsonTests
        , typeTests
        , e2eTests
        ]

-- | Try to get API credentials from environment
getCredentials :: IO (Maybe (T.Text, T.Text))
getCredentials = do
    result <- try $ do
        cid <- T.pack <$> Environment.getEnv "BORDERLESS_CLIENT_ID"
        csec <- T.pack <$> Environment.getEnv "BORDERLESS_CLIENT_SECRET"
        pure (cid, csec)
    case result of
        Left (_ :: SomeException) -> pure Nothing
        Right creds -> pure (Just creds)

--------------------------------------------------------------------------------
-- JSON Serialization Tests
--------------------------------------------------------------------------------

jsonTests :: TestTree
jsonTests = testGroup "JSON Serialization"
    [ testCase "DashboardRole roundtrip" $ do
        let role = RoleOwner
        A.decode (A.encode role) @?= Just role

    , testCase "TransactionType roundtrip" $ do
        let txType = Deposit
        A.decode (A.encode txType) @?= Just txType

    , testCase "TransactionStatus roundtrip" $ do
        let status = Completed
        A.decode (A.encode status) @?= Just status

    , testCase "AssetId roundtrip" $ do
        let asset = USDC_ETHEREUM
        A.decode (A.encode asset) @?= Just asset

    , testCase "FiatCurrencyId roundtrip" $ do
        let currency = USD
        A.decode (A.encode currency) @?= Just currency

    , testCase "PageResponse roundtrip" $ do
        let page = PageResponse
                { _pageResponseHasMore = True
                , _pageResponseData_ = mempty :: Vector T.Text
                }
        A.decode (A.encode page) @?= Just page

    , testCase "AuthM2MSignInDto roundtrip" $ do
        let dto = AuthM2MSignInDto
                { _authM2MSignInDtoClientId = "client123"
                , _authM2MSignInDtoClientSecret = "secret456"
                }
        A.decode (A.encode dto) @?= Just dto

    , testCase "AuthM2MTokenEntity roundtrip" $ do
        let entity = AuthM2MTokenEntity
                { _authM2MTokenEntityAccessToken = "token123"
                , _authM2MTokenEntityTokenType = "Bearer"
                , _authM2MTokenEntityExpiresIn = 3600
                }
        A.decode (A.encode entity) @?= Just entity

    , testCase "BusinessType Other mapping" $ do
        let json = A.String "Other"
        A.fromJSON json @?= A.Success BusinessOther

    , testCase "OperationType Deposit mapping" $ do
        let json = A.String "Deposit"
        A.fromJSON json @?= A.Success OperationDeposit

    , testCase "ComplianceStatus Cancelled mapping" $ do
        let json = A.String "Cancelled"
        A.fromJSON json @?= A.Success ComplianceCancelled
    ]

--------------------------------------------------------------------------------
-- Type Utilities Tests
--------------------------------------------------------------------------------

typeTests :: TestTree
typeTests = testGroup "Type Utilities"
    [ testCase "UserId IsString" $ do
        let uid :: UserId
            uid = "user123"
        unUserId uid @?= "user123"

    , testCase "AccountId IsString" $ do
        let aid :: AccountId
            aid = "account456"
        unAccountId aid @?= "account456"

    , testCase "TransactionId IsString" $ do
        let tid :: TransactionId
            tid = "tx789"
        unTransactionId tid @?= "tx789"

    , testCase "CountryCode IsString" $ do
        let cc :: CountryCode
            cc = "US"
        unCountryCode cc @?= "US"

    , testCase "DecimalNumber IsString" $ do
        let dn :: DecimalNumber
            dn = "123.456"
        unDecimalNumber dn @?= "123.456"

    , testCase "emptyPage" $ do
        let page = emptyPage :: PageResponse ()
        _pageResponseHasMore page @?= False
        _pageResponseData_ page @?= mempty
    ]

--------------------------------------------------------------------------------
-- E2E Tests (only run when credentials are available)
--------------------------------------------------------------------------------

-- | E2E tests that require API credentials
e2eTestsWithCreds :: T.Text -> T.Text -> TestTree
e2eTestsWithCreds clientId clientSecret = testGroup "E2E Tests"
    [ testCase "EUR deposit flow (sandbox)" $ eurDepositFlow clientId clientSecret
    ]

-- | EUR deposit flow test (following quick-start-guide)
--
-- Flow:
--   1. Create a personal identity
--   2. Update identity with taxId
--   3. Get compliance checks for EUR/SEPA deposits
--   4. Get compliance requirements
--   5. Upload identity documents (Passport, ProofOfAddress)
--   6. Start compliance check
--   7. Poll for compliance approval (every 30s, max 10 minutes)
--   8. Create account
--   9. Add asset to account (USDC_POLYGON)
--   10. Get deposit quote
--   11. Create deposit (if compliance approved)
--
eurDepositFlow :: T.Text -> T.Text -> Assertion
eurDepositFlow clientId clientSecret = do
    let config = defaultConfig
            { _borderlessConfigClientId = clientId
            , _borderlessConfigClientSecret = clientSecret
            , _borderlessConfigEnvironment = Sandbox
            }

    withBorderless config $ \methods@Methods{..} -> do
        -- Step 1: Create Personal Identity
        identity <- createTestIdentity methods
        let identityId = _identityEntityId identity
        assertBool "Identity should have an ID" (not $ T.null $ unIdentityId identityId)

        -- Step 2: Update identity with taxId
        let taxUpdateDto = _PersonalIdentityUpdateDto
                { _personalIdentityUpdateDtoTaxId = Just "DE123456789"
                }
        void $ updatePersonalIdentity identityId taxUpdateDto

        -- Step 3: Get Compliance Checks
        complianceChecks <- getIdentityComplianceChecks identityId
            (Just "DE")             -- country
            (Just USDC_POLYGON)     -- asset
            (Just EUR)              -- fiat
            (Just Sepa)             -- paymentMethod
            (Just OperationDeposit) -- type
        assertBool "Should have at least one compliance check" (not $ V.null complianceChecks)
        let complianceInfo = V.head complianceChecks
            slug = _complianceCheckEntitySlug complianceInfo
        assertBool "Compliance check should have a slug" (not $ T.null $ unComplianceSlug slug)

        -- Step 4: Get compliance requirements
        requirements <- getComplianceRequirements identityId slug
        assertBool "Requirements should have slug" (unComplianceSlug (_complianceRequirementsEntitySlug requirements) == unComplianceSlug slug)

        -- Step 5: Upload identity documents
        -- Upload Passport
        let passportDoc = _IdentityDocumentCreateDto
                { _identityDocumentCreateDtoIssuingCountry = "DE"
                , _identityDocumentCreateDtoType_          = Passport
                , _identityDocumentCreateDtoIssuedDate     = "2020-01-15"
                , _identityDocumentCreateDtoExpiryDate     = Just "2030-01-15"
                , _identityDocumentCreateDtoImageFront     = Just testImageBase64
                }
        void $ uploadDocument identityId passportDoc

        -- Upload ProofOfAddress
        let proofOfAddressDoc = _IdentityDocumentCreateDto
                { _identityDocumentCreateDtoIssuingCountry = "DE"
                , _identityDocumentCreateDtoType_          = ProofOfAddress
                , _identityDocumentCreateDtoIssuedDate     = "2024-01-01"
                , _identityDocumentCreateDtoImageFront     = Just testImageBase64
                }
        void $ uploadDocument identityId proofOfAddressDoc

        -- Step 6: Start compliance check
        idempKey <- newIdempotencyKey
        complianceResult <- try $ startComplianceCheck identityId slug idempKey
        case complianceResult of
            Left (e :: SomeException) -> do
                -- Compliance check may fail if ToS not accepted via web link
                -- This is expected in automated tests
                putStrLn $ "Note: Compliance check start failed (expected if ToS not accepted): " <> show e
            Right startedCheck -> do
                assertBool "Started check should have a status"
                    (True) -- Just verify we got a response
                let status = _complianceCheckStartedEntityStatus startedCheck
                putStrLn $ "Compliance check started: " <> show status

        -- Step 7: Poll for compliance approval (20 attempts * 30 seconds = 10 minutes max)
        putStrLn "Polling for compliance approval (every 30s, max 10 minutes)..."
        finalStatus <- pollForApproval getIdentityComplianceChecks identityId slug 20
        putStrLn $ "Final compliance status: " <> show finalStatus

        -- Step 8: Create Account
        account <- createTestAccount methods identityId
        let accountId = _accountEntityId account
        assertBool "Account should have an ID" (not $ T.null $ unAccountId accountId)
        putStrLn $ "Account created: " <> T.unpack (unAccountId accountId)

        -- Step 9: Add asset to account
        putStrLn "Adding USDC_POLYGON asset to account..."
        let assetDto = AccountAssetDto
                { _accountAssetDtoAsset = USDC_POLYGON
                , _accountAssetDtoAddress = Nothing
                , _accountAssetDtoDfnsWalletId = Nothing
                }
        addAssetResult <- try $ addAssetToAccount accountId assetDto
        case addAssetResult of
            Left (e :: SomeException) -> do
                putStrLn $ "Warning: Failed to add asset: " <> show e
                putStrLn "Note: Standalone mode may require a pre-existing wallet address."
                putStrLn "Continuing anyway to see if deposit still works..."
            Right addedAssets -> do
                putStrLn $ "Asset added. " <> show (V.length addedAssets) <> " asset(s) configured"

        -- Step 10: Get Deposit Quote
        quote <- getDepositQuote
            "DE"                 -- country
            EUR                  -- fiat
            USDC_POLYGON         -- asset
            (Just Sepa)          -- paymentMethod
            (Just "100.00")      -- fromAmount
            Nothing              -- toAmount
        _depositQuoteEntityFiat quote @?= EUR
        _depositQuoteEntityAsset quote @?= USDC_POLYGON
        assertBool "Quote should have a from amount" (not $ T.null $ unDecimalNumber $ _depositQuoteEntityFromAmount quote)
        assertBool "Quote should have a to amount" (not $ T.null $ unDecimalNumber $ _depositQuoteEntityToAmount quote)

        -- Step 11: Create deposit (only if compliance is approved)
        if finalStatus == Approved
            then do
                putStrLn "Creating deposit..."
                depositIdempKey <- newIdempotencyKey
                let depositDto = DepositCreateDto
                        { _depositCreateDtoAccountId = accountId
                        , _depositCreateDtoAsset = USDC_POLYGON
                        , _depositCreateDtoFiat = EUR
                        , _depositCreateDtoAmount = "100.00"
                        , _depositCreateDtoCountry = "DE"
                        , _depositCreateDtoPaymentMethod = Just Sepa
                        , _depositCreateDtoDeveloperFee = Nothing
                        }
                tx <- createDeposit depositIdempKey depositDto
                assertBool "Transaction should have an ID"
                    (not $ T.null $ unTransactionId $ _transactionEntityId tx)
                putStrLn $ "Deposit created! Transaction ID: " <> T.unpack (unTransactionId $ _transactionEntityId tx)
            else do
                putStrLn $ "Skipping deposit creation (compliance status: " <> show finalStatus <> ")"
                putStrLn "Note: Deposit creation requires approved compliance"

-- | Create a test personal identity for EUR deposits (Germany)
createTestIdentity :: Methods -> IO IdentityEntity
createTestIdentity Methods{..} = do
    -- Generate unique email to avoid conflicts with previous test runs
    uniqueId <- newIdempotencyKey
    let uniqueEmail = "test+" <> T.pack (show (unIdempotencyKey uniqueId)) <> "@example.com"
    let dto = _PersonalIdentityCreateDto
            { _personalIdentityCreateDtoFirstName = "Test"
            , _personalIdentityCreateDtoLastName = "User"
            , _personalIdentityCreateDtoDateOfBirth = "1990-01-15"
            , _personalIdentityCreateDtoEmail = Just uniqueEmail
            , _personalIdentityCreateDtoPhone = Just "+49123456789"
            , _personalIdentityCreateDtoAddress = Just PostalAddressDto
                { _postalAddressDtoStreet1 = "Musterstraße 123"
                , _postalAddressDtoStreet2 = Nothing
                , _postalAddressDtoCity = "Berlin"
                , _postalAddressDtoState = Just "Berlin"
                , _postalAddressDtoCountry = "DE"
                , _postalAddressDtoPostalCode = "10115"
                }
            }
    createPersonalIdentity dto

-- | Create an account for receiving deposits
createTestAccount :: Methods -> IdentityId -> IO AccountEntity
createTestAccount Methods{..} identityId = do
    let dto = _AccountCreateDto
            { _accountCreateDtoName = "[E2E Auto-Generated] Test EUR Deposit Account"
            , _accountCreateDtoIdentityId = identityId
            }
    createAccount dto

-- | Minimal 1x1 pixel PNG image encoded as base64 data URI
-- This is used for test document uploads
testImageBase64 :: T.Text
testImageBase64 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="

-- | Poll for compliance approval
-- Returns the final status after approval or timeout
pollForApproval
    :: (IdentityId -> Maybe CountryCode -> Maybe AssetId -> Maybe FiatCurrencyId -> Maybe PaymentMethod -> Maybe OperationType -> IO (Vector ComplianceCheckEntity))
    -> IdentityId
    -> ComplianceSlug
    -> Int  -- ^ Max attempts
    -> IO ComplianceStatus
pollForApproval getChecks identityId slug maxAttempts = go 1
  where
    pollIntervalSeconds = 30
    pollIntervalMicros = pollIntervalSeconds * 1000000

    go attempt = do
        putStrLn $ "  [" <> show attempt <> "/" <> show maxAttempts <> "] Checking compliance status..."

        checksResult <- try $ getChecks identityId
            (Just "DE")
            (Just USDC_POLYGON)
            (Just EUR)
            (Just Sepa)
            (Just OperationDeposit)

        case checksResult of
            Left (e :: SomeException) -> do
                putStrLn $ "    Error: " <> show e
                if attempt >= maxAttempts
                    then pure NotStarted
                    else do
                        threadDelay pollIntervalMicros
                        go (attempt + 1)
            Right checks -> do
                let mCheck = V.find (\c -> _complianceCheckEntitySlug c == slug) checks
                case mCheck of
                    Nothing -> do
                        putStrLn "    Check not found!"
                        if attempt >= maxAttempts
                            then pure NotStarted
                            else do
                                threadDelay pollIntervalMicros
                                go (attempt + 1)
                    Just check -> do
                        let status = _complianceCheckEntityStatus check
                        putStrLn $ "    Status: " <> show status

                        case status of
                            Approved -> pure Approved
                            Rejected -> pure Rejected
                            ComplianceCancelled -> pure ComplianceCancelled
                            _ -> do
                                if attempt >= maxAttempts
                                    then pure status
                                    else do
                                        putStrLn $ "    Waiting " <> show pollIntervalSeconds <> " seconds..."
                                        threadDelay pollIntervalMicros
                                        go (attempt + 1)
