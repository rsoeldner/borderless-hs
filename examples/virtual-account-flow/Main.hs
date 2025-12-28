{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Virtual account deposit flow application
--
-- This application walks through the virtual account creation flow.
-- Virtual accounts provide dedicated banking details that automatically
-- convert incoming fiat to crypto - no separate deposit transaction needed.
--
-- For standard deposits (per-transaction banking details), see standard-deposit-flow.
--
-- Usage:
--   export BORDERLESS_CLIENT_ID=your_client_id
--   export BORDERLESS_CLIENT_SECRET=your_client_secret
--   cabal run virtual-account-flow
--
module Main where

import Borderless.V1

import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.Text (Text)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V

main :: IO ()
main = do
    putStrLn "=== Borderless Virtual Account Flow ==="
    putStrLn ""

    -- Get credentials from environment
    mClientId <- fmap T.pack <$> lookupEnv "BORDERLESS_CLIENT_ID"
    mClientSecret <- fmap T.pack <$> lookupEnv "BORDERLESS_CLIENT_SECRET"

    case (mClientId, mClientSecret) of
        (Just clientId, Just clientSecret) -> do
            runVirtualAccountFlow clientId clientSecret
        _ -> do
            putStrLn "Error: Missing credentials"
            putStrLn "Please set BORDERLESS_CLIENT_ID and BORDERLESS_CLIENT_SECRET"
            exitFailure

runVirtualAccountFlow :: Text -> Text -> IO ()
runVirtualAccountFlow clientId clientSecret = do
    let config = defaultConfig
            { _borderlessConfigClientId = clientId
            , _borderlessConfigClientSecret = clientSecret
            , _borderlessConfigEnvironment = Sandbox
            }

    withBorderless config $ \methods@Methods{..} -> do
        -- Step 1: Create Identity
        putStrLn "Step 1: Creating personal identity..."
        identity <- createTestIdentity methods
        let identityId = _identityEntityId identity
        TIO.putStrLn $ "  Created identity: " <> unIdentityId identityId
        putStrLn ""

        -- Step 2: Get compliance checks
        putStrLn "Step 2: Getting compliance checks for EUR/SEPA deposits..."
        complianceChecks <- getIdentityComplianceChecks identityId
            (Just "DE")
            (Just USDC_POLYGON)
            (Just EUR)
            (Just Sepa)
            (Just OperationDeposit)

        when (V.null complianceChecks) $ do
            putStrLn "  Error: No compliance checks available"
            exitFailure

        let check = V.head complianceChecks
            slug = _complianceCheckEntitySlug check
        TIO.putStrLn $ "  Found compliance check: " <> unComplianceSlug slug
        TIO.putStrLn $ "  Status: " <> T.pack (show $ _complianceCheckEntityStatus check)
        putStrLn ""

        -- Step 3: Get requirements (includes ToS link)
        putStrLn "Step 3: Getting compliance requirements..."
        requirements <- getComplianceRequirements identityId slug

        -- Print missing fields
        let missingFields = _complianceRequirementsEntityMissingFields requirements
        when (not $ V.null missingFields) $ do
            putStrLn "  Missing fields:"
            mapM_ (\f -> TIO.putStrLn $ "    - " <> f) missingFields

        -- Print required documents
        let docs = _complianceRequirementsEntityRequiredDocuments requirements
        when (not $ V.null docs) $ do
            putStrLn "  Required documents:"
            mapM_ (\d -> TIO.putStrLn $ "    - " <> T.pack (show d)) docs

        -- Get ToS link
        let mTosLink = do
                extras <- _complianceRequirementsEntityRequiredExtras requirements
                tos <- _requiredExtrasAcceptTermsOfService extras
                _termsOfServiceRequirementLink tos

        case mTosLink of
            Just tosLink -> do
                putStrLn ""
                putStrLn "=========================================="
                putStrLn "TERMS OF SERVICE ACCEPTANCE REQUIRED"
                putStrLn "=========================================="
                putStrLn ""
                TIO.putStrLn $ "Please open this link in your browser and accept the Terms of Service:"
                putStrLn ""
                TIO.putStrLn $ "  " <> tosLink
                putStrLn ""
                putStrLn "After accepting, press ENTER to continue..."
                hFlush stdout
                _ <- getLine
                putStrLn ""
            Nothing -> do
                putStrLn "  No Terms of Service link found (may already be accepted)"
                putStrLn ""

        -- Step 4: Upload documents (interactive)
        putStrLn "Step 4: Document upload..."
        putStrLn ""
        putStrLn "The API requires the following documents:"
        putStrLn "  1. ProofOfAddress (utility bill, bank statement, etc.)"
        putStrLn "  2. One of: Passport, DriverLicense, NationalId, ResidencePermit"
        putStrLn ""
        putStrLn "Enter the path to your Passport image (or press ENTER for test image):"
        hFlush stdout
        passportPath <- getLine

        do
            putStrLn "  Uploading passport..."
            base64Data <- if null passportPath
                then do
                    putStrLn "  Using test image..."
                    pure testImageBase64
                else do
                    mBase64 <- readFileAsBase64 passportPath
                    case mBase64 of
                        Nothing -> do
                            putStrLn "  Error: Could not read file, using test image..."
                            pure testImageBase64
                        Just b -> pure b
            let docDto = _IdentityDocumentCreateDto
                    { _identityDocumentCreateDtoIssuingCountry = "DE"
                    , _identityDocumentCreateDtoType_ = Passport
                    , _identityDocumentCreateDtoIssuedDate = "2020-01-01"
                    , _identityDocumentCreateDtoExpiryDate = Just "2030-01-01"
                    , _identityDocumentCreateDtoImageFront = Just base64Data
                    }
            result <- try $ uploadDocument identityId docDto
            case result of
                Left (e :: SomeException) -> putStrLn $ "  Error uploading: " <> show e
                Right _ -> putStrLn "  Passport uploaded successfully"

        putStrLn ""
        putStrLn "Enter the path to your ProofOfAddress image (or press ENTER for test image):"
        hFlush stdout
        poaPath <- getLine

        do
            putStrLn "  Uploading proof of address..."
            base64Data <- if null poaPath
                then do
                    putStrLn "  Using test image..."
                    pure testImageBase64
                else do
                    mBase64 <- readFileAsBase64 poaPath
                    case mBase64 of
                        Nothing -> do
                            putStrLn "  Error: Could not read file, using test image..."
                            pure testImageBase64
                        Just b -> pure b
            let docDto = _IdentityDocumentCreateDto
                    { _identityDocumentCreateDtoIssuingCountry = "DE"
                    , _identityDocumentCreateDtoType_ = ProofOfAddress
                    , _identityDocumentCreateDtoIssuedDate = "2024-01-01"
                    , _identityDocumentCreateDtoImageFront = Just base64Data
                    }
            result <- try $ uploadDocument identityId docDto
            case result of
                Left (e :: SomeException) -> putStrLn $ "  Error uploading: " <> show e
                Right _ -> putStrLn "  Proof of address uploaded successfully"

        putStrLn ""

        -- Step 5: Update identity with taxId
        putStrLn "Step 5: Tax ID..."
        putStrLn "Enter your tax ID (or press ENTER to skip):"
        hFlush stdout
        taxIdInput <- getLine

        when (not $ null taxIdInput) $ do
            putStrLn "  Updating identity with tax ID..."
            let updateDto = _PersonalIdentityUpdateDto
                    { _personalIdentityUpdateDtoTaxId = Just (T.pack taxIdInput)
                    }
            result <- try $ updatePersonalIdentity identityId updateDto
            case result of
                Left (e :: SomeException) -> putStrLn $ "  Error updating: " <> show e
                Right _ -> putStrLn "  Tax ID updated successfully"

        putStrLn ""

        -- Re-check requirements to see what's still missing
        putStrLn "Step 5b: Re-checking compliance requirements..."
        requirementsAfter <- getComplianceRequirements identityId slug

        let missingFieldsAfter = _complianceRequirementsEntityMissingFields requirementsAfter
        if V.null missingFieldsAfter
            then putStrLn "  All required fields are complete!"
            else do
                putStrLn "  Still missing fields:"
                mapM_ (\f -> TIO.putStrLn $ "    - " <> f) missingFieldsAfter

        let docsAfter = _complianceRequirementsEntityRequiredDocuments requirementsAfter
        if V.null docsAfter
            then putStrLn "  All required documents are uploaded!"
            else do
                putStrLn "  Still missing documents:"
                mapM_ (\d -> TIO.putStrLn $ "    - " <> T.pack (show d)) docsAfter

        putStrLn ""

        -- Pause for manual steps before starting compliance check
        putStrLn "==========================================="
        putStrLn "MANUAL STEPS REQUIRED BEFORE CONTINUING"
        putStrLn "==========================================="
        putStrLn ""
        putStrLn "Before starting the compliance check, please ensure:"
        putStrLn "  1. You have accepted the Terms of Service (see link above)"
        putStrLn "  2. All missing fields and documents are addressed"
        putStrLn "  3. Complete any required identity validation in the dashboard"
        putStrLn ""
        TIO.putStrLn $ "Identity ID: " <> unIdentityId identityId
        TIO.putStrLn $ "Compliance slug: " <> unComplianceSlug slug
        putStrLn ""
        putStrLn "Press ENTER when ready to start compliance check..."
        hFlush stdout
        _ <- getLine
        putStrLn ""

        -- Step 6: Start compliance check
        putStrLn "Step 6: Starting compliance check..."
        idempKey <- newIdempotencyKey
        result <- try $ startComplianceCheck identityId slug idempKey
        case result of
            Left (e :: SomeException) -> do
                putStrLn "  Error starting compliance check:"
                putStrLn $ "  " <> show e
                putStrLn ""
                putStrLn "  This likely means documents or taxId are still missing."
                putStrLn "  Please complete the requirements and try again."
            Right startedCheck -> do
                TIO.putStrLn $ "  Compliance check started: " <> T.pack (show $ _complianceCheckStartedEntityStatus startedCheck)
                putStrLn ""

                -- Step 7: Poll for compliance approval
                putStrLn "Step 7: Waiting for compliance approval..."
                putStrLn "  Polling every 30 seconds (max 20 attempts = 10 minutes)..."
                putStrLn ""
                finalStatus <- pollForApproval getIdentityComplianceChecks identityId slug 20
                putStrLn ""

                -- Step 8: Create Account
                putStrLn "Step 8: Creating account..."
                account <- createTestAccount methods identityId
                let accountId = _accountEntityId account
                TIO.putStrLn $ "  Created account: " <> unAccountId accountId
                putStrLn ""

                -- Step 9: Create Virtual Account
                if finalStatus == Approved
                    then do
                        putStrLn "Step 9: Creating virtual account..."
                        vaIdempKey <- newIdempotencyKey
                        let vaDto = _VirtualAccountCreateDto
                                { _virtualAccountCreateDtoFiat = EUR
                                , _virtualAccountCreateDtoCountry = CountryCode "DE"
                                , _virtualAccountCreateDtoAsset = USDC_POLYGON
                                , _virtualAccountCreateDtoCounterPartyIdentityId = Nothing
                                , _virtualAccountCreateDtoDeveloperFeePercent = Nothing
                                }
                        vaResult <- try $ createVirtualAccount accountId vaIdempKey vaDto
                        case vaResult of
                            Left (e :: SomeException) -> do
                                putStrLn $ "  Error creating virtual account: " <> show e
                            Right va -> do
                                let vaId = _virtualAccountEntityId va
                                TIO.putStrLn $ "  Virtual Account ID: " <> unVirtualAccountId vaId
                                TIO.putStrLn $ "  Status: " <> T.pack (show $ _virtualAccountEntityStatus va)
                                TIO.putStrLn $ "  Asset: " <> T.pack (show $ _virtualAccountEntityAsset va)
                                TIO.putStrLn $ "  Fiat: " <> T.pack (show $ _virtualAccountEntityFiat va)
                                putStrLn ""

                                -- Print banking instructions
                                case _virtualAccountEntityInstructions va of
                                    Just instr -> do
                                        putStrLn "  =========================================="
                                        putStrLn "  VIRTUAL ACCOUNT BANKING DETAILS"
                                        putStrLn "  =========================================="
                                        putStrLn "  Send EUR to these bank details."
                                        putStrLn "  Funds will be automatically converted to USDC."
                                        putStrLn ""
                                        putStrLn "  Banking Details (JSON):"
                                        print instr
                                    Nothing -> do
                                        putStrLn "  Banking instructions not yet available."
                                        putStrLn "  The virtual account may still be provisioning."
                                        TIO.putStrLn $ "  Virtual Account ID: " <> unVirtualAccountId vaId

                                -- Check for failure reason
                                case _virtualAccountEntityFailureReason va of
                                    Just reason -> do
                                        putStrLn ""
                                        putStrLn "  Warning - Failure reason:"
                                        print reason
                                    Nothing -> pure ()
                    else do
                        putStrLn "Step 9: Skipping virtual account creation"
                        putStrLn "  Compliance check is not yet approved."
                        TIO.putStrLn $ "  Final status: " <> T.pack (show finalStatus)
                        putStrLn "  The compliance check may need manual review."

        putStrLn ""
        putStrLn "=== Done ==="

-- | Poll for compliance approval
-- Returns the final status after approval or timeout
pollForApproval
    :: (IdentityId -> Maybe CountryCode -> Maybe AssetId -> Maybe FiatCurrencyId -> Maybe PaymentMethod -> Maybe OperationType -> IO (V.Vector ComplianceCheckEntity))
    -> IdentityId
    -> ComplianceSlug
    -> Int  -- ^ Max attempts
    -> IO ComplianceStatus
pollForApproval getChecks identityId slug maxAttempts = go 1
  where
    pollIntervalSeconds = 30
    pollIntervalMicros = pollIntervalSeconds * 1000000

    go attempt = do
        putStr $ "  [" <> show attempt <> "/" <> show maxAttempts <> "] Checking status... "
        hFlush stdout

        checksResult <- try $ getChecks identityId
            (Just "DE")
            (Just USDC_POLYGON)
            (Just EUR)
            (Just Sepa)
            (Just OperationDeposit)

        case checksResult of
            Left (e :: SomeException) -> do
                putStrLn $ "Error: " <> show e
                if attempt >= maxAttempts
                    then pure NotStarted  -- Return a non-approved status on error
                    else do
                        threadDelay pollIntervalMicros
                        go (attempt + 1)
            Right checks -> do
                -- Find the check with matching slug
                let mCheck = V.find (\c -> _complianceCheckEntitySlug c == slug) checks
                case mCheck of
                    Nothing -> do
                        putStrLn "Check not found!"
                        if attempt >= maxAttempts
                            then pure NotStarted
                            else do
                                threadDelay pollIntervalMicros
                                go (attempt + 1)
                    Just check -> do
                        let status = _complianceCheckEntityStatus check
                        TIO.putStrLn $ T.pack (show status)

                        case status of
                            Approved -> do
                                putStrLn "  Compliance check APPROVED!"
                                pure Approved
                            Rejected -> do
                                putStrLn "  Compliance check REJECTED."
                                pure Rejected
                            ComplianceCancelled -> do
                                putStrLn "  Compliance check CANCELLED."
                                pure ComplianceCancelled
                            _ -> do
                                -- Still in progress
                                if attempt >= maxAttempts
                                    then do
                                        putStrLn $ "  Max attempts reached. Final status: " <> show status
                                        pure status
                                    else do
                                        putStrLn $ "  Waiting " <> show pollIntervalSeconds <> " seconds..."
                                        threadDelay pollIntervalMicros
                                        go (attempt + 1)

-- | Read a file and encode it as base64 with MIME type prefix
readFileAsBase64 :: FilePath -> IO (Maybe Text)
readFileAsBase64 path = do
    result <- try $ BS.readFile path
    case result of
        Left (_ :: SomeException) -> pure Nothing
        Right content -> do
            let base64 = Base64.encode content
                mimeType = guessMimeType path
                dataUri = "data:" <> mimeType <> ";base64," <> TE.decodeUtf8 base64
            pure (Just dataUri)

-- | Guess MIME type from file extension
guessMimeType :: FilePath -> Text
guessMimeType path
    | ".jpg" `T.isSuffixOf` T.pack path  = "image/jpeg"
    | ".jpeg" `T.isSuffixOf` T.pack path = "image/jpeg"
    | ".png" `T.isSuffixOf` T.pack path  = "image/png"
    | ".pdf" `T.isSuffixOf` T.pack path  = "application/pdf"
    | otherwise = "application/octet-stream"

-- | Minimal 1x1 pixel PNG image encoded as base64 data URI
-- This is used as a fallback for test document uploads
testImageBase64 :: Text
testImageBase64 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="

-- | Create a test personal identity
createTestIdentity :: Methods -> IO IdentityEntity
createTestIdentity Methods{..} = do
    uniqueId <- newIdempotencyKey
    let uniqueEmail = "test+" <> T.pack (show (unIdempotencyKey uniqueId)) <> "@example.com"
    let dto = _PersonalIdentityCreateDto
            { _personalIdentityCreateDtoFirstName = "Test"
            , _personalIdentityCreateDtoLastName = "User"
            , _personalIdentityCreateDtoDateOfBirth = "1990-01-15"
            , _personalIdentityCreateDtoEmail = Just uniqueEmail
            , _personalIdentityCreateDtoPhone = Just "+49123456789"
            , _personalIdentityCreateDtoAddress = Just PostalAddressDto
                { _postalAddressDtoStreet1 = "Musterstrasse 123"
                , _postalAddressDtoStreet2 = Nothing
                , _postalAddressDtoCity = "Berlin"
                , _postalAddressDtoState = Just "Berlin"
                , _postalAddressDtoCountry = "DE"
                , _postalAddressDtoPostalCode = "10115"
                }
            }
    createPersonalIdentity dto

-- | Create a test account
createTestAccount :: Methods -> IdentityId -> IO AccountEntity
createTestAccount Methods{..} identityId = do
    let dto = _AccountCreateDto
            { _accountCreateDtoName = "[E2E Auto-Generated] Virtual Account Test"
            , _accountCreateDtoIdentityId = identityId
            }
    createAccount dto
