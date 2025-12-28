{-# LANGUAGE OverloadedStrings #-}

module Main where

import Borderless.V1

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified System.Environment as Environment

main :: IO ()
main = do
    -- Read credentials from environment variables
    cid <- T.pack <$> Environment.getEnv "BORDERLESS_CLIENT_ID"
    csec <- T.pack <$> Environment.getEnv "BORDERLESS_CLIENT_SECRET"

    putStrLn "Starting Borderless API client..."

    -- Configure the client
    let config = defaultConfig
            { _borderlessConfigClientId = cid
            , _borderlessConfigClientSecret = csec
            , _borderlessConfigEnvironment = Sandbox  -- Use Sandbox for testing
            }

    -- Use the client with automatic token management
    withBorderless config $ \methods -> do
        putStrLn "Successfully authenticated!"

        -- Example: List users
        putStrLn "\n--- Listing Users ---"
        userPage <- getUsers methods Nothing (Just 10)
        let users = _pageResponseData_ userPage
        putStrLn $ "Found " <> show (V.length users) <> " users"
        V.forM_ users $ \user -> do
            let name = fromMaybe "" (_userEntityFirstName user)
                    <> " "
                    <> fromMaybe "" (_userEntityLastName user)
            TIO.putStrLn $ "  - " <> _userEntityEmail user
                <> " (" <> T.strip name <> ")"
                <> " [" <> T.pack (show $ _userEntityRole user) <> "]"

        -- Example: List identities
        putStrLn "\n--- Listing Identities ---"
        identityPage <- getIdentities methods Nothing (Just 10)
        let identities = _pageResponseData_ identityPage
        putStrLn $ "Found " <> show (V.length identities) <> " identities"
        V.forM_ identities $ \identity -> do
            let idType = _identityEntityType_ identity
                status = _identityEntityStatus identity
                name = case _identityEntityPersonalData identity of
                    Just pd -> _personalDataEntityFirstName pd <> " " <> _personalDataEntityLastName pd
                    Nothing -> case _identityEntityBusinessData identity of
                        Just bd -> _businessDataEntityName bd
                        Nothing -> "(unknown)"
            TIO.putStrLn $ "  - " <> unIdentityId (_identityEntityId identity)
                <> ": " <> name
                <> " [" <> T.pack (show idType) <> ", " <> T.pack (show status) <> "]"

        -- Example: List accounts
        putStrLn "\n--- Listing Accounts ---"
        accountPage <- getAccounts methods Nothing Nothing (Just 10)
        let accounts = _pageResponseData_ accountPage
        putStrLn $ "Found " <> show (V.length accounts) <> " accounts"
        V.forM_ accounts $ \account -> do
            let numAddresses = V.length (_accountEntityAddresses account)
            TIO.putStrLn $ "  - " <> unAccountId (_accountEntityId account)
                <> ": " <> _accountEntityName account
                <> " (" <> T.pack (show numAddresses) <> " assets)"

        -- Example: List transactions
        putStrLn "\n--- Listing Transactions ---"
        txPage <- getTransactions methods Nothing Nothing Nothing Nothing Nothing (Just 10)
        let txs = _pageResponseData_ txPage
        putStrLn $ "Found " <> show (V.length txs) <> " transactions"
        V.forM_ txs $ \tx -> do
            let txId = _transactionEntityId tx
                txType = _transactionEntityType_ tx
                txStatus = _transactionEntityStatus tx
                src = _transactionEntitySource tx
                dest = _transactionEntityDestination tx
                srcInfo = case (_transactionSourceEntityFiatCurrency src, _transactionSourceEntityFiatAmount src) of
                    (Just cur, Just amt) -> unDecimalNumber amt <> " " <> T.pack (show cur)
                    _ -> case (_transactionSourceEntityAsset src, _transactionSourceEntityAmount src) of
                        (Just asset, Just amt) -> unDecimalNumber amt <> " " <> T.pack (show asset)
                        _ -> "?"
                destInfo = case (_transactionDestinationEntityAsset dest, _transactionDestinationEntityAmount dest) of
                    (Just asset, Just amt) -> unDecimalNumber amt <> " " <> T.pack (show asset)
                    _ -> case (_transactionDestinationEntityFiatCurrency dest, _transactionDestinationEntityFiatAmount dest) of
                        (Just cur, Just amt) -> unDecimalNumber amt <> " " <> T.pack (show cur)
                        _ -> "?"
            TIO.putStrLn $ "  - " <> unTransactionId txId
                <> " [" <> T.pack (show txType) <> "] "
                <> srcInfo <> " -> " <> destInfo
                <> " (" <> T.pack (show txStatus) <> ")"

        putStrLn "\nDone!"
