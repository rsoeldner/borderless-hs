# borderless

> **Work in Progress** - Type-safe Haskell bindings for the [Borderless.xyz](https://borderless.xyz) API.

## Status

This library is under active development. Currently tested flows:

| Flow | Status |
|------|--------|
| Deposits (EUR/SEPA) | Tested |
| Virtual Accounts | Tested |
| Compliance/KYC | Tested |
| Withdrawals | Not tested |
| Swaps | Not tested |
| Webhooks | Not tested |

**API Coverage**: ~20% of endpoints have E2E tests.

## Quick Start

```haskell
import Borderless.V1

main :: IO ()
main = do
    let config = defaultConfig
            { _borderlessConfigClientId = "your-client-id"
            , _borderlessConfigClientSecret = "your-client-secret"
            , _borderlessConfigEnvironment = Sandbox
            }

    withBorderless config $ \Methods{..} -> do
        accounts <- getAccounts Nothing Nothing (Just 10)
        print accounts
```

## Examples

```bash
export BORDERLESS_CLIENT_ID="your-client-id"
export BORDERLESS_CLIENT_SECRET="your-client-secret"

# List users, identities, accounts, transactions
cabal run borderless-example

# Full deposit flow with standard account
cabal run standard-deposit-flow

# Full deposit flow with virtual account
cabal run virtual-account-flow
```
