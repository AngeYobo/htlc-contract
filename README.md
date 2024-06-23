## Haskell HTLC Contract in Jambhala Framework

### Introduction

This is an HTLC (Hash Time-Locked Contract) implemented using the Jambhala framework. The contract ensures secure and conditional payments based on time constraints and cryptographic hashes. Below, we explain the contract's structure, functionality, and how it integrates with the Jambhala framework.

### Module and Imports

```haskell
module Htlc where

import Data.Map qualified as Map
import Jambhala.Plutus
import Jambhala.Utils
import Plutus.V2.Ledger.Api
```

**Explanation**:
- **Data.Map**: Provides functions for working with maps, used for UTXO management.
- **Jambhala.Plutus**: Aggregates common Plutus types and functions for contract development.
- **Jambhala.Utils**: Offers utilities and framework-specific types to simplify contract creation.
- **Plutus.V2.Ledger.Api**: Contains core Plutus ledger types and functions.

### Definition of Synonyms

```haskell
type SecretHash = BuiltinByteString
type Timeout = POSIXTime
type OwnerPKH = PubKeyHash
type Secret = BuiltinByteString
type FeeAddress = PaymentPubKeyHash
```

**Explanation**:
Defines type synonyms for clarity:
- **SecretHash**: Hash of the secret.
- **Timeout**: Expiration time for the contract.
- **OwnerPKH**: Public key hash of the owner.
- **Secret**: Secret value.
- **FeeAddress**: Payment public key hash for the fee.

### Data Type Definitions

```haskell
data HTLCDatum = HTLCDatum
  { htlcSecretHash :: SecretHash
  , htlcTimeout :: Timeout
  , htlcOwner :: OwnerPKH
  }
  deriving (Show, Generic, FromJSON, ToJSON)

makeIsDataIndexed ''HTLCDatum [('HTLCDatum, 0)]
makeLift ''HTLCDatum

data HTLCRedeemer = Claim Secret | Refund
  deriving (Show, Generic, FromJSON, ToJSON)

makeIsDataIndexed ''HTLCRedeemer [('Claim, 0), ('Refund, 1)]
makeLift ''HTLCRedeemer
```

**Explanation**:
- **HTLCDatum**: Holds the secret hash, timeout, and owner public key hash.
- **HTLCRedeemer**: Defines actions (Claim with a secret, Refund).

### Validator Function

```haskell
{-# INLINEABLE validatorHTLC #-}
validatorHTLC :: FeeAddress -> OwnerPKH -> HTLCDatum -> HTLCRedeemer -> ScriptContext -> Bool
validatorHTLC teamAddr ownerPkh datum redeemer ctx =
  case redeemer of
    Claim secret ->
      traceIfFalse "Secret does not match" (secret #== htlcSecretHash datum)
        && traceIfFalse "Deadline has passed, claim not allowed" (deadlineNotPassed (htlcTimeout datum) ctx)
        && traceIfFalse "Paid fee is insufficient or not to the team address" (feePaid ctx teamAddr feeValue)
    Refund ->
      traceIfFalse "Signature failure: owner not signer for refund" (txSignedBy (scriptContextTxInfo ctx) ownerPkh)
        && traceIfFalse "Deadline not passed, refund not allowed" (deadlinePassed (htlcTimeout datum) ctx)
        && traceIfFalse "Fee not paid or insufficient for refund" (feePaid ctx teamAddr feeValue)
```

**Explanation**:
- **validatorHTLC**: Core validation logic ensuring claims and refunds follow specified rules.

### Helper Functions

```haskell
{-# INLINEABLE deadlineNotPassed #-}
deadlineNotPassed :: Timeout -> ScriptContext -> Bool
deadlineNotPassed timeout ctx = getLowerBound (scriptContextTxInfo ctx) #<= timeout

{-# INLINEABLE deadlinePassed #-}
deadlinePassed :: Timeout -> ScriptContext -> Bool
deadlinePassed timeout ctx = getLowerBound (scriptContextTxInfo ctx) #> timeout
```

**Explanation**:
- **deadlineNotPassed**: Ensures the deadline hasn't passed.
- **deadlinePassed**: Checks if the deadline has passed.

### Fee Payment Validation

```haskell
teamAddress :: FeeAddress
teamAddress = PaymentPubKeyHash "b38dbf7c2212fc77c637676a3ca1d269cf01f2ec6287d75bb62d1a07"

{-# INLINEABLE makeCurrencyValue #-}
makeCurrencyValue :: CurrencySymbol -> TokenName -> Integer -> Value
makeCurrencyValue = singleton

feeValue :: Value
feeValue = makeCurrencyValue adaSymbol adaToken 1000000
{-# INLINEABLE feeValue #-}

{-# INLINEABLE feePaid #-}
feePaid :: ScriptContext -> FeeAddress -> Value -> Bool
feePaid ctx teamPKH requiredFee =
  let outputs = txInfoOutputs $ scriptContextTxInfo ctx
      correctOutput = checkFeePayment (unPaymentPubKeyHash teamPKH) requiredFee outputs
   in traceIfFalse "Fee payment check failed" correctOutput

{-# INLINEABLE checkFeePayment #-}
checkFeePayment :: PubKeyHash -> Value -> [TxOut] -> Bool
checkFeePayment teamPKH fee = pany (isValidFeeOutput teamPKH fee)

{-# INLINEABLE isValidFeeOutput #-}
isValidFeeOutput :: PubKeyHash -> Value -> TxOut -> Bool
isValidFeeOutput teamPKH fee TxOut {..} =
  case toPubKeyHash txOutAddress of
    Nothing -> False
    Just pkh -> pkh #== teamPKH && txOutValue #== fee

{-# INLINEABLE getLowerBound #-}
getLowerBound :: TxInfo -> POSIXTime
getLowerBound txInfo = case ivFrom (txInfoValidRange txInfo) of
  LowerBound (Finite time) _ -> time
  _ -> traceError "Invalid interval"
```

**Explanation**:
- **feePaid**: Verifies if the correct fee is paid to the team address.
- **checkFeePayment**: Checks if a specific output meets the fee payment conditions.
- **isValidFeeOutput**: Validates individual outputs for fee payment.
- **getLowerBound**: Retrieves the start time of the transaction validity range.

### Untyped Validator and Pre-Compilation

```haskell
{-# INLINEABLE untypedHTLC #-}
untypedHTLC :: FeeAddress -> OwnerPKH -> UntypedValidator
untypedHTLC teamAddr ownerPkh = mkUntypedValidator (validatorHTLC teamAddr ownerPkh)

type HTLCValidator = ValidatorContract "jamb-htlc"

compileHTLCValidator :: FeeAddress -> OwnerPKH -> HTLCValidator
compileHTLCValidator feeAddr ownerPkh =
  mkValidatorContract
    ( $$(compile [||untypedHTLC||])
        `applyCode` liftCode feeAddr
        `applyCode` liftCode ownerPkh
    )
```

**Explanation**:
- **untypedHTLC**: Converts the typed validator function to an untyped form for Plutus.
- **compileHTLCValidator**: Pre-compiles the untyped validator into a Jambhala contract.

### Example Datum

```haskell
mkExampleDatum :: POSIXTime -> HTLCDatum
mkExampleDatum currentTime =
  HTLCDatum
    { htlcSecretHash = sha2_256 $ stringToBuiltinByteString "validSecret"
    , htlcTimeout = currentTime
    , htlcOwner = sampleOwnerPkh
    }

sampleOwnerPkh :: OwnerPKH
sampleOwnerPkh = "3488918590d38ad30c8924fcbea76cc1388eb4c6eb60fc2972b6fa4e"

sampleTeamPkHash :: FeeAddress
sampleTeamPkHash = PaymentPubKeyHash "b38dbf7c2212fc77c637676a3ca1d269cf01f2ec6287d75bb62d1a07"

mockPOSIXTime :: POSIXTime
mockPOSIXTime = 1596059591000

mockPOSIXTime2 :: POSIXTime
mockPOSIXTime2 = 1596059591000
```

**Explanation**:
- **mkExampleDatum**: Creates a sample datum for testing.
- **sampleOwnerPkh**: Example owner public key hash.
- **sampleTeamPkHash**: Example team address.
- **mockPOSIXTime/mockPOSIXTime2**: Mock POSIX times for testing.

### Export to Jambhala

```haskell
htlc2Exports :: JambExports
htlc2Exports =
  export
    (defExports $ compileHTLCValidator sampleTeamPkHash sampleOwnerPkh)
      { dataExports =
          [ mkExampleDatum mockPOSIXTime `toJSONfile` "exampleDatum"
          , Claim (sha2_256 $ stringToBuiltinByteString "validSecret") `toJSONfile` "redeemFunds"
          , Refund `toJSONfile` "redeemRefund"
          ]
      , emulatorTest = test
      }
```

**Explanation**:
- **htlc2Exports**: Defines exports for the contract, including example data and emulator tests.

### Emulator Test

```haskell
instance ValidatorEndpoints HTLCValidator where
  data GiveParam HTLCValidator = Lock HTLCDatum Value
    deriving (Show, Generic, FromJSON, ToJSON)

  newtype GrabParam HTLCValidator = Unlock HTLCRedeemer
    deriving (Generic, FromJSON, ToJSON)

  give :: GiveParam HTLCValidator -> ContractM HTLCValidator ()
  give (Lock datum value) = do


    let appliedValidator = compileHTLCValidator teamAddress (htlcOwner datum)
    let totalValue = value <> feeValue
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor appliedValidator
        , constraints = mustPayScriptWithDatum appliedValidator datum totalValue
        }
    logInfo @String "Locked funds in HTLC contract"

  grab :: GrabParam HTLCValidator -> ContractM HTLCValidator ()
  grab (Unlock redeemer) = do
    _ <- getOwnPKH
    let appliedValidator = compileHTLCValidator teamAddress sampleOwnerPkh
    utxos <- getUtxosAt appliedValidator
    currentInterval <- getCurrentInterval
    let currentTime = case ivFrom currentInterval of
          LowerBound (Finite time) _ -> time
          _ -> traceError "Invalid interval"
    logInfo @String $ "Current time: " ++ show currentTime
    logInfo @String $ "UTXOs found: " ++ show utxos

    if Map.null utxos
      then logInfo @String "No UTXOs found at the validator address"
      else do
        let ownerPkh = extractOwnerPkh utxos
        logInfo @String $ "Owner PKH extracted: " ++ show ownerPkh
        logInfo @String $ "Redeemer: " ++ show redeemer

        logInfo @String $ "Sender PKH: " ++ show ownerPkh

        let baseConstraints =
              mconcat [mustSpendScriptOutput oref (Redeemer $ toBuiltinData redeemer) | (oref, _) <- Map.toList utxos]
                <> mustPayToPubKey teamAddress feeValue

        case redeemer of
          Refund -> do
            let deadline = extractDeadline utxos
            logInfo @String $ "Deadline: " ++ show deadline
            if currentTime >= deadline
              then
                let refundConstraints =
                      mconcat [mustSpendScriptOutput oref (Redeemer $ toBuiltinData redeemer) | (oref, _) <- Map.toList utxos]
                        <> mustPayToPubKey teamAddress feeValue
                        <> mustSign ownerPkh
                 in submitAndConfirm
                      Tx
                        { lookups = scriptLookupsFor appliedValidator `andUtxos` utxos
                        , constraints = refundConstraints
                        }
              else logInfo @String "Deadline not passed, refund not allowed"
          Claim _ ->
            let claimConstraints = baseConstraints <> mustValidateInTimeRange (fromPlutusInterval currentInterval)
             in submitAndConfirm
                  Tx
                    { lookups = scriptLookupsFor appliedValidator `andUtxos` utxos
                    , constraints = claimConstraints
                    }
    logInfo @String "Unlocked funds from HTLC contract"
```

**Explanation**:
- **give**: Locks funds in the contract.
- **grab**: Unlocks funds based on the redeemer (Claim or Refund).

### Helper Functions

```haskell
extractOwnerPkh :: Map.Map TxOutRef DecoratedTxOut -> OwnerPKH
extractOwnerPkh utxos = case Map.elems utxos of
  (decoratedTxOut : _) ->
    case getDecoratedTxOutDatum decoratedTxOut of
      Just (_, datumFromQuery) ->
        case getDatumInDatumFromQuery datumFromQuery of
          Just datum ->
            case fromBuiltinData (getDatum datum) of
              Just (HTLCDatum _ _ ownerPkh) -> ownerPkh
              Nothing -> error "Invalid datum type"
          Nothing -> error "Datum not found"
      Nothing -> error "No datum in UTXO"
  _ -> error "No UTXOs found"

extractDeadline :: Map.Map TxOutRef DecoratedTxOut -> POSIXTime
extractDeadline utxos = case Map.elems utxos of
  (decoratedTxOut : _) ->
    case getDecoratedTxOutDatum decoratedTxOut of
      Just (_, datumFromQuery) ->
        case getDatumInDatumFromQuery datumFromQuery of
          Just datum ->
            case fromBuiltinData (getDatum datum) of
              Just (HTLCDatum _ deadline _) -> deadline
              Nothing -> error "Invalid datum type"
          Nothing -> error "Datum not found"
      Nothing -> error "No datum in UTXO"
  _ -> error "No UTXOs found"
```

**Explanation**:
- **extractOwnerPkh**: Extracts the owner public key hash from UTXOs for debugging.
- **extractDeadline**: Extracts the deadline from UTXOs for debugging.

### Test

```haskell
test :: EmulatorTest
test =
  initEmulator @HTLCValidator
    2
    [ Lock (mkExampleDatum mockPOSIXTime) (lovelaceValueOf 4000000) `fromWallet` 1
    , Unlock (Claim (sha2_256 $ stringToBuiltinByteString "validSecret")) `toWallet` 2
    , Lock (mkExampleDatum mockPOSIXTime2) (lovelaceValueOf 4000000) `fromWallet` 1
    , waitUntil 100 
    , Unlock Refund `toWallet` 1
    ]

{- Final balances
Wallet 2: 103613469 lovelace
Wallet 1: 89655622 lovelace
PubKeyHash b38dbf7c2212fc77c637676a3ca1d269cf01f2ec6287d75bb62d1a07: 1000000 lovelace
Script 0a8e01c78af50fcb232612cef7cf0dfdae6cc7e71ac49d3180cb37b8: 5000000 lovelace -- Refund not possible as deadline not passed
 -}
```

**Explanation**:
- **test**: Defines an emulator test to simulate contract interactions, locking, claiming, and refunding funds based on conditions.

---

This documentation provides an overview of the HTLC contract implemented using the Jambhala framework, highlighting the key components, helper functions, and emulator tests. Future enhancements will include parameterization for dynamic fee and addresses to increase the contract's flexibility.
