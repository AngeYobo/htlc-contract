module Htlc where

import Data.Map qualified as Map
import Jambhala.Plutus
import Jambhala.Utils
import Plutus.Contract ()
import Plutus.V2.Ledger.Api

-- Definition of Synonyms
type SecretHash = BuiltinByteString
type Timeout = POSIXTime
type OwnerPKH = PubKeyHash
type Secret = BuiltinByteString
type FeeAddress = PaymentPubKeyHash

-- Data type definitions
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

-- Validator function
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

-- Helper functions
{-# INLINEABLE deadlineNotPassed #-}
deadlineNotPassed :: Timeout -> ScriptContext -> Bool
deadlineNotPassed timeout ctx = getLowerBound (scriptContextTxInfo ctx) #<= timeout

{-# INLINEABLE deadlinePassed #-}
deadlinePassed :: Timeout -> ScriptContext -> Bool
deadlinePassed timeout ctx = getLowerBound (scriptContextTxInfo ctx) #> timeout

teamAddress :: FeeAddress
teamAddress = PaymentPubKeyHash "b38dbf7c2212fc77c637676a3ca1d269cf01f2ec6287d75bb62d1a07"
{-# INLINEABLE teamAddress #-}

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


{-# INLINEABLE untypedHTLC #-}
untypedHTLC :: FeeAddress -> OwnerPKH -> UntypedValidator
untypedHTLC teamAddr ownerPkh = mkUntypedValidator (validatorHTLC teamAddr ownerPkh)

-- Pre-Compilation 
type HTLCValidator = ValidatorContract "jamb-swapTest"

compileHTLCValidator :: FeeAddress -> OwnerPKH -> HTLCValidator
compileHTLCValidator feeAddr ownerPkh =
  mkValidatorContract
    ( $$(compile [||untypedHTLC||])
        `applyCode` liftCode feeAddr
        `applyCode` liftCode ownerPkh
    )

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

-- Define a mock POSIX time for testing
mockPOSIXTime :: POSIXTime
mockPOSIXTime = 1596059591000

mockPOSIXTime2 :: POSIXTime
mockPOSIXTime2 = 1596059591000

-- Export to Jambhala
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

-- Emulator test
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

-- Helper function to extract the OwnerPkh ( sender) and deadline for debugging purpose
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