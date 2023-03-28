{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Homework2 where

import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Interval
import           PlutusTx             (applyCode, compile, liftCode)
import           PlutusTx.Prelude     (Bool (..), (.), traceIfFalse, (&&))
import           Utilities            (wrap, writeValidatorToFile)
import Prelude                   (IO)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkParameterizedVestingValidator #-}
-- This should validate if the transaction has a signature from the parameterized beneficiary and the deadline has passed.
mkParameterizedVestingValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator beneficiary deadline () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                                                              traceIfFalse "deadline not reached" deadlineReached
    where 
        info :: TxInfo
        info = scriptContextTxInfo ctx

        txValidRange :: POSIXTimeRange
        txValidRange = txInfoValidRange info

        signedByBeneficiary :: Bool
        signedByBeneficiary = txSignedBy info beneficiary

        deadlineReached :: Bool
        deadlineReached = contains (from deadline) txValidRange

{-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator = wrap . mkParameterizedVestingValidator

validator :: PubKeyHash -> Validator
validator beneficiary = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode beneficiary)
