{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V1.Ledger.Interval (contains, to, from)
import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext (scriptContextTxInfo), TxInfo (txInfoValidRange), Validator,
                                       mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..), traceIfFalse, ($), (&&), (||), (+))
import           Utilities            (wrap)
import Data.Aeson (Value(Bool))

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator _dat () _ctx = 
    (signedByBeneficiary1 && deadlineNotReached)
    || (signedByBeneficiary2 && deadlineReached)
        where
            info :: TxInfo
            info = scriptContextTxInfo _ctx

            signedByBeneficiary1 :: Bool
            signedByBeneficiary1 = txSignedBy info $ beneficiary1 _dat

            signedByBeneficiary2 :: Bool
            signedByBeneficiary2 = txSignedBy info $ beneficiary2 _dat

            deadlineNotReached :: Bool
            deadlineNotReached = contains (to $ deadline _dat) $ txInfoValidRange info

            deadlineReached :: Bool
            deadlineReached = contains (from $ 1 + deadline _dat) $ txInfoValidRange info

        -- condition 1: beneficiary1 has signed & current slot is before or at the deadline
        -- beneficiary has signed = txSignedBy info $ beneficiary1 dat
        -- txInfoValidRage to deadline
        -- signedByBeneficiary1 = txSignedBy info $ beneficiary1 dat && contains (to $ deadline dat) $ txInfoValidRange info
        -- OR
        -- condition 2: beneficiary2 has signed & deadline has passed
        -- signedByBeneficiary2 = txSignedBy info $ beneficiary2 dat && contains (from & deadline dat) $ txInfoValidRange info
        

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrap mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
