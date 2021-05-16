{-# LANGUAGE TypeApplications #-}

module Spec.TimeSlotSpec where

import           Data.List                 (sort)
import           Hedgehog                  (Property, forAll, property)
import qualified Hedgehog
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range
import qualified Plutus.V1.Ledger.Interval as Interval
import           Plutus.V1.Ledger.Slot
import           Plutus.V1.Ledger.Time     (POSIXTime (POSIXTime))
import           Plutus.V1.Ledger.TimeSlot (posixTimeRangeToSlotRange, slotRangeToPOSIXTimeRange, slotToPOSIXTime)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog       (testProperty)

initialSlotTime :: TestTree
initialSlotTime = do
  testCase "Initial slot to time" $
    assertBool "should be equal to Shelley launch date" $ slotToPOSIXTime (Slot 0) == POSIXTime 1596059091

inverseProp :: Property
inverseProp = property $ do
  [b, e] <- forAll $ sort <$> traverse (const $ Gen.integral (fromIntegral <$> Range.linearBounded @Int)) [1..2 :: Integer]
  let slotRange = Interval.interval (Slot b) (Slot e)
  Hedgehog.assert $ slotRange == posixTimeRangeToSlotRange (slotRangeToPOSIXTimeRange slotRange)

tests :: TestTree
tests =
  testGroup
    "plutus-ledger-api-timeslot"
    [ initialSlotTime
    , testProperty " inverse property" inverseProp
    ]
