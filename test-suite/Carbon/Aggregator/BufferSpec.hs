{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Carbon.Aggregator.BufferSpec (spec) where

import Carbon.Aggregator.Buffer

import Test.Hspec

deriving instance Show MetricBuffers
deriving instance Eq MetricBuffers
deriving instance Show ModificationResult
deriving instance Eq ModificationResult
deriving instance Show DataPoint
deriving instance Eq DataPoint

spec :: Spec
spec = do
    describe "empty MetricBuffers" $ do
        it "don't emmit events" $ do
            let metricBuf = bufferFor "metric.path" 10
            computeAggregated 1 100 metricBuf `shouldBe` Nothing
