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
        let metricBuf = bufferFor "metric.path" 10
        it "don't emmit events" $ do
            computeAggregated 1 100 metricBuf `shouldBe` Nothing

    describe "MetricBuffers" $ do
        let metricBufEmpty = bufferFor "metric.path" 10
        let metricBuf = appendDataPoint metricBufEmpty DataPoint { timestamp = 102, value = 42 }

        it "emmits events" $ do
            let Just modResult = computeAggregated 5 112 metricBuf
            emittedEvents modResult `shouldBe` [("metric.path", DataPoint 100 42)]

        it "drops outdated intervals" $ do
            let Just modResult = computeAggregated 1 1000 metricBuf
            metricBuffers modResult `shouldBe` metricBufEmpty
