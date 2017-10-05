{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Carbon.Aggregator.BufferSpec (spec) where

import Data.IORef

import Carbon
import Carbon.Aggregator
import Carbon.Aggregator.Buffer
import Carbon.TestExtensions ()

import qualified Data.Map as Map

import Test.Hspec

deriving instance Show MetricBuffers
deriving instance Eq MetricBuffers

spec :: Spec
spec = do
    describe "empty MetricBuffers" $ do
        it "don't emit events" $ do
            metricBuf <- bufferFor "metric.path" 10 Sum
            computeAggregatedIO 1 100 metricBuf `shouldReturn` []

    describe "MetricBuffers" $ do
        it "emits events" $ do
            metricBuf <- makeMetricBuf
            computeAggregatedIO 5 112 metricBuf `shouldReturn` [DataPoint 100 42]

        it "drops outdated intervals" $ do
            metricBuf <- makeMetricBuf
            -- Shouldn't return any DataPoints for outdated intervals
            computeAggregatedIO 1 1000 metricBuf `shouldReturn` []

            -- Should also drop outdated intervals to avoid further processing
            intervalMetrics <- readIORef $ intervalBuffers metricBuf
            intervalMetrics `shouldSatisfy` Map.null

        it "doesn't emit duplicates" $ do
            metricBuf <- makeMetricBuf
            computeAggregatedIO 5 112 metricBuf `shouldReturn` [DataPoint 100 42]
            -- No new DataPoints added - nothing to emit
            computeAggregatedIO 5 120 metricBuf `shouldReturn` []

        it "emits aggregated interval if data added" $ do
            metricBuf <- makeMetricBuf
            computeAggregatedIO 5 112 metricBuf
            appendDataPoint metricBuf $ DataPoint 103 24
            computeAggregatedIO 5 122 metricBuf `shouldReturn` [DataPoint 100 66]

        it "emits data point per interval" $ do
            metricBuf <- makeMetricBuf
            appendDataPoint metricBuf $ DataPoint 110 24
            computeAggregatedIO 5 112 metricBuf `shouldReturn` [DataPoint 100 42, DataPoint 110 24]

        where
            makeMetricBuf :: IO MetricBuffers
            makeMetricBuf = do
                metricBuf <- bufferFor "metric.path" 10 Sum
                appendDataPoint metricBuf $ DataPoint 102 42
                return metricBuf
