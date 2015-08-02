{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Carbon.Aggregator.ProcessorSpec (spec) where

import Carbon
import Carbon.Aggregator (AggregationMethod(..))
import Carbon.Aggregator.Rules (Rule(..))
import Carbon.Aggregator.Buffer (MetricBuffers(..))
import Carbon.Aggregator.Processor
import Carbon.TestExtensions()

import Control.Concurrent.STM

import Test.Hspec

deriving instance Show MetricBuffers
deriving instance Eq MetricBuffers

spec :: Spec
spec = do
    describe "collectAggregated" $ do

        it "works with empty buffer manager" $ do
            let bm = newBuffersManager
            let (metrics, bm') = collectAggregated 5 1000 bm
            metrics `shouldBe` []
            bm' `shouldBe` bm

        it "works with non-empty buffer manager" $ do
            let rules = [Rule "metric" "metric-sum" Sum 10]
            tbm <- newTVarIO newBuffersManager
            metrics <- atomically $ do
                _ <- processAggregateT rules tbm $ metricTuple "metric" 1001 42.0
                _ <- processAggregateT rules tbm $ metricTuple "metric" 1002 24.0
                collectAggregatedT 5 1000 tbm
            metrics `shouldBe` [metricTuple "metric-sum" 1000 66.0]
