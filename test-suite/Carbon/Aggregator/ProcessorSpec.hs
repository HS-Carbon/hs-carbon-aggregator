{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Carbon.Aggregator.ProcessorSpec (spec) where

import Carbon
import Carbon.Aggregator (AggregationMethod(..))
import Carbon.Aggregator.Rules (Rule(..))
import Carbon.Aggregator.Buffer (MetricBuffers(..))
import Carbon.Aggregator.Processor
import Carbon.TestExtensions()

import Test.Hspec

deriving instance Show MetricBuffers
deriving instance Eq MetricBuffers

spec :: Spec
spec = do
    describe "collectAggregated" $ do

        it "works with empty buffer manager" $ do
            bm <- newBuffersManagerIO
            metrics <- collectAggregatedIO 5 1000 bm
            metrics `shouldBe` []

        it "works with non-empty buffer manager" $ do
            let rules = [Rule "metric" "metric-sum" Sum 10]
            bm <- newBuffersManagerIO
            outtuples <- processAggregateManyIO rules bm [metricTuple "metric" 1001 42.0, metricTuple "metric" 1002 24.0]

            outtuples `shouldBe` [metricTuple "metric" 1001 42.0, metricTuple "metric" 1002 24.0]

            metrics <- collectAggregatedIO 5 1000 bm
            metrics `shouldBe` [metricTuple "metric-sum" 1000 66.0]

            metrics' <- collectAggregatedIO 5 1000 bm
            metrics' `shouldBe` []

        it "does not propagate metrics that have to be overwritten" $ do
            let rules = [Rule "metric" "metric" Sum 10]
            bm <- newBuffersManagerIO
            outtuples <- processAggregateManyIO rules bm [metricTuple "metric" 1001 42.0, metricTuple "metric" 1002 24.0]

            outtuples `shouldBe` []

            metrics <- collectAggregatedIO 5 1000 bm
            metrics `shouldBe` [metricTuple "metric" 1000 66.0]
