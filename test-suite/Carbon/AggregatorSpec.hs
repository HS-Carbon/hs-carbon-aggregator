{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Carbon.AggregatorSpec (spec) where

import Carbon.Aggregator
import Carbon.Aggregator.Rules
import Carbon.Aggregator.Rules.Definition

import Test.Hspec
import Carbon.TestExtensions ()

spec :: Spec
spec = do

    describe "rule" $ do
        describe "simple aggregateMetric" $ do
            let method = Avg
            let frequency = 10

            let rule99 = Rule "hosts.*.hist.p99" "aggregated.hist.p99" method frequency
            let rule999 = Rule "hosts.*.hist.p999" "aggregated.hist.p999" method frequency

            let aggregateMetric = flip ruleAggregatedMetricName
            it "should calculate rule99 correctly" $ do
                aggregateMetric "hosts.abc.hist.p99" rule99 `shouldBe` Just "aggregated.hist.p99"
                aggregateMetric "hosts.abc.hist.p99" rule999 `shouldBe` Nothing

            it "should calculate rule999 correctly" $ do
                aggregateMetric "hosts.abc.hist.p999" rule99 `shouldBe` Nothing
                aggregateMetric "hosts.abc.hist.p999" rule999 `shouldBe` Just "aggregated.hist.p999"

    describe "parser" $ do
        it "parses correct Sum rule" $ do
            parseRuleDefinition "outp (10) = sum inp" `shouldBe` Just (Rule "inp" "outp" Sum 10)
        it "parses correct Count rule" $ do
            parseRuleDefinition "outp (10) = count inp" `shouldBe` Just (Rule "inp" "outp" Count 10)
        it "returns Nothing for malformed rule" $ do
            parseRuleDefinition "outp (10) = inp" `shouldBe` Nothing
        it "returns Nothing for malformed aggregation method" $ do
            parseRuleDefinition "outp (10) = unknown inp" `shouldBe` Nothing
        it "return Nothing for incorrect aggregation frequency" $ do
            parseRuleDefinition "outp (a) = sum inp" `shouldBe` Nothing
