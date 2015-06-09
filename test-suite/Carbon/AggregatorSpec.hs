module Carbon.AggregatorSpec (spec) where

import Carbon.Aggregator

import Test.Hspec

spec :: Spec
spec = do

    describe "main" $ do
        it "returns the unit" $
            main `shouldReturn` ()

    describe "rule" $ do
        describe "simple aggregateMetric" $ do
            let method = Avg
            let frequency = 10

            let rule99 = Rule "hosts.*.hist.p99" "aggregated.hist.p99" method frequency
            let rule999 = Rule "hosts.*.hist.p999" "aggregated.hist.p999" method frequency

            it "should calculate rule99 correctly" $ do
                aggregateMetric rule99 "hosts.abc.hist.p99" `shouldBe` Just "aggregated.hist.p99"
                aggregateMetric rule999 "hosts.abc.hist.p99" `shouldBe` Nothing

            it "should calculate rule999 correctly" $ do
                aggregateMetric rule99 "hosts.abc.hist.p999" `shouldBe` Nothing
                aggregateMetric rule999 "hosts.abc.hist.p999" `shouldBe` Just "aggregated.hist.p999"
