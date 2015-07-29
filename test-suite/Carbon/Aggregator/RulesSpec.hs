{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Carbon.Aggregator.RulesSpec (spec) where

import Carbon.Aggregator
import Carbon.Aggregator.Rules

import Test.Hspec

deriving instance Eq Rule

spec :: Spec
spec = do

    describe "makeAggregatedMetricName" $ do

        it "processes complex pattern" $ do
            let sm = "nj01.app.component.worker-id.task-id.metric-max"
            let rule = Rule "<dc>.<app>.<component>.*.*.<metric>-max"
                            "<dc>.<app>-aggregated.<component>.<metric>-max" Max 10
            let om = "nj01.app-aggregated.component.metric-max"
            makeAggregatedMetricName rule sm `shouldBe` Just om
