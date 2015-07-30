{-# LANGUAGE OverloadedStrings #-}

module Carbon.Aggregator.Rules.TokenizerSpec (spec) where

import Data.ByteString.Char8
import Carbon.Aggregator.Rules.Tokenizer

import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec = do

    describe "parseTokenize" $ do

        it "handles constant strings" $ do
            ("metric-name" :: ByteString) ~> parseTokenize
                `shouldParse` [Const "metric-name"]

        it "handles simple separated patterns" $ do
            ("metric.name" :: ByteString) ~> parseTokenize
                `shouldParse` [Const "metric", Sep, Const "name"]

        it "handlex complex patterns" $ do
            ("dc-<dc>.<component>.*.*.<metric>-max" :: ByteString) ~> parseTokenize
                `shouldParse`
                    [Const "dc-", Group "dc", Sep, Group "component", Sep,
                        SkipMany, Sep, SkipMany, Sep, Group "metric", Const "-max"]
