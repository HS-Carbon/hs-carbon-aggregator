{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Carbon.DecoderSpec (spec) where

import Carbon.Decoder
import Carbon

import Test.Hspec
import Carbon.TestExtensions ()

spec :: Spec
spec = do

    describe "decodePlainText" $ do

        it "parses correct input" $ do
            decodePlainText "metric.path 4 1000" `shouldBe` Just ("metric.path", DataPoint 1000 4)

        it "ignores incorrect input" $ do
            decodePlainText "garbage" `shouldBe` Nothing

        it "ignores incorrect numbers" $ do
            -- It isn't necessary, but why not?
            decodePlainText "metric 4a 1000b" `shouldBe` Just ("metric", DataPoint 1000 4)
