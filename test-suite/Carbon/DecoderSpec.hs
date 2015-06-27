{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Carbon.DecoderSpec (spec) where

import Carbon.Decoder
import Carbon

import Test.Hspec

deriving instance Eq DataPoint
deriving instance Show DataPoint

spec :: Spec
spec = do

    describe "decodePlainText" $ do

        it "parses correct input" $ do
            decodePlainText "metric.path 4 1000" `shouldBe` Just ("metric.path", DataPoint 1000 4)
