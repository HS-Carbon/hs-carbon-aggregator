{-# LANGUAGE OverloadedStrings #-}

module Carbon.Codec.PickleSpec (spec) where

import Carbon
import Carbon.Codec.Pickle

import qualified Data.ByteString.Lazy as L

import Test.Hspec
import Carbon.TestExtensions ()

spec :: Spec
spec = do

    it "decodes encoded pickle message" $ do
        let encoded = encodePickle [metricTuple "metric" 1000 42.0]
        let decoded = decodePickle $ L.toStrict encoded
        decoded `shouldBe` Right [metricTuple "metric" 1000 42.0]
