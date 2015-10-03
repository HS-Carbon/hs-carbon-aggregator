{-# LANGUAGE OverloadedStrings #-}

module Carbon.Codec.PickleSpec (spec) where

import Carbon
import Carbon.Codec.Pickle

import qualified Data.ByteString.Lazy as L
import Data.ByteString (pack)

import Test.Hspec
import Data.Knob
import System.IO
import Carbon.TestExtensions ()

spec :: Spec
spec = do

    it "decodes encoded pickle message" $ do
        let encoded = encodePickle [metricTuple "metric" 1000 42.0]
        let decoded = decodePickle $ L.toStrict encoded
        decoded `shouldBe` Right [metricTuple "metric" 1000 42.0]

    it "decodes multiple batches from single Handler" $ do
        knob <- newKnob (pack [])
        hw <- newFileHandle knob "test.txt" WriteMode
        writePickled [metricTuple "metric" 1000 42.0, metricTuple "metric" 1010 24.0] hw
        writePickled [metricTuple "metric" 1020 124.0] hw
        hClose hw

        hr <- newFileHandle knob "test.txt" ReadMode
        readPickled hr `shouldReturn` Just [metricTuple "metric" 1000 42.0, metricTuple "metric" 1010 24.0]
        readPickled hr `shouldReturn` Just [metricTuple "metric" 1020 124.0]
