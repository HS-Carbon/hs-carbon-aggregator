{-# LANGUAGE RecordWildCards #-}

module Carbon.Aggregator.Buffer (
                                  DataPoint(..)
                                , MetricBuffers(..)
                                , bufferFor
                                , ModificationResult(..)
                                , appendDataPoint
                                , computeAggregated
                                ) where

import Data.ByteString (ByteString)
import Carbon.Aggregator (AggregationFrequency)
import Data.Map (Map)
import qualified Data.Map as Map

type Timestamp = Int
type MetricValue = Double
data DataPoint = DataPoint { timestamp :: Timestamp, value :: MetricValue }

type MetricPath = ByteString
type Interval = Int
type Buffer = [MetricValue]
type IntervalBuffers = Map Interval Buffer
data MetricBuffers = MetricBuffers { path :: MetricPath, frequency :: AggregationFrequency, intervalBuffers :: IntervalBuffers }

data ModificationResult = ModificationResult MetricBuffers [(MetricPath, DataPoint)]

bufferFor :: MetricPath -> AggregationFrequency -> MetricBuffers
bufferFor path freq = MetricBuffers path freq Map.empty

appendDataPoint :: MetricBuffers -> DataPoint -> MetricBuffers
appendDataPoint MetricBuffers{..} dp = MetricBuffers path frequency newBuf
    where newBuf = appendBufferDataPoint frequency dp intervalBuffers

appendBufferDataPoint :: AggregationFrequency -> DataPoint -> IntervalBuffers -> IntervalBuffers
appendBufferDataPoint freq DataPoint{..} bufs = Map.insertWith (++) interval [value] bufs
    where interval = timestamp `quot` freq

-- Check if there are data point ready to be emitted. If there aren't any, Nothing is returned.
computeAggregated :: Int -> Timestamp -> MetricBuffers -> Maybe ModificationResult
computeAggregated _ _ mbufs
    -- No buffers - nothing to return
    | Map.null $ intervalBuffers mbufs = Nothing
    | otherwise = error "Not implemented"
