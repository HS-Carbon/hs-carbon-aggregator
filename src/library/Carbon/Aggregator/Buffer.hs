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

data ModificationResult = ModificationResult { metricBuffers :: MetricBuffers, emittedEvents :: [(MetricPath, DataPoint)] }

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
computeAggregated maxIntervals now mbufs
    -- No buffers - nothing to return
    | Map.null $ intervalBuffers mbufs = Nothing
    | otherwise = do
        let currentInterval = now `quot` frequency mbufs
        --let ageThreshold = currentInterval - maxIntervals * frequency mbufs
        let thresholdInterval = currentInterval - maxIntervals
        -- Split buffers into those that passed age threshold and those that didn't.
        let (_, newBufs) = Map.split (thresholdInterval) (intervalBuffers mbufs)
        -- We are interested only in the "fresh" part and can safely drop old one unevaluated.
        let events = Map.foldrWithKey appendIntervalDps [] newBufs
        let mbufs' = MetricBuffers { path = path mbufs, frequency = frequency mbufs, intervalBuffers = newBufs}
        return $ ModificationResult mbufs' events
        where
            appendIntervalDps interval buf dps = dps ++ [bufDps interval buf]
            bufDps interval buf = (path mbufs, DataPoint (interval * frequency mbufs) (head buf))
