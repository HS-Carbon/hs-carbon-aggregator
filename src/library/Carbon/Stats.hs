{-# LANGUAGE RecordWildCards #-}

module Carbon.Stats (
                      StatsConfig(..)
                    , StatsMap
                    , newStatsMap
                    , recordReceivedDataPoint
                    , recordAggregatedDataPoint
                    , collectSelfStatsIO
                    ) where

import Carbon (Timestamp, MetricTuple, metricTuple)
import Carbon.Aggregator.Buffer (BuffersManager, bufferManagerLength, countBufferedDataPoints)

import Control.Concurrent.STM (atomically)
import qualified Data.ByteString.Char8 as B
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Data.Maybe (fromJust)
import Control.Monad (forM)
import Control.Applicative ((<$>))
import Text.Printf

data StatsConfig = StatsConfig {
    metricPrefix :: String,
    hostname :: String,
    instanceName :: Maybe String
}

data Stat = AllocatedBuffers | BufferedDataPoints | AggregatedDataPoints |
            DataPointsReceived | BlacklistMatches | WhitelistRejects |
            CPUUsage | MemUsage
            deriving (Show, Eq, Ord)

type StatsMap = Map Stat (IORef Int)

newStatsMap :: IO StatsMap
newStatsMap = do
    let trackableStats = [AggregatedDataPoints, DataPointsReceived, BlacklistMatches, WhitelistRejects]
    mapFromKeysM (\_ -> newIORef 0) trackableStats
    where
        mapFromKeysM :: (Monad m, Functor m, Ord k) => (k -> m v) -> [k] -> m (Map k v)
        mapFromKeysM f keys = Map.fromList <$> forM keys (\k -> f k >>= \v -> return (k, v))

recordStat :: Stat -> StatsMap -> Int -> IO ()
recordStat stat smap incrementBy = do
    atomicModifyIORef' (get stat smap) $ modifyIncrement incrementBy
    where
        get k m = fromJust $ Map.lookup k m
        modifyIncrement add oldval = (oldval + add, ())

recordReceivedDataPoint :: StatsMap -> Int -> IO ()
recordReceivedDataPoint = recordStat DataPointsReceived

recordAggregatedDataPoint :: StatsMap -> Int -> IO ()
recordAggregatedDataPoint = recordStat AggregatedDataPoints

getAndResetStat :: StatsMap -> Stat -> IO Int
getAndResetStat smap stat = do
    atomicModifyIORef' (get stat smap) $ getAndReset
    where
        get k m = fromJust $ Map.lookup k m
        getAndReset old = (0, old)

fullMetricPath :: StatsConfig -> String -> B.ByteString
fullMetricPath StatsConfig{..} metricName = case instanceName of
    Nothing -> B.pack $ printf "%s.aggregator.%s.%s" metricPrefix hostname metricName
    Just justInstanceName -> B.pack $ printf "%s.aggregator.%s-%s.%s" metricPrefix hostname justInstanceName metricName

collectSelfStatsIO :: StatsConfig -> StatsMap -> BuffersManager -> Timestamp -> IO [MetricTuple]
collectSelfStatsIO statsConf statsMap bufmanager now = do
    let metricName = fullMetricPath statsConf
    let getAndReset = getAndResetStat statsMap
    let aggregatorMetric name value = metricTuple (metricName name) now $ realToFrac value

    -- Aggregator specific metrics:
    allocatedBuffersCount <- atomically $ bufferManagerLength bufmanager
    bufferedDatapointsCount <- countBufferedDataPoints bufmanager
    aggregatedDataPointsCount <- getAndReset AggregatedDataPoints

    -- Common metrics:
    dataPointsReceivedCount <- getAndReset DataPointsReceived
    -- TODO:
    -- cpuUsage
    -- memUsage

    return [
        aggregatorMetric "allocatedBuffers" allocatedBuffersCount,
        aggregatorMetric "bufferedDatapoints" bufferedDatapointsCount,
        aggregatorMetric "aggregateDatapointsSent" aggregatedDataPointsCount,
        aggregatorMetric "metricsReceived" dataPointsReceivedCount
        ]
