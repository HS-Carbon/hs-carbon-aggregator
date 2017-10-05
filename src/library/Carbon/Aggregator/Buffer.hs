{-# LANGUAGE RecordWildCards #-}

module Carbon.Aggregator.Buffer (
                                  MetricBuffers(..)
                                , BuffersManager
                                , bufferFor
                                , bufferManagerLength
                                , countBufferedDataPoints
                                , appendDataPoint
                                , computeAggregatedIO
                                , newBuffersManager
                                , newBuffersManagerIO
                                ) where

import Carbon
import Carbon.Aggregator (AggregationFrequency, AggregationMethod(..))
import Data.Map.Strict (Map)
import Data.IORef
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM (atomically, STM)
import Control.Monad (foldM)
import Control.Monad.Extra (mapMaybeM)
import qualified STMContainers.Map as STMap
import ListT (toList)

type Interval = Int
type Buffer = (Bool, [MetricValue])
type IntervalBuffers = Map Interval (IORef Buffer)
data MetricBuffers = MetricBuffers {
    path :: MetricPath,
    frequency :: AggregationFrequency,
    aggregationMethod :: AggregationMethod,
    intervalBuffers :: IORef IntervalBuffers
}

type BuffersManager = STMap.Map MetricPath MetricBuffers

bufferFor :: MetricPath -> AggregationFrequency -> AggregationMethod -> IO MetricBuffers
bufferFor path freq aggmethod = do
    intervallBuffers <- newIORef Map.empty
    return $ MetricBuffers path freq aggmethod intervallBuffers

newBuffersManager :: STM BuffersManager
newBuffersManager = STMap.new

newBuffersManagerIO :: IO BuffersManager
newBuffersManagerIO = STMap.newIO

bufferManagerLength :: BuffersManager -> STM Int
bufferManagerLength bm = length <$> (toList $ STMap.stream bm)

countBufferedDataPoints :: BuffersManager -> IO Int
countBufferedDataPoints bm = foldM accumBufferManagerDps 0 =<< streamBuffers bm
    where
        streamBuffers = atomically . toList . STMap.stream
        accumBufferManagerDps :: Int -> (MetricPath, MetricBuffers) -> IO Int
        accumBufferManagerDps r (_, metricBuffers) = do
            intervalBuffers' <- readIORef $ intervalBuffers metricBuffers
            buffers' <- sequence $ readIORef <$> Map.elems intervalBuffers'
            let bufferredDps = sum $ map (\(_, dps) -> length dps) buffers'
            return $! r + bufferredDps

appendDataPoint :: MetricBuffers -> DataPoint -> IO ()
appendDataPoint MetricBuffers{..} dp = appendBufferDataPoint frequency dp intervalBuffers

appendBufferDataPoint :: AggregationFrequency -> DataPoint -> IORef IntervalBuffers -> IO ()
appendBufferDataPoint freq (DataPoint timestamp value) intervalBuffersRef = do
    let interval = timestamp `quot` freq
    intervalBuffers <- readIORef intervalBuffersRef
    case Map.lookup interval intervalBuffers of
        Just bufRef -> do
            -- TODO: test preformance with strict and non-strict versions of atomicModifyIORef
            atomicModifyIORef' bufRef (modifyBuf value)
        Nothing -> do
            newBufRef <- newIORef (False, [])
            bufRef <- atomicModifyIORef' intervalBuffersRef (lookupOrInsert interval newBufRef)
            -- TODO: test preformance with strict and non-strict versions of atomicModifyIORef
            atomicModifyIORef' bufRef (modifyBuf value)
    return ()
    where
        lookupOrInsert :: Interval -> IORef Buffer -> IntervalBuffers -> (IntervalBuffers, IORef Buffer)
        lookupOrInsert interval newBufRef intervalBuffers = case Map.lookup interval intervalBuffers of
            Just bufRef -> (intervalBuffers, bufRef)
            Nothing -> (Map.insert interval newBufRef intervalBuffers, newBufRef)

        modifyBuf :: MetricValue -> Buffer -> (Buffer, Buffer)
        modifyBuf val buf = mktuple $ appendBufferValue val buf

mktuple :: a -> (a, a)
mktuple a = (a, a)

appendBufferValue :: MetricValue -> Buffer -> Buffer
appendBufferValue val (_, oldVals) = (True, val : oldVals)

computeAggregatedIO :: Int -> Timestamp -> MetricBuffers -> IO [DataPoint]
computeAggregatedIO maxIntervals now mbufs@MetricBuffers{..} = do
    freshBufs <- splitBuffers maxIntervals now mbufs
    if Map.null $ freshBufs
        then
            return []
        else
            computeAggregateBuffers frequency aggregationMethod freshBufs

-- | Update 'MetricBuffers' *in place* and return "fresh" buffers, silently dropping outdated ones
splitBuffers :: Int -> Timestamp -> MetricBuffers -> IO IntervalBuffers
splitBuffers maxIntervals now MetricBuffers{..} = do
    let currentInterval = now `quot` frequency
    let thresholdInterval = currentInterval - maxIntervals
    atomicModifyIORef' intervalBuffers (mktuple <$> dropOutdatedIntervals thresholdInterval)
    where
        dropOutdatedIntervals :: Interval -> IntervalBuffers -> IntervalBuffers
        dropOutdatedIntervals thresholdInterval bufs = snd $ Map.split thresholdInterval bufs

computeAggregateBuffers :: AggregationFrequency -> AggregationMethod -> IntervalBuffers -> IO [DataPoint]
computeAggregateBuffers frequency aggregationMethod bufs =
    mapMaybeM processBuffer $ Map.assocs bufs
    where
        processBuffer :: (Interval, IORef Buffer) -> IO (Maybe DataPoint)
        processBuffer (interval, bufref) = atomicModifyIORef' bufref (calculateIfChanged interval)

        calculateIfChanged :: Interval -> Buffer -> (Buffer, Maybe DataPoint)
        calculateIfChanged _ buf@(False, _) = (buf, Nothing)
        calculateIfChanged interval (True, vals) = ((False, vals), Just $! bufferDp (interval * frequency) vals)

        bufferDp :: Timestamp -> [MetricValue] -> DataPoint
        bufferDp time vals = DataPoint time (aggreagte vals)

        aggreagte :: [MetricValue] -> MetricValue
        aggreagte = aggregateWith aggregationMethod
            where aggregateWith Sum   vals = sum vals
                  aggregateWith Avg   vals = sum vals / (realToFrac $ length vals)
                  aggregateWith Min   vals = minimum vals
                  aggregateWith Max   vals = maximum vals
                  aggregateWith Count vals = realToFrac $ length vals
