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
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Control.Concurrent.STM (atomically, STM, TVar, newTVar, readTVar, readTVarIO, writeTVar, modifyTVar')
import Control.Monad (foldM)
import qualified STMContainers.Map as STMap
import ListT (toList)

type Interval = Int
type Buffer = (Bool, [MetricValue])
type IntervalBuffers = Map Interval (TVar Buffer)
data MetricBuffers = MetricBuffers {
    path :: MetricPath,
    frequency :: AggregationFrequency,
    aggregationMethod :: AggregationMethod,
    intervalBuffers :: TVar IntervalBuffers
}

type BuffersManager = STMap.Map MetricPath MetricBuffers

bufferFor :: MetricPath -> AggregationFrequency -> AggregationMethod -> STM MetricBuffers
bufferFor path freq aggmethod = do
    intervallBuffers <- newTVar Map.empty
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
            intervalBuffers' <- readTVarIO $ intervalBuffers metricBuffers
            buffers' <- sequence $ readTVarIO <$> Map.elems intervalBuffers'
            let bufferredDps = sum $ map (\(_, dps) -> length dps) buffers'
            return $! r + bufferredDps

appendDataPoint :: MetricBuffers -> DataPoint -> STM ()
appendDataPoint MetricBuffers{..} dp = appendBufferDataPoint frequency dp intervalBuffers

appendBufferDataPoint :: AggregationFrequency -> DataPoint -> TVar IntervalBuffers -> STM ()
appendBufferDataPoint freq (DataPoint timestamp value) tbufs = do
    -- TODO: can be improved with custom "insertOrUpdate" Map function.
    bufs <- readTVar tbufs
    case Map.lookup interval bufs of
        Just (tBuf) -> do
            modifyTVar' tBuf $ appendBufferValue value
        Nothing -> do
            tBuf <- newTVar (True, [value])
            writeTVar tbufs $! Map.insert interval tBuf bufs
    where interval = timestamp `quot` freq

appendBufferValue :: MetricValue -> Buffer -> Buffer
appendBufferValue val (_, oldVals) = (True, val : oldVals)

computeAggregatedIO :: Int -> Timestamp -> MetricBuffers -> IO [DataPoint]
computeAggregatedIO maxIntervals now mbufs@MetricBuffers{..} = do
    freshBufs <- atomically $ splitBuffersT maxIntervals now mbufs
    if Map.null $ freshBufs
        then
            return []
        else do
            maybeDps <- mapM atomically $ computeAggregateBuffersT frequency aggregationMethod freshBufs
            let dps = catMaybes maybeDps
            return dps

-- | Update 'MetricBuffers' *in place* and return "fresh" buffers, silently dropping outdated ones
splitBuffersT :: Int -> Timestamp -> MetricBuffers -> STM IntervalBuffers
splitBuffersT maxIntervals now MetricBuffers{..} = do
    let currentInterval = now `quot` frequency
    let thresholdInterval = currentInterval - maxIntervals
    -- Split buffers into those that passed age threshold and those that didn't.
    (_outdatedBufs, freshBufs) <- Map.split thresholdInterval <$> readTVar intervalBuffers
    writeTVar intervalBuffers $! freshBufs
    return freshBufs

computeAggregateBuffersT :: AggregationFrequency -> AggregationMethod -> IntervalBuffers -> [STM (Maybe DataPoint)]
computeAggregateBuffersT frequency aggregationMethod bufs = map processBuffer $ Map.assocs bufs
    where
        processBuffer :: (Interval, TVar Buffer) -> STM (Maybe DataPoint)
        processBuffer (interval, tbuf) = do
            buf <- readTVar tbuf
            case buf of
                (False, _) -> return Nothing
                (True, vals) -> do
                    writeTVar tbuf (False, vals)
                    return . Just $! bufferDp (interval * frequency) vals

        bufferDp :: Timestamp -> [MetricValue] -> DataPoint
        bufferDp time vals = DataPoint time (aggreagte vals)

        aggreagte :: [MetricValue] -> MetricValue
        aggreagte = aggregateWith aggregationMethod
            where aggregateWith Sum   vals = sum vals
                  aggregateWith Avg   vals = sum vals / (realToFrac $ length vals)
                  aggregateWith Min   vals = minimum vals
                  aggregateWith Max   vals = maximum vals
                  aggregateWith Count vals = realToFrac $ length vals
