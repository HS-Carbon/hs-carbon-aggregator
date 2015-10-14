{-# LANGUAGE MultiParamTypeClasses #-}

module Carbon.Aggregator.Processor (
                                     BuffersManager
                                   , BufferManagerMonad(..)
                                   , newBuffersManager
                                   , processAggregate
                                   , processAggregateManyIO
                                   , collectAggregated
                                   , collectAggregatedIO
                                   ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, catMaybes)
import Control.Applicative ((<$>))
import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, writeTVar, modifyTVar)

import Carbon
import Carbon.Aggregator
import Carbon.Aggregator.Rules
import Carbon.Aggregator.Buffer

type BuffersManager v = Map MetricPath (v MetricBuffers)

newBuffersManager :: BuffersManager v
newBuffersManager = Map.empty

processAggregateManyIO :: [Rule] -> TVar (BuffersManager TVar) -> [MetricTuple] -> IO [MetricTuple]
processAggregateManyIO rules tbm mtuples = do
    let (actionss, moutms) = unzip $ map (processAggregate rules tbm) mtuples
    mapM_ atomically $ concat actionss
    return $ catMaybes moutms

processAggregate :: (BufferManagerMonad m v) => [Rule] -> v (BuffersManager v) -> MetricTuple -> ([m ()], (Maybe MetricTuple))
processAggregate rules vbm mtuple@(MetricTuple metric dp) = do
    -- TODO: rewrite rules PRE

    let matchingRules = mapMaybe (metricRule metric) rules :: [(AggregatedMetricName, Rule)]
    let actions = map applyAggregationRule matchingRules

    -- TODO: rewrite rules POST

    if metric `elem` (fst <$> matchingRules)
        then (actions, Nothing)
        else (actions, Just mtuple)

    where
        metricRule :: MetricPath -> Rule -> Maybe (AggregatedMetricName, Rule)
        metricRule rpath rule = ruleAggregatedMetricName rule rpath >>= \p -> return (p, rule)

        applyAggregationRule (metricName, rule) = processAggregateRule rule vbm metricName dp

processAggregateRule :: (BufferManagerMonad m v) => Rule -> v (BuffersManager v) -> AggregatedMetricName -> DataPoint -> m ()
processAggregateRule rule vmbm metric dp = do
    vBuf <- getBufferRef vmbm (metric, rule)
    appendBufferDataPoint vBuf dp

collectAggregatedIO :: Int -> Timestamp -> BuffersManager TVar -> IO [MetricTuple]
collectAggregatedIO maxBuckets now bm = concat <$> mapM atomically (collectAggregated maxBuckets now bm)

collectAggregated :: Int -> Timestamp -> BuffersManager TVar -> [STM [MetricTuple]]
collectAggregated maxBuckets now bm = do
    -- We need to iterate through map _modifying_ each if its MetricBuffers
    -- We could of course return just "STM [MetricTuple]", but chosen approach allows us to operate with smaller transactions.
    map process $ Map.assocs bm
    where
        process :: (AggregatedMetricName, TVar MetricBuffers) -> STM [MetricTuple]
        process (mpath, vmbufs) = do
            mbufs <- readTVar vmbufs
            case computeAggregated maxBuckets now mbufs of
                Nothing -> return []
                Just result -> do
                    writeTVar vmbufs $! metricBuffers result
                    return [MetricTuple mpath p | p <- emittedDataPoints result]

class Monad m => BufferManagerMonad m v where

    getBufferRef :: v (BuffersManager v) -> (AggregatedMetricName, Rule) -> m (v MetricBuffers)

    appendBufferDataPoint :: v MetricBuffers -> DataPoint -> m ()

instance BufferManagerMonad STM TVar where

    getBufferRef vbm (mpath, rule) = do
        bm <- readTVar vbm
        buf <- newTVar $ createBuffer
        case Map.insertLookupWithKey' keepOldValue mpath buf bm of
            (Nothing, bm') -> do
                writeTVar vbm bm'
                return buf
            (Just existingBuf, _) -> do
                return existingBuf

        where createBuffer = bufferFor mpath ruleFrequency ruleMethod
              ruleFrequency = ruleAggregationFrequency rule
              ruleMethod = ruleAggregationMethod rule
              keepOldValue _key _newval oldval = oldval

    appendBufferDataPoint vBuf dp = modifyTVar vBuf $
        \buf -> appendDataPoint buf dp
