{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Carbon.Aggregator.Processor (
                                     BuffersManager
                                   , newBuffersManager
                                   , processAggregate
                                   , processAggregateManyIO
                                   , collectAggregatedIO
                                   ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, catMaybes)
import Control.Applicative ((<$>))
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, writeTVar)

import Carbon
import Carbon.Aggregator
import Carbon.Aggregator.Rules
import Carbon.Aggregator.Buffer

type BuffersManager = Map MetricPath MetricBuffers

newBuffersManager :: BuffersManager
newBuffersManager = Map.empty

processAggregateManyIO :: [Rule] -> TVar BuffersManager -> [MetricTuple] -> IO [MetricTuple]
processAggregateManyIO rules tbm mtuples = do
    let (actionss, moutms) = unzip $ map (processAggregate rules tbm) mtuples
    mapM_ atomically $ concat actionss
    return $ catMaybes moutms

processAggregate :: [Rule] -> TVar BuffersManager -> MetricTuple -> ([STM ()], (Maybe MetricTuple))
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

processAggregateRule :: Rule -> TVar BuffersManager -> AggregatedMetricName -> DataPoint -> STM ()
processAggregateRule rule vmbm metric dp = do
    buf <- getBufferRef vmbm (metric, rule)
    appendDataPoint buf dp

collectAggregatedIO :: Int -> Timestamp -> BuffersManager -> IO [MetricTuple]
collectAggregatedIO maxBuckets now bm = do
    fmap concat (mapM process $ Map.assocs bm)
    where
        process :: (AggregatedMetricName, MetricBuffers) -> IO [MetricTuple]
        process (mpath, mbufs) = do
            dps <- computeAggregatedIO maxBuckets now mbufs
            return [MetricTuple mpath p | p <- dps]

getBufferRef :: TVar BuffersManager -> (AggregatedMetricName, Rule) -> STM MetricBuffers
getBufferRef vbm (mpath, rule) = do
    bm :: Map MetricPath MetricBuffers <- readTVar vbm
    buf <- createBuffer
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
