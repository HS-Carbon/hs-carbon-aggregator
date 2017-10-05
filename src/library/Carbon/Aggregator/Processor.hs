{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Carbon.Aggregator.Processor (
                                     BuffersManager
                                   , newBuffersManager
                                   , newBuffersManagerIO
                                   , processAggregate
                                   , processAggregateManyIO
                                   , collectAggregatedIO
                                   ) where

import Data.Maybe (mapMaybe, catMaybes)
import Control.Concurrent.STM (STM, atomically)
import qualified STMContainers.Map as STMap
import ListT (toList)
import Focus (StrategyM, Decision(..))

import Carbon
import Carbon.Aggregator
import Carbon.Aggregator.Rules
import Carbon.Aggregator.Buffer

processAggregateManyIO :: [Rule] -> BuffersManager -> [MetricTuple] -> IO [MetricTuple]
processAggregateManyIO rules bm mtuples = do
    let (actionss, moutms) = unzip $ map (processAggregate rules bm) mtuples
    sequence_ $ concat actionss
    return $ catMaybes moutms

processAggregate :: [Rule] -> BuffersManager -> MetricTuple -> ([IO ()], (Maybe MetricTuple))
processAggregate rules bm mtuple@(MetricTuple metric dp) = do
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

        applyAggregationRule (metricName, rule) = processAggregateRule rule bm metricName dp

processAggregateRule :: Rule -> BuffersManager -> AggregatedMetricName -> DataPoint -> IO ()
processAggregateRule rule bm metric dp = do
    buf <- getBufferRef bm (metric, rule)
    appendDataPoint buf dp

collectAggregatedIO :: Int -> Timestamp -> BuffersManager -> IO [MetricTuple]
collectAggregatedIO maxBuckets now bm = do
    pairs <- atomically . toList $ STMap.stream bm
    concat <$> mapM process pairs
    where
        process :: (AggregatedMetricName, MetricBuffers) -> IO [MetricTuple]
        process (mpath, mbufs) = do
            dps <- computeAggregatedIO maxBuckets now mbufs
            return [MetricTuple mpath p | p <- dps]

getBufferRef :: BuffersManager -> (AggregatedMetricName, Rule) -> IO MetricBuffers
getBufferRef bm (mpath, rule) = do
    buf <- createBuffer
    atomically $ STMap.focus (insertIfNotExistsM buf) mpath bm
    where
        createBuffer = bufferFor mpath ruleFrequency ruleMethod
        ruleFrequency = ruleAggregationFrequency rule
        ruleMethod = ruleAggregationMethod rule

        insertIfNotExistsM :: (Monad m) => a -> StrategyM m a a
        insertIfNotExistsM a = maybe (return (a, Replace a)) (\r -> return (r, Keep))
        {-# SPECIALIZE insertIfNotExistsM :: MetricBuffers -> StrategyM STM MetricBuffers MetricBuffers #-}
