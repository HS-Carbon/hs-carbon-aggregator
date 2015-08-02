module Carbon.Aggregator.Processor (
                                     BuffersManager
                                   , newBuffersManager
                                   , processAggregateT
                                   , collectAggregatedT
                                   , collectAggregated
                                   ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Control.Applicative ((<$>))
import Control.Concurrent.STM (STM, TVar, readTVar, writeTVar, retry)

import Carbon
import Carbon.Aggregator
import Carbon.Aggregator.Rules
import Carbon.Aggregator.Buffer
import Carbon.Compose

type BuffersManager = Map MetricPath MetricBuffers

newBuffersManager :: BuffersManager
newBuffersManager = Map.empty

processAggregateT :: [Rule] -> TVar BuffersManager -> MetricTuple -> STM (Maybe MetricTuple)
processAggregateT rules tbm mtuple = do
    bm <- readTVar tbm
    let (bm', maybeTuple) = processAggregate rules bm mtuple
    writeTVar tbm bm'
    return maybeTuple

processAggregate :: [Rule] -> BuffersManager -> MetricTuple -> (BuffersManager, Maybe MetricTuple)
processAggregate rules bm mtuple@(MetricTuple metric dp) = do
    -- TODO: rewrite rules PRE

    let matchingRules = mapMaybe (metricRule metric) rules :: [(AggregatedMetricName, Rule)]
    let buffers = getOrCreateBuffer bm <$> matchingRules :: [MetricBuffers]
    let buffers' = flip appendDataPoint dp <$> buffers
    let bm' = updateBuffers buffers' bm

    -- TODO: rewrite rules POST

    if metric `elem` (fst <$> matchingRules)
        then (bm', Nothing)
        else (bm', Just mtuple)

    where
        metricRule :: MetricPath -> Rule -> Maybe (AggregatedMetricName, Rule)
        metricRule rpath rule = ruleAggregatedMetricName rule rpath >>= \p -> return (p, rule)

updateBuffers :: [MetricBuffers] -> BuffersManager -> BuffersManager
updateBuffers buffers bm = compose (insertBuffer <$> buffers) $ bm
    where insertBuffer buf manager = Map.insert (path buf) buf manager

getOrCreateBuffer :: BuffersManager -> (MetricPath, Rule) -> MetricBuffers
getOrCreateBuffer bm (metric, rule) = Map.findWithDefault (createBuffer) metric bm
    where createBuffer = bufferFor metric ruleFrequency ruleMethod
          ruleFrequency = ruleAggregationFrequency rule
          ruleMethod = ruleAggregationMethod rule

collectAggregatedT :: Int -> Timestamp -> TVar BuffersManager -> STM [MetricTuple]
collectAggregatedT maxBuckets now tbm = do
    bm <- readTVar tbm
    let (metrics, bm') = collectAggregated maxBuckets now bm
    if null metrics
        then retry
        else do
            writeTVar tbm bm'
            return metrics

-- | 'collectAggregated' collects aggregated metrics that are ready to be emitted.
-- 'maxBuckets' is program-wide configuration that  specifies how many buckets per
-- aggregated metric name should be kept.
collectAggregated :: Int -> Timestamp -> BuffersManager -> ([MetricTuple], BuffersManager)
collectAggregated maxBuckets now bm = do
    Map.mapAccumWithKey accum [] bm
    where
        accum :: [MetricTuple] -> MetricPath -> MetricBuffers -> ([MetricTuple], MetricBuffers)
        accum dps mpath mbufs = case computeAggregated maxBuckets now mbufs of
            Nothing -> (dps, mbufs)
            Just result -> (dps ++ emittedMetrics result, metricBuffers result)
            where
                emittedMetrics result = [MetricTuple mpath p | p <- emittedDataPoints result]
