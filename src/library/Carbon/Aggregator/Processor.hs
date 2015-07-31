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

processAggregateT :: [Rule] -> TVar BuffersManager -> (MetricPath, DataPoint) -> STM (Maybe (MetricPath, DataPoint))
processAggregateT rules tbm (mpath, dp) = do
    bm <- readTVar tbm
    let (bm', maybeDp) = processAggregate rules bm (mpath, dp)
    writeTVar tbm bm'
    return maybeDp

processAggregate :: [Rule] -> BuffersManager -> (MetricPath, DataPoint) -> (BuffersManager, Maybe (MetricPath, DataPoint))
processAggregate rules bm (metric, dp) = do
    -- TODO: rewrite rules PRE

    let matchingRules = mapMaybe (metricRule metric) rules :: [(AggregatedMetricName, Rule)]
    let buffers = getOrCreateBuffer bm <$> matchingRules :: [MetricBuffers]
    let buffers' = flip appendDataPoint dp <$> buffers
    let bm' = updateBuffers buffers' bm

    -- TODO: rewrite rules POST

    if metric `elem` (fst <$> matchingRules)
        then (bm', Nothing)
        else (bm', Just (metric, dp))

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

collectAggregatedT :: Int -> Timestamp -> TVar BuffersManager -> STM [(MetricPath, DataPoint)]
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
collectAggregated :: Int -> Timestamp -> BuffersManager -> ([(MetricPath, DataPoint)], BuffersManager)
collectAggregated maxBuckets now bm = do
    Map.mapAccumWithKey accum [] bm
    where
        accum :: [(MetricPath, DataPoint)] -> MetricPath -> MetricBuffers -> ([(MetricPath, DataPoint)], MetricBuffers)
        accum dps mpath mbufs = case computeAggregated maxBuckets now mbufs of
            Nothing -> (dps, mbufs)
            Just result -> (dps ++ emittedMetrics result, metricBuffers result)
            where
                emittedMetrics result = [(mpath, p) | p <- emittedDataPoints result]
