module Carbon.Aggregator.Processor

where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Control.Applicative

import Carbon
import Carbon.Aggregator hiding (aggregationMethod, aggregationFrequency)
import qualified Carbon.Aggregator
import Carbon.Aggregator.Buffer

type BuffersManager = Map MetricPath MetricBuffers

newBuffersManager :: BuffersManager
newBuffersManager = Map.empty

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
        metricRule rpath rule = aggregateMetric rpath rule >>= \p -> return (p, rule)

updateBuffers :: [MetricBuffers] -> BuffersManager -> BuffersManager
updateBuffers buffers bm = compose (insertBuffer <$> buffers) $ bm
    where insertBuffer buf manager = Map.insert (path buf) buf manager

compose :: [a -> a] -> a -> a
compose fs v = foldl (flip (.)) id fs $ v

getOrCreateBuffer :: BuffersManager -> (MetricPath, Rule) -> MetricBuffers
getOrCreateBuffer bm (metric, rule) = Map.findWithDefault (createBuffer) metric bm
    where createBuffer = bufferFor metric ruleFrequency ruleMethod
          ruleFrequency = Carbon.Aggregator.aggregationFrequency rule
          ruleMethod = Carbon.Aggregator.aggregationMethod rule
