module Carbon.Aggregator (
                           InputPattern
                         , OutputPattern
                         , AggregationMethod(..)
                         , AggregationFrequency
                         , SourceMetricName
                         , AggregatedMetricName
                         , AggregatedMetricNameMaker
                         ) where

import Carbon
import Data.ByteString

type InputPattern = ByteString
type OutputPattern = ByteString
data AggregationMethod = Sum | Avg | Min | Max | Count deriving (Show, Eq)
type AggregationFrequency = Int
type SourceMetricName = MetricPath
type AggregatedMetricName = MetricPath
type AggregatedMetricNameMaker = SourceMetricName -> Maybe AggregatedMetricName
