module Carbon.Aggregator.Rules (
                                 Rule(..)
                               , ruleAggregationMethod
                               , ruleAggregationFrequency
                               , ruleAggregatedMetricName
                               ) where

import Carbon.Aggregator
import Carbon.Aggregator.Rules.Template

data Rule = Rule InputPattern OutputPattern AggregationMethod AggregationFrequency deriving (Show)

ruleAggregationMethod :: Rule -> AggregationMethod
ruleAggregationMethod (Rule _ _ method _) = method

ruleAggregationFrequency :: Rule -> AggregationFrequency
ruleAggregationFrequency (Rule _ _ _ freq) = freq

ruleAggregatedMetricName :: Rule -> SourceMetricName -> Maybe AggregatedMetricName
ruleAggregatedMetricName (Rule inp outp _ _) sm = makeAggregatedMetricName inp outp sm
