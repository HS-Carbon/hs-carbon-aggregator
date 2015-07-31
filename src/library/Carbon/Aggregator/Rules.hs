{-# LANGUAGE OverloadedStrings #-}

module Carbon.Aggregator.Rules (
                                 Rule(..)
                               , ruleAggregationMethod
                               , ruleAggregationFrequency
                               , parseRuleDefinition
                               , ruleAggregatedMetricName
                               ) where

import Text.Regex.PCRE
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (readInt)
import Control.Applicative
import Control.Parallel.Strategies (NFData)
import Carbon.Aggregator
import Carbon.Aggregator.Rules.Template

data Rule = Rule InputPattern OutputPattern AggregationMethod AggregationFrequency deriving (Show)
instance NFData Rule

ruleAggregationMethod :: Rule -> AggregationMethod
ruleAggregationMethod (Rule _ _ method _) = method

ruleAggregationFrequency :: Rule -> AggregationFrequency
ruleAggregationFrequency (Rule _ _ _ freq) = freq

ruleAggregatedMetricName :: Rule -> SourceMetricName -> Maybe AggregatedMetricName
ruleAggregatedMetricName (Rule inp outp _ _) sm = makeAggregatedMetricName inp outp sm

parseRuleDefinition :: ByteString -> Maybe Rule
parseRuleDefinition rulestr = do
    let pattern = B.intercalate "\\s+" ["(.+?)", "\\((\\d+)\\)", "=", "(.+?)", "(.+)"]
    matches <- rulestr =~~ pattern
    -- first element is the source string, so drop it
    let [outp, sfreq, smethod, inp] = tail $ head matches
    freq <- fst <$> readInt sfreq
    method <- readMethod smethod
    return $ Rule inp outp method freq

readMethod :: Monad m => ByteString -> m AggregationMethod
readMethod "sum"   = return Sum
readMethod "avg"   = return Avg
readMethod "min"   = return Min
readMethod "max"   = return Max
readMethod "count" = return Count
readMethod _       = fail "Aggregation method not recognized"
