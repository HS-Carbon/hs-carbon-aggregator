{-# LANGUAGE OverloadedStrings #-}

module Carbon.Aggregator.Rules (
                                 Rule(..)
                               , ruleAggregationMethod
                               , ruleAggregationFrequency
                               , parseRuleDefinition
                               , aggregateMetric
                               ) where

import Text.Regex.PCRE
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (readInt)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Search (split, replace)
import Control.Applicative
import Control.Parallel.Strategies (NFData)
import Carbon.Aggregator

data Rule = Rule InputPattern OutputPattern AggregationMethod AggregationFrequency deriving (Show)
instance NFData Rule

ruleAggregationMethod :: Rule -> AggregationMethod
ruleAggregationMethod (Rule _ _ method _) = method

ruleAggregationFrequency :: Rule -> AggregationFrequency
ruleAggregationFrequency (Rule _ _ _ freq) = freq

aggregateMetric :: SourceMetricName -> Rule -> Maybe AggregatedMetricName
aggregateMetric sm (Rule inp outp _ _) = do
    let regex = buildRegex inp outp
    if sm =~ regex
        then Just outp
        else Nothing

buildRegex :: InputPattern -> OutputPattern -> ByteString
buildRegex inp _ = B.concat ["^", (B.intercalate "\\." regex_pattern_parts), "$"]
    where regex_pattern_parts = translate_part <$> split "." inp
          translate_part part = strictReplace "*" "[^.]*" part

strictReplace :: ByteString -> ByteString -> ByteString -> ByteString
strictReplace old new str = toStrict $ replace old new str

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
