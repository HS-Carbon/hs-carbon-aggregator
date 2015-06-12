{-# LANGUAGE OverloadedStrings #-}

module Carbon.Aggregator (
                           Rule(..)
                         , InputPattern
                         , OutputPattern
                         , AggregationMethod(..)
                         , AggregationFrequency
                         , parseRuleDefinition

                         , SourceMetricName
                         , AggregatedMetricName
                         , aggregateMetric
                         ) where

import Text.Regex.PCRE
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (readInt)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Search (split, replace)
import Control.Applicative

type InputPattern = ByteString
type OutputPattern = ByteString
data AggregationMethod = Sum | Avg | Min | Max | Count deriving (Show, Eq)
type AggregationFrequency = Int

data Rule = Rule InputPattern OutputPattern AggregationMethod AggregationFrequency deriving (Show)

type SourceMetricName = ByteString
type AggregatedMetricName = ByteString
aggregateMetric :: Rule -> SourceMetricName -> Maybe AggregatedMetricName
aggregateMetric (Rule inp outp _ _) sm = do
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
    let [outp, sfreq, _, inp] = tail $ head matches
    freq <- fst <$> readInt sfreq
    return $ Rule inp outp Sum freq
