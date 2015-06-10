{-# LANGUAGE OverloadedStrings #-}

module Carbon.Aggregator (module Carbon.Aggregator) where

import Text.Regex.Posix
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Search (split, replace)
import Control.Applicative

main :: IO ()
main = return ()

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
    if sm =~ regex then return outp else Nothing

buildRegex :: InputPattern -> OutputPattern -> ByteString
buildRegex inp _ = B.concat ["^", (B.intercalate "\\." regex_pattern_parts), "$"]
    where regex_pattern_parts = translate_part <$> split "." inp
          translate_part part = strictReplace "*" "[^.]*" part

strictReplace :: ByteString -> ByteString -> ByteString -> ByteString
strictReplace old new str = toStrict $ replace old new str
