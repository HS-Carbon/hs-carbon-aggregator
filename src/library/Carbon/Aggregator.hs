module Carbon.Aggregator (module Carbon.Aggregator) where

import Text.Regex.Posix
import Data.List
import Data.List.Split
import Control.Applicative

main :: IO ()
main = return ()

type InputPattern = String
type OutputPattern = String
data AggregationMethod = Sum | Avg | Min | Max | Count deriving (Show, Eq)
type AggregationFrequency = Int

data Rule = Rule InputPattern OutputPattern AggregationMethod AggregationFrequency deriving (Show)

type SourceMetricName = String
type AggregatedMetricName = String
aggregateMetric :: Rule -> SourceMetricName -> Maybe AggregatedMetricName
aggregateMetric (Rule inp outp _ _) sm = do
    let regex = buildRegex inp outp
    if sm =~ regex then return outp else Nothing

buildRegex :: InputPattern -> OutputPattern -> String
buildRegex inp _ = "^" ++ (intercalate "\\." regex_pattern_parts) ++ "$"
    where regex_pattern_parts = translate_part <$> splitOn "." inp
          translate_part part = replace "*" "[^.]*" part

replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old
