module Carbon.Aggregator.Config (
                                  CarbonDestination(..)
                                , CarbonAggregatorConfig(..)
                                ) where

data CarbonDestination = CarbonDestination String Int (Maybe String)

data CarbonAggregatorConfig = CarbonAggregatorConfig {
    configLineReceiverInterface :: String,
    configLineReceiverPort :: Int,
    configAggregationRulesPath :: FilePath,
    configRewriteRulesPath :: FilePath,
    configDestinations :: [CarbonDestination],
    configMaxDataPointsPerMsg :: Int,
    configMaxAggregationIntervals :: Int
}
