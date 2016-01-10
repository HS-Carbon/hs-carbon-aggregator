module Carbon.Aggregator.Config (
                                  CarbonDestination(..)
                                , destinationHost
                                , destinationPort
                                , CarbonAggregatorConfig(..)
                                ) where

data CarbonDestination = CarbonDestination (Maybe String) String Int

destinationHost :: CarbonDestination -> String
destinationHost (CarbonDestination _ host _) = host

destinationPort :: CarbonDestination -> Int
destinationPort (CarbonDestination _ _ port) = port

data CarbonAggregatorConfig = CarbonAggregatorConfig {
    configConfDir :: Maybe FilePath,
    configLineReceiverInterface :: String,
    configLineReceiverPort :: Int,
    configAggregationRulesPath :: FilePath,
    configRewriteRulesPath :: FilePath,
    configDestinations :: [CarbonDestination],
    configMaxDataPointsPerMsg :: Int,
    configMaxAggregationIntervals :: Int,
    configCarbonMetricPrefix :: String,
    configCarbonMetricInterval :: Int
}
