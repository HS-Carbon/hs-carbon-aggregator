module Carbon.Aggregator.Config (
                                  InstanceName
                                , CarbonDestination(..)
                                , destinationHost
                                , destinationPort
                                , CarbonAggregatorConfig(..)
                                ) where

type InstanceName = Maybe String

data CarbonDestination = CarbonDestination InstanceName String Int

destinationHost :: CarbonDestination -> String
destinationHost (CarbonDestination _ host _) = host

destinationPort :: CarbonDestination -> Int
destinationPort (CarbonDestination _ _ port) = port

data CarbonAggregatorConfig = CarbonAggregatorConfig {
    configConfDir :: Maybe FilePath,
    configLineReceiverInterface :: String,
    configLineReceiverPort :: Int,
    configPickleReceiverInterface :: String,
    configPickleReceiverPort :: Int,
    configAggregationRulesPath :: FilePath,
    configRewriteRulesPath :: FilePath,
    configDestinations :: [CarbonDestination],
    configMaxDataPointsPerMsg :: Int,
    configMaxAggregationIntervals :: Int,
    configCarbonMetricPrefix :: String,
    configCarbonMetricInterval :: Int
}
