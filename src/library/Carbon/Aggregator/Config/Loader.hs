module Carbon.Aggregator.Config.Loader (
                                         parseAggregatorConfig
                                       ) where

import Data.ConfigFile (ConfigParser(..), CPError, CPErrorData(..), emptyCP)
import qualified Data.ConfigFile as CF
import Control.Monad (join, when)
import Control.Monad.Error (runErrorT, liftIO, throwError)
import Control.Applicative ((<$>))
import System.FilePath (takeFileName)
import Data.Maybe (isNothing, fromJust)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

import Carbon.Aggregator.Config

parseAggregatorConfig :: FilePath -> IO (Either String CarbonAggregatorConfig)
parseAggregatorConfig path = do
    econf <- runErrorT $ do
        -- ConfigFile.emptyCP uses 'toLower' as default keys transformation function.
        -- It means it won't be possible to lookup for keys like "LINE_RECEIVER_INTERFACE"
        -- Pass 'id' instead to search exact given key
        cp <- join $ liftIO $ CF.readfile emptyCP{optionxform = id} path
        lineReceiverInterface <- CF.get cp "aggregator" "LINE_RECEIVER_INTERFACE"
        lineReceiverPort <- CF.get cp "aggregator" "LINE_RECEIVER_PORT"
        let aggregationRulesPath = fromEither
                                    "aggregation-rules.conf" $
                                    CF.get cp "aggregator" "AGGREGATION_RULES"
        let rewriteRulesPath = fromEither
                                    "rewrite-rules.conf" $
                                    CF.get cp "aggregator" "REWRITE_RULES"
        maxDataPointsPerMsg <- CF.get cp "aggregator" "MAX_DATAPOINTS_PER_MESSAGE"
        maxAggregationIntervals <- CF.get cp "aggregator" "MAX_AGGREGATION_INTERVALS"
        destinationsString <- CF.get cp "aggregator" "DESTINATIONS"
        let destinations = parseDestinations destinationsString
        when (isNothing destinations) $ throwError (ParseError "Couldn't parse DESTINATIONS", "")

        let carbonMetricPrefix = fromEither
                                    "carbon" $
                                    CF.get cp "aggregator" "CARBON_METRIC_PREFIX"

        let carbonMetricInterval = fromEither
                                    60 $
                                    CF.get cp "aggregator" "CARBON_METRIC_INTERVAL"

        return $! CarbonAggregatorConfig {
            configLineReceiverInterface = lineReceiverInterface,
            configLineReceiverPort = lineReceiverPort,
            configAggregationRulesPath = aggregationRulesPath,
            configRewriteRulesPath = rewriteRulesPath,
            configDestinations = fromJust destinations,
            configMaxDataPointsPerMsg = maxDataPointsPerMsg,
            configMaxAggregationIntervals = maxAggregationIntervals,
            configCarbonMetricPrefix = carbonMetricPrefix,
            configCarbonMetricInterval = carbonMetricInterval
        }

    case econf of
        Left e -> return $ Left (explainCPError e)
        Right conf -> return $ Right conf

    where
        explainCPError :: CPError -> String
        explainCPError (ParseError reason, _) = reason
        explainCPError (NoSection secion, _) = "Parse error (file '" ++ (takeFileName path) ++ "'): expected section '[" ++ secion ++ "]' wasn't found."
        explainCPError (NoOption option, _) = "Parse error (file '" ++ (takeFileName path) ++ "'): expected configuration option '" ++ option ++ "' wasn't found."
        explainCPError _ = "Parse error (file '" ++ (takeFileName path) ++ "'): unknown error."

fromEither :: a -> Either e a -> a
fromEither _ (Right a) = a
fromEither a (Left _) = a

parseDestinations :: String -> Maybe [CarbonDestination]
parseDestinations destinationsStr = do
    let destinations = map strip $ splitOn "," destinationsStr
    mapM parseDestination destinations
    where
        -- Very inefficient, but since we need it only once, wouldn't bother.
        strip = lstrip . rstrip
        lstrip = dropWhile (`elem` " \t")
        rstrip = reverse . lstrip . reverse

parseDestination :: String -> Maybe CarbonDestination
parseDestination destinationStr = makeDestination bits
    where
        bits = splitOn ":" destinationStr
        makeDestination [host, port] = CarbonDestination Nothing host <$> readMaybe port
        makeDestination [host, port, instanceId] = CarbonDestination (Just instanceId) host <$> readMaybe port
        makeDestination _ = Nothing
