{-# LANGUAGE FlexibleContexts #-}

module Carbon.Aggregator.Config.Loader (
                                         parseAggregatorConfig
                                       ) where

import Data.ConfigFile (ConfigParser(..), CPError, CPErrorData(..), emptyCP)
import qualified Data.ConfigFile as CF
import Control.Monad (join, when)
import Control.Monad.Except (liftIO, throwError, MonadError)
import Control.Monad.Trans.Except (runExceptT)
import System.FilePath (takeFileName)
import Data.Maybe (isNothing, fromJust)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

import Carbon.Aggregator.Config

getValue :: (CF.Get_C a, MonadError CF.CPError m) => CF.ConfigParser -> InstanceName -> CF.OptionSpec -> m a
getValue cp Nothing option = CF.get cp "aggregator" option
getValue cp (Just instanceName) option = do
    if CF.has_option cp ("aggregator:" ++ instanceName) option
        then
            CF.get cp ("aggregator:" ++ instanceName) option
        else
            getValue cp Nothing option

parseAggregatorConfig :: FilePath -> InstanceName -> IO (Either String CarbonAggregatorConfig)
parseAggregatorConfig path maybeInstanceName = do
    econf <- runExceptT $ do
        -- ConfigFile.emptyCP uses 'toLower' as default keys transformation function.
        -- It means it won't be possible to lookup for keys like "LINE_RECEIVER_INTERFACE"
        -- Pass 'id' instead to search exact given key
        cp <- join $ liftIO $ CF.readfile emptyCP{optionxform = id} path

        let confDir = optional $ getValue cp maybeInstanceName "CONF_DIR"
        lineReceiverInterface <- getValue cp maybeInstanceName "LINE_RECEIVER_INTERFACE"
        lineReceiverPort <- getValue cp maybeInstanceName "LINE_RECEIVER_PORT"
        pickleReceiverInterface <- getValue cp maybeInstanceName "PICKLE_RECEIVER_INTERFACE"
        pickleReceiverPort <- getValue cp maybeInstanceName "PICKLE_RECEIVER_PORT"
        let aggregationRulesPath = fromEither
                                    "aggregation-rules.conf" $
                                    getValue cp maybeInstanceName "AGGREGATION_RULES"
        let rewriteRulesPath = fromEither
                                    "rewrite-rules.conf" $
                                    getValue cp maybeInstanceName "REWRITE_RULES"
        maxDataPointsPerMsg <- getValue cp maybeInstanceName "MAX_DATAPOINTS_PER_MESSAGE"
        maxAggregationIntervals <- getValue cp maybeInstanceName "MAX_AGGREGATION_INTERVALS"
        destinationsString <- getValue cp maybeInstanceName "DESTINATIONS"
        let destinations = parseDestinations destinationsString
        when (isNothing destinations) $ throwError (ParseError "Couldn't parse DESTINATIONS", "")

        let carbonMetricPrefix = fromEither
                                    "carbon" $
                                    getValue cp maybeInstanceName "CARBON_METRIC_PREFIX"

        let carbonMetricInterval = fromEither
                                    60 $
                                    getValue cp maybeInstanceName "CARBON_METRIC_INTERVAL"

        return $! CarbonAggregatorConfig {
            configConfDir = confDir,
            configLineReceiverInterface = lineReceiverInterface,
            configLineReceiverPort = lineReceiverPort,
            configPickleReceiverInterface = pickleReceiverInterface,
            configPickleReceiverPort = pickleReceiverPort,
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

optional :: Either e a -> Maybe a
optional (Right a) = Just a
optional _ = Nothing

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
