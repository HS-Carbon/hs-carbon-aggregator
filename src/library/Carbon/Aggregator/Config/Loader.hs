module Carbon.Aggregator.Config.Loader (
                                         parseAggregatorConfig
                                       ) where

import Data.ConfigFile (ConfigParser(..), CPError, CPErrorData(..), emptyCP)
import qualified Data.ConfigFile as CF
import Control.Monad.Error (runErrorT, liftIO, join)
import System.FilePath (takeFileName)

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

        return CarbonAggregatorConfig {
            configLineReceiverInterface = lineReceiverInterface,
            configLineReceiverPort = lineReceiverPort,
            configAggregationRulesPath = aggregationRulesPath,
            configRewriteRulesPath = rewriteRulesPath,
            -- TODO: parse destinations
            configDestinations = [],
            configMaxDataPointsPerMsg = maxDataPointsPerMsg,
            configMaxAggregationIntervals = maxAggregationIntervals
        }

    case econf of
        Left e -> return $ Left $ explainCPError e
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
