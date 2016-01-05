module Main (main) where

import Carbon.Aggregator.Server
import Carbon.Aggregator.Sink
import Carbon.Codec.Pickle
import Carbon.Aggregator.Processor
import Carbon.Aggregator.Config
import Carbon.Aggregator.Config.Loader
import Carbon.Aggregator.Rules.Loader
import Carbon.Stats
import System.FilePath (combine)
import Network.Socket
import Network.HostName
import Control.Monad (forever, unless, forM_)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.Time.Clock.POSIX

import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
    let confPath = combine "/opt/graphite/conf"
    econf <- parseAggregatorConfig $ confPath "carbon.conf"
    case econf of
        Left errorMsg -> do
            putStrLn errorMsg
            putStrLn "You can find more about configuration files here: http://graphite.readthedocs.org/en/latest/config-carbon.html"
        Right conf -> do
            proceedWithConfig confPath conf

proceedWithConfig :: (FilePath -> FilePath) -> CarbonAggregatorConfig -> IO ()
proceedWithConfig confPath conf = do
    smap <- newStatsMap

    bm <- newBuffersManagerIO
    -- Channel with metrics to be sent to downstream
    outchan <- newBroadcastTChanIO

    let port = configLineReceiverPort conf

    let maxIntervals = configMaxAggregationIntervals conf
    let destinations = configDestinations conf

    forM_ destinations $ \destination -> do
        let sinkHost = destinationHost destination
        let sinkPort = destinationPort destination
        outchan' <- atomically $ dupTChan outchan
        forkIO $ runSink sinkHost sinkPort outchan' writePickled

    forkIO . forever $ do
        now <- round `fmap` getPOSIXTime
        metrics <- collectAggregatedIO maxIntervals now bm
        atomically $ writeTChan outchan metrics
        recordAggregatedDataPoint smap $ (length metrics)
        -- Sleep 1 second
        threadDelay 1000000

    serverHostName <- getHostName
    let statsConfig = StatsConfig {
        metricPrefix = configCarbonMetricPrefix conf,
        hostname = serverHostName,
        -- TODO: Carbon instance names not supported yet
        instanceName = Nothing
    }
    forkIO . forever $ do
        now <- round `fmap` getPOSIXTime
        metrics <- collectSelfStatsIO statsConfig smap bm now
        atomically $ writeTChan outchan metrics

        threadDelay $ 1000000 * configCarbonMetricInterval conf

    (rules, malformedRules) <- loadRules $ confPath (configAggregationRulesPath conf)
    putStrLn $ "Aggregation rules loaded. Total rules count = " ++ show (length rules)
    unless (null malformedRules) $ do
        putStrLn "Some definitions contain errors and could not be parsed:"
        mapM_ B.putStrLn malformedRules

    let reportMetricsCount = recordReceivedDataPoint smap

    -- TODO: there should be TCP server for each 'aggregator:x' section in config.
    putStrLn $ "Server is running on port " ++ show port
    runTCPServer (handlePickleConnection reportMetricsCount rules outchan bm) (iNADDR_ANY, port)
