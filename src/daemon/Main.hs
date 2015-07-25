module Main (main) where

import Carbon.Aggregator
import Carbon.Aggregator.Server
import Carbon.Aggregator.Processor
import Network.Socket
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import Control.Parallel.Strategies

import Data.ByteString.Char8

main :: IO ()
main = do
    -- Program-wide TVar to handle metric buffers state
    tbm <- newTVarIO newBuffersManager
    -- Channel with metrics to be sent to downstream
    outchan <- newTChanIO

    let maxIntervals = 5
    let parallelismLevel = 4

    forkIO . forever $ do
        metrics <- atomically $ readTChan outchan
        -- Metrics are whnf'd. We evaluate them in parallel to gain performance.
        let metrics' = metrics `using` parListChunk parallelismLevel rdeepseq
        -- TODO: serialize metrics and send to downstream
        return ()

    forkIO . forever $ do
        let now = 1001
        metrics <- atomically $ collectAggregatedT maxIntervals now tbm
        atomically $ writeTChan outchan metrics
        -- Sleep 1 second
        threadDelay 1000000

    let rule = fromJust . parseRuleDefinition $ pack "metric-sum (10) = sum metric"
    runTCPServer (handleConnection [rule] outchan tbm) (iNADDR_ANY, 8082)

    return ()
