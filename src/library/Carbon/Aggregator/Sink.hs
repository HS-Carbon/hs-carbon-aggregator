module Carbon.Aggregator.Sink (
                                runSink
                              , ParallelismLevel
                              , Host
                              , Port
                              ) where

import Control.Monad
import Control.Concurrent.STM
import Control.Parallel.Strategies (using, parListChunk, rdeepseq)
import Network.Socket
import System.IO
import Carbon

type ParallelismLevel = Int
type Host = String
type Port = Int

runSink :: ParallelismLevel -> Host -> Port -> TChan [MetricTuple] -> ([MetricTuple] -> Handle -> IO ()) -> IO ()
runSink parallelismLevel host port outchan sinkHandler = do
    addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serverAddr)

    h <- socketToHandle sock WriteMode
    -- We're going to set buffering to BlockBuffering and then
    -- explicitly call hFlush after each message.
    hSetBuffering h (BlockBuffering Nothing)

    forever $ do
        metrics <- atomically $ readTChan outchan
        -- Metrics are whnf'd. We evaluate them in parallel to gain performance.
        let metrics' = metrics `using` parListChunk parallelismLevel rdeepseq
        -- What happens if connection breaks during metrics send? Well, we'll loose theese metrics.
        sinkHandler metrics' h
        hFlush h
