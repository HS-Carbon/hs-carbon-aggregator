module Carbon.Aggregator.Sink (
                                runSink
                              , Host
                              , Port
                              ) where

import Control.Monad
import Control.Concurrent.STM
import Network.Socket
import System.IO
import Carbon

type Host = String
type Port = Int

runSink :: Host -> Port -> TChan [MetricTuple] -> ([MetricTuple] -> Handle -> IO ()) -> IO ()
runSink host port outchan sinkHandler = do
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
        -- What happens if connection breaks during metrics send? Well, we'll loose theese metrics.
        sinkHandler metrics h
        hFlush h
