{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Carbon.Aggregator.Sink (
                                runSink
                              , Host
                              , Port
                              ) where

import Control.Monad
import Control.Concurrent.STM
import Control.Exception (catch)
import Control.Concurrent (threadDelay)
import Network.Socket
import System.IO
import Data.IORef
import Carbon

type Host = String
type Port = Int

runSink :: Host -> Port -> TChan [MetricTuple] -> ([MetricTuple] -> Handle -> IO ()) -> IO ()
runSink host port outchan sinkHandler = do
    addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
    let serverAddr = head addrInfo

    -- We read next metrics batch into this IO ref.
    -- If something goes wrong, metrics won't be lost,
    -- they will be kept in this variable and sent with next sink attempt.
    bufMetrics <- newIORef Nothing

    forever $ do
        runSinkInternal bufMetrics serverAddr outchan sinkHandler
        `catch` \(e :: IOError) -> do
            -- Original Carbon Aggregator starts with 1 sec delay and grow it up to 5 sec.
            print e
            threadDelay $ 2 * 1000000

runSinkInternal :: IORef (Maybe [MetricTuple]) -> AddrInfo -> TChan [MetricTuple] -> ([MetricTuple] -> Handle -> IO ()) -> IO ()
runSinkInternal bufMetrics serverAddr outchan sinkHandler = do
    sock <- reconnectingConnect 5 serverAddr

    h <- socketToHandle sock WriteMode
    -- We're going to set buffering to BlockBuffering and then
    -- explicitly call hFlush after each message.
    hSetBuffering h (BlockBuffering Nothing)

    forever $ do
        readIORef bufMetrics >>= \case
            Nothing -> do
                metrics <- atomically $ readTChan outchan
                -- Store read metrics before running sink
                unless (null metrics) $ do
                    writeIORef bufMetrics (Just metrics)
                    sinkHandler metrics h
            Just metrics -> do
                -- We have buffered values, no need to read next batch
                sinkHandler metrics h

        hFlush h
        writeIORef bufMetrics Nothing

reconnectingConnect :: Int -> AddrInfo -> IO Socket
reconnectingConnect maxDelay serverAddr = do
    connectInternal 1
    where
        connectInternal currentDelay = do
            sock <- socket (addrFamily serverAddr) Stream defaultProtocol
            setSocketOption sock KeepAlive 1
            connect sock (addrAddress serverAddr)
            return sock
            `catch` \(e :: IOError) -> do
                putStrLn $ "Error connecting to carbon-cache on " ++ show (addrAddress serverAddr) ++ ", retrying in " ++ show currentDelay ++ " sec; error was: " ++ show e
                threadDelay $ currentDelay * 1000000
                connectInternal $ min (currentDelay + 1) maxDelay
