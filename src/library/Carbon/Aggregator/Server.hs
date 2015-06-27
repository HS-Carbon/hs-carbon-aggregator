{-# LANGUAGE ScopedTypeVariables #-}

module Carbon.Aggregator.Server

where

import Network.Socket hiding (socket)
import qualified Network.Socket as NS
import qualified Data.ByteString as B
import Control.Monad
import Control.Exception
import Control.Concurrent
import System.IO
import Control.Concurrent.STM
import Carbon
import Carbon.Decoder
import Carbon.Aggregator
import Carbon.Aggregator.Processor

type ServerHandler = Handle -> IO ()

-- iNADDR_ANY
runTCPServer :: ServerHandler -> (HostAddress, Int) -> IO ()
runTCPServer handler (host, port) = withSocketsDo $ bracket
    (bindPort host port)
    (\s -> do print "Wow-wow, shutting server down!"; sClose s)
    (forever . serve)
    where
        serve ssock = do
            (sock, _) <- acceptSafe ssock
            h <- socketToHandle sock ReadMode
            forkFinally (handler h) (\_ -> do print "Client gone..."; hClose h)

-- This is the only method related to Carbon. Should I extract everything else to dedicated module?
handleConnection :: [Rule] -> TChan (MetricPath, DataPoint) -> TVar BuffersManager -> ServerHandler
handleConnection rules outchan tbm h = do
    print "Wow! such connection!"
    hSetBuffering h LineBuffering
    loop
    where
        loop :: IO ()
        loop = do
            line <- B.hGetLine h
            print $ "Got the " ++ show line
            -- TODO: log connection? increment counter?
            let mm = decodePlainText line
            case mm of
                Just metric -> atomically $ do
                    bm <- readTVar tbm
                    let (bm', mmetric') = processAggregate rules bm metric
                    case mmetric' of
                        Just metric' -> writeTChan outchan metric'
                        Nothing -> return ()
                    writeTVar tbm bm'
                Nothing -> return ()

            loop


createSocket :: HostAddress -> Int -> IO Socket
createSocket host port = do
  sock <- NS.socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock $ SockAddrInet (fromIntegral port) host
  return sock

bindPort :: HostAddress -> Int -> IO Socket
bindPort host port = do
    sock <- createSocket host port
    listen sock (max 2048 maxListenQueue)
    return sock

acceptSafe :: Socket -> IO (Socket, SockAddr)
acceptSafe socket = loop
    where
        loop =
            accept socket `catch` \(_ :: IOException) -> do
                -- Sleep 1 second
                threadDelay 1000000
                loop
