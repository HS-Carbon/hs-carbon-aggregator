module Main (main) where

import Carbon.Aggregator.Server
import Carbon.Aggregator.Processor
import Network.Socket
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

main :: IO ()
main = do
    -- Program-wide TVar to handle metric buffers state
    bm <- newTVarIO newBuffersManager
    -- Channel with metrics to be sent to downstream
    outchan <- newTChanIO

    forkIO . forever $ do
        (path, _) <- atomically $ readTChan outchan
        print $ "Wow, new metric " ++ show path ++ " is ready to be sent"

    runTCPServer (handleConnection [] outchan bm) (iNADDR_ANY, 8082)

    return ()
