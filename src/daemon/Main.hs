module Main (main) where

import Carbon.Aggregator.Server
import Carbon.Aggregator.Processor
import Network.Socket
import Control.Concurrent.STM

main :: IO ()
main = do
    -- Program-wide TVar to handle metric buffers state
    bm <- newTVarIO newBuffersManager
    runTCPServer (handleConnection bm) (iNADDR_ANY, 8082)
