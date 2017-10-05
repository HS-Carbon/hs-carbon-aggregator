{-# LANGUAGE StandaloneDeriving #-}

module Carbon.TestExtensions () where

import Data.IORef (IORef, readIORef)
import System.IO.Unsafe

import Carbon
import Carbon.Aggregator.Rules

deriving instance Show DataPoint
deriving instance Eq DataPoint
deriving instance Show MetricTuple
deriving instance Eq MetricTuple
deriving instance Eq Rule

instance (Show a) => Show (IORef a) where
    -- Holi macaroni, you shound't have seen it!
    show ref = "IORef<" ++ (show $ unsafePerformIO (readIORef ref)) ++ ">"
