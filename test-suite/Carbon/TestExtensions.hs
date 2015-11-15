{-# LANGUAGE StandaloneDeriving #-}

module Carbon.TestExtensions () where

import Control.Concurrent.STM
import System.IO.Unsafe

import Carbon
import Carbon.Aggregator.Rules

deriving instance Show DataPoint
deriving instance Eq DataPoint
deriving instance Show MetricTuple
deriving instance Eq MetricTuple
deriving instance Eq Rule

instance (Show a) => Show (TVar a) where
    -- Holi macaroni, you shound't have seen it!
    show t = "TVar<" ++ (show $ unsafePerformIO (readTVarIO t)) ++ ">"
