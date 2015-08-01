{-# LANGUAGE StandaloneDeriving #-}

module Carbon.TestExtensions () where

import Carbon
import Carbon.Aggregator.Rules

deriving instance Show DataPoint
deriving instance Eq DataPoint
deriving instance Eq Rule
