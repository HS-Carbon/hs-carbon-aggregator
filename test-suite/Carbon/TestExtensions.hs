{-# LANGUAGE StandaloneDeriving #-}

module Carbon.TestExtensions () where

import Carbon

deriving instance Show DataPoint
deriving instance Eq DataPoint
