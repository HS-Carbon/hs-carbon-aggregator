module Carbon (
                MetricPath
              , Timestamp
              , MetricValue
              , DataPoint(..)
              ) where

import Data.ByteString (ByteString)

type MetricPath = ByteString

type Timestamp = Int
type MetricValue = Double
data DataPoint = DataPoint { timestamp :: Timestamp, value :: MetricValue }
