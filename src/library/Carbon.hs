module Carbon (
                MetricPath
              , Timestamp
              , MetricValue
              , DataPoint(..)
              , MetricTuple(..)
              , metricTuple
              ) where

import Data.ByteString (ByteString)
import Control.Parallel.Strategies (NFData)

type MetricPath = ByteString

type Timestamp = Int
type MetricValue = Double

data DataPoint = DataPoint
                    {-# UNPACK #-} !Timestamp
                    {-# UNPACK #-} !MetricValue
instance NFData DataPoint

data MetricTuple = MetricTuple !MetricPath !DataPoint
instance NFData MetricTuple

metricTuple :: MetricPath -> Timestamp -> MetricValue -> MetricTuple
metricTuple path timestamp value = MetricTuple path (DataPoint timestamp value)
{-# INLINABLE metricTuple #-}
