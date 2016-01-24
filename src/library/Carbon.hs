module Carbon (
                MetricPath
              , Timestamp
              , MetricValue
              , DataPoint(..)
              , MetricTuple(..)
              , metricTuple
              ) where

import Data.ByteString (ByteString)

type MetricPath = ByteString

type Timestamp = Int
type MetricValue = Double

data DataPoint = DataPoint
                    {-# UNPACK #-} !Timestamp
                    {-# UNPACK #-} !MetricValue

data MetricTuple = MetricTuple !MetricPath !DataPoint

metricTuple :: MetricPath -> Timestamp -> MetricValue -> MetricTuple
metricTuple path timestamp value = MetricTuple path (DataPoint timestamp value)
{-# INLINABLE metricTuple #-}
