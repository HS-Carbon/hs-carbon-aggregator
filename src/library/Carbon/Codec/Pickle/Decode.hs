{-# LANGUAGE ScopedTypeVariables #-}

module Carbon.Codec.Pickle.Decode (
                                    readPickled
                                  , decodePickle
                                  ) where

import Carbon

import Language.Python.Pickle
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lex.Fractional as Fractional
import Data.Binary
import Data.Maybe
import System.IO

readPickled :: Handle -> IO (Maybe [MetricTuple])
readPickled handle = do
    -- read 4 bytes size
    msgSize :: Word32 <- decode <$> BL.hGet handle 4
    -- read metrics message
    msg <- B.hGet handle (fromIntegral msgSize)
    -- and decode
    case decodePickle msg of
        Left err -> print err >> return Nothing
        Right res -> return . Just $! res

decodePickle :: ByteString -> Either String [MetricTuple]
decodePickle pickled = do
    let unpickled = unpickle pickled
    case unpickled of
        Left err -> fail err
        Right (List values) -> return $ mapMaybe mkMetricTuple values
        _ -> fail "Unpickled data can not be cast to [MetricTuple]"

mkMetricTuple :: (Monad m) => Value -> m MetricTuple
mkMetricTuple (Tuple [BinString path, Tuple [valTimestamp, valValue]]) = do
    timestamp <- pickleToTimestamp valTimestamp
    value <- pickleToValue valValue
    return $ metricTuple path timestamp value
mkMetricTuple v = fail $ "Can't parse MetricTuple from " ++ show v
{-# SPECIALIZE mkMetricTuple :: Value -> Maybe MetricTuple #-}

pickleToTimestamp :: (Monad m) => Value -> m Timestamp
pickleToTimestamp (BinLong timestamp) = return timestamp
pickleToTimestamp (BinInt timestamp) = return timestamp
pickleToTimestamp v = fail $ "Unable to parse timestamp from " ++ show v
{-# SPECIALIZE pickleToTimestamp :: Value -> Maybe Timestamp #-}

pickleToValue :: (Monad m) => Value -> m MetricValue
-- By default metric value sent as string.
pickleToValue (BinString svalue) = do
    case Fractional.readDecimal svalue of
        Just (value, _rest) -> return value
        Nothing -> fail $ "Unable to parse metric value " ++ show svalue
pickleToValue (BinFloat value) = return value
pickleToValue (BinLong value) = return $ fromIntegral value
pickleToValue (BinInt value) = return $ fromIntegral value
pickleToValue v = fail $  "Unable to parse metric value from " ++ show v
{-# SPECIALIZE pickleToValue :: Value -> Maybe MetricValue #-}
