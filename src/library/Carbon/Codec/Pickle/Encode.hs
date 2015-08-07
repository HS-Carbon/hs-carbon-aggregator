module Carbon.Codec.Pickle.Encode (
                                    writePickled
                                  , encodePickle
                                  ) where

import Carbon
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.Monoid
import Data.Foldable
import Data.Binary

import System.IO (Handle)

writePickled :: [MetricTuple] -> Handle -> IO ()
writePickled mtuples handle = do
    let pickled = L.toStrict $ encodePickle mtuples
    let e = encode $ B.length pickled
    L.hPut handle e
    B.hPut handle pickled

encodePickle :: [MetricTuple] -> L.ByteString
encodePickle mtuples = toLazyByteString $ pickledBuilder mtuples

pMark, pString, pLong, pEndTuple, pAppend, pList, pStop :: Builder
pMark = char8 '('
pString = char8 'S'
pLong = char8 'L'
pEndTuple = char8 't'
pAppend = char8 'a'
pList = char8 'l'
pStop = char8 '.'

pickledBuilder :: [MetricTuple] -> Builder
pickledBuilder mtuples = mconcat [
    pMark,
    pList,
    foldMap encodeMetricTuple mtuples,
    pStop
    ]

encodeMetricTuple :: MetricTuple -> Builder
encodeMetricTuple (MetricTuple path (DataPoint timestamp value)) = mconcat [
    -- start the outer tuple
    pMark,

    quoteString $ byteString path,

    -- start the inner tuple
    pMark,
    encodeLong timestamp,
    quoteString $ doubleDec value, -- For some reason value is encoded as string
    pEndTuple,

    pEndTuple,
    pAppend
    ]

quoteString :: Builder -> Builder
quoteString s = pString <> char8 '\'' <> s <> char8 '\'' <> char8 '\n'

encodeLong :: Int -> Builder
encodeLong val = pLong <> intDec val <> char8 'L' <> char8 '\n'
