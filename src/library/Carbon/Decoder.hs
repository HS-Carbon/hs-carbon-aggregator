{-# LANGUAGE OverloadedStrings #-}

module Carbon.Decoder (
                        decodePlainText
                      )

where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lex.Fractional as Fractional
import qualified Data.ByteString.Lex.Integral as Integral
import Data.ByteString.Search (split)
import Carbon

decodePlainText :: ByteString -> Maybe MetricTuple
decodePlainText string = do
    let bits = split " " string
    if length bits /= 3
        then Nothing
        else do
            let [path, sval, stime] = bits
            val <- fst <$> Fractional.readDecimal sval
            time <- fst <$> Integral.readDecimal stime
            return $ metricTuple path time val
