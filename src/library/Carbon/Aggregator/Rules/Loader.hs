{-# LANGUAGE OverloadedStrings #-}

module Carbon.Aggregator.Rules.Loader (
                                        loadRules
                                      ) where

import Carbon.Aggregator.Rules (Rule, parseRuleDefinition)

import Data.Either (partitionEithers)
import Data.Tuple (swap)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Control.DeepSeq (force)

type MalformedDefinition = BS.ByteString

loadRules :: FilePath -> IO ([Rule], [MalformedDefinition])
loadRules path = do
    content <- BS.readFile path
    let contentLines = BS8.lines content
    let meaningfulLines = filter meaningful contentLines
    let result = partitionEithers $ map readRule meaningfulLines
    return . force $ swap result
    where
        meaningful :: BS.ByteString -> Bool
        meaningful line = not ("#" `BS.isPrefixOf` line || BS.null line)

        readRule :: BS.ByteString -> Either BS.ByteString Rule
        readRule line = case parseRuleDefinition line of
            Nothing -> Left line
            Just rule -> Right rule
