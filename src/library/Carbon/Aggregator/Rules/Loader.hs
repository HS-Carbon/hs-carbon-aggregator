{-# LANGUAGE OverloadedStrings #-}

module Carbon.Aggregator.Rules.Loader (
                                        loadRules
                                      ) where

import Carbon.Aggregator.Rules (Rule)
import Carbon.Aggregator.Rules.Definition (parseRuleDefinition)

import Data.Either (partitionEithers)
import Data.Tuple (swap)
import qualified Data.ByteString.Char8 as B

type MalformedDefinition = B.ByteString

loadRules :: FilePath -> IO ([Rule], [MalformedDefinition])
loadRules path = do
    content <- B.readFile path
    let contentLines = B.lines content
    let meaningfulLines = filter meaningful contentLines
    let result = partitionEithers $ map readRule meaningfulLines
    return $! swap result
    where
        meaningful :: B.ByteString -> Bool
        meaningful line = not ("#" `B.isPrefixOf` line || B.null line)

        readRule :: B.ByteString -> Either B.ByteString Rule
        readRule line = case parseRuleDefinition line of
            Nothing -> Left line
            Just rule -> Right rule
