{-# LANGUAGE OverloadedStrings #-}

module Carbon.Aggregator.Rules.Definition (
                                            parseRuleDefinition
                                          ) where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Either.Convenient
import Control.Applicative

import Carbon.Aggregator
import Carbon.Aggregator.Rules

ruleDefinitionParser :: Parser Rule
ruleDefinitionParser = do
    skipSpace
    outp <- takeWhile1 (not .  isSpace)
    skipSpace
    freq <- char '(' *> decimal <* char ')'
    skipSpace
    char '='
    skipSpace
    method <- takeWhile1 (not . isSpace) >>= readMethod
    skipSpace
    inp <- takeWhile1 (not .  isSpace)
    return $ Rule inp outp method freq

parseRuleDefinition :: ByteString -> Maybe Rule
parseRuleDefinition = eitherToMaybe . parseOnly ruleDefinitionParser

readMethod :: Monad m => ByteString -> m AggregationMethod
readMethod "sum"   = return Sum
readMethod "avg"   = return Avg
readMethod "min"   = return Min
readMethod "max"   = return Max
readMethod "count" = return Count
readMethod _       = fail "Aggregation method not recognized"
