{-# LANGUAGE OverloadedStrings #-}

module Carbon.Aggregator.Rules.Template (
                                          makeAggregatedMetricName
                                        ) where

import Carbon.Aggregator
import Carbon.Aggregator.Rules.Tokenizer
import Carbon.Compose

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Search (replace)
import Text.Regex.PCRE

{-- | We fallback to regular expressions here
because constructing Attoparsec Parser would require constant look aheads
and even worse — would be unreadable.
It could be much easier if Haskell had support for regex named groups, but it hasn't.
--}
regexFromToken :: Token -> ByteString
regexFromToken Sep = "\\."
regexFromToken (Const s) = s -- TODO: escape
regexFromToken SkipMany = "[^.]*"
regexFromToken (Group _) = "([^.]+?)"

regexFromTokens :: [Token] -> ByteString
regexFromTokens tokens = B.concat $ ["^"] ++ (map regexFromToken tokens) ++ ["$"]

extractGroups :: [Token] -> ByteString -> Maybe [(ByteString, ByteString)]
extractGroups tokens input = do
    let regex = regexFromTokens tokens
    r <- input =~~ regex :: Maybe [[ByteString]]
    -- first element is the source string, so drop it
    let groupValues = tail . head $ r
    let groupNames = mapGroups tokens
    return $ zip groupNames groupValues

    where
        mapGroups [] = []
        mapGroups ((Group g):ts) = g : mapGroups ts
        mapGroups (_:ts) = mapGroups ts

makeAggregatedMetricName :: InputPattern -> OutputPattern -> SourceMetricName -> Maybe AggregatedMetricName
makeAggregatedMetricName inp outp sm = do
    case parseOnlyTokenize inp of
        Left _ -> Nothing -- This case should really be handled during app start
        Right ts -> do
            groups <- extractGroups ts sm
            let replaceGroups = [(B.concat ["<", name, ">"], val) | (name, val) <- groups]
            return $ replaceAll replaceGroups outp
    where
        replaceAll groups = compose [strictReplace name val | (name, val) <- groups]

strictReplace :: ByteString -> ByteString -> ByteString -> ByteString
strictReplace old new str = toStrict $ replace old new str
