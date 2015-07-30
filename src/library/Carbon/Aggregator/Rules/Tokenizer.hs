module Carbon.Aggregator.Rules.Tokenizer (
                                           Token(..)
                                         , parseTokenize
                                         ) where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString
import Control.Applicative hiding (Const)

data Token = Sep
           | Const ByteString
           | SkipMany
           | Group ByteString
           deriving (Show, Eq)

parseTokenize :: Parser [Token]
parseTokenize = do
    many $  parseSep
        <|> parseGroup
        <|> parseSkipMany
        <|> parseConst
    where
        parseSep      = char '.' >> return Sep
        parseGroup    = fmap Group (char '<' *> takeWhile1 (/='>') <* char '>')
        parseSkipMany = char '*' >> return SkipMany
        parseConst    = Const <$> takeWhile1 nonCommand
            where
                nonCommand '.' = False
                nonCommand '<' = False
                nonCommand  _  = True
