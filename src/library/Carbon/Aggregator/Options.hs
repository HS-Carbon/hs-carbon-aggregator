module Carbon.Aggregator.Options (
                                   AggregatorOptions(..)
                                 , aggregatorOptsParser
                                 , withOptionsDo
                                 ) where

import Options.Applicative
import Data.Semigroup ((<>))

data AggregatorOptions = AggregatorOptions
    { configPath   :: Maybe FilePath
    , rulesPath    :: Maybe FilePath
    , instanceName :: Maybe String }

maybeStrOption :: Mod OptionFields String -> Parser (Maybe String)
maybeStrOption = optional . strOption

aggregatorOptsParser :: Parser AggregatorOptions
aggregatorOptsParser = AggregatorOptions
    <$> maybeStrOption
        ( long "config"
        <> short 'c'
        <> metavar "CONFIG"
        <> help "Use the given config file" )
    <*> maybeStrOption
        ( long "rules"
        <> metavar "RULES"
        <> help "Use the given aggregation rules file." )
    <*> maybeStrOption
        ( long "instance"
        <> metavar "INSTANCE"
        <> help "Manage a specific carbon instance" )

withOptionsDo :: (AggregatorOptions -> IO a) -> IO a
withOptionsDo maction = execParser (info (helper <*> aggregatorOptsParser) idm) >>= maction
