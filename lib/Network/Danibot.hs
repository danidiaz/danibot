{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Danibot (
        defaultMain
    ) where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson
import Options.Applicative

data Configuration = Configuration
    {
        slack_api_token :: Text
    } deriving (Generic,Show)

instance FromJSON Configuration

instance ToJSON Configuration

data Cmdline = Cmdline
    {
        configurationPath :: String
    } deriving (Show)

parserInfo :: ParserInfo Cmdline
parserInfo = 
    info (helper <*> parser) infoMod
  where
    parser = 
        Cmdline <$> strArgument (help "configuration file" <> metavar "CONF")
    infoMod = 
        fullDesc <> header "program desc" 

defaultMain :: IO ()
defaultMain = do
    cmdline <- execParser parserInfo
    print cmdline

