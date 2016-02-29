{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Danibot.Main (
        defaultMain
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Function ((&))
import qualified Data.ByteString as Bytes
import Data.Text (Text)
import Data.Aeson (FromJSON,eitherDecodeStrict')
import Options.Applicative 
import qualified Options.Applicative as Options

import GHC.Generics

import Network.Danibot.Slack.Types (introUrl)
import Network.Danibot.Slack.API (startRTM)
import Network.Danibot.Slack.RTM (fromWSSURI,loopRTM)
import Network.Danibot.Slack (mute)

data Conf = Conf
    {
        slack_api_token :: Text
    } deriving (Generic,Show)

instance FromJSON Conf

data Args = Args
    {
        confPath :: String
    } deriving (Show)

parserInfo :: Options.ParserInfo Args
parserInfo = 
    info (helper <*> parser) infoMod
  where
    parser = 
        Args <$> strArgument (help "configuration file" <> metavar "CONF")
    infoMod = 
        fullDesc <> header "program desc" 

exceptionalMain :: ExceptT String IO ()
exceptionalMain = do
    args <- liftIO (Options.execParser parserInfo)
    conf <- ExceptT (do
        bytes <- Bytes.readFile (confPath args)
        pure (eitherDecodeStrict' bytes))
    intro <- ExceptT (startRTM (slack_api_token conf))
    liftIO (print intro)
    endpoint <- fromWSSURI (introUrl intro)
              & either throwE pure
    liftIO (loopRTM endpoint print mute)

defaultMain :: IO ()
defaultMain = do
    final <- runExceptT exceptionalMain 
    case final of
        Left err -> print err
        Right () -> pure ()

