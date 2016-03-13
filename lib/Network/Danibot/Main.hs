{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Danibot.Main (
        mainWith
    ) where

import Data.Function ((&))
import qualified Data.ByteString as Bytes
import Data.Text (Text)
import Data.Aeson (FromJSON,eitherDecodeStrict')

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Control.Foldl as Foldl

import Control.Concurrent.Conceit

import GHC.Generics

import Options.Applicative 
import qualified Options.Applicative as Options

import Network.Danibot.Slack 
import Network.Danibot.Slack.Types (introUrl,introChat)
import Network.Danibot.Slack.API (startRTM)
import Network.Danibot.Slack.RTM (fromWSSURI,loopRTM)

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

exceptionalMain :: (Text -> IO Text) -> ExceptT String IO ()
exceptionalMain handler = do
    args <- liftIO (Options.execParser parserInfo)
    conf <- ExceptT (do
        bytes <- Bytes.readFile (confPath args)
        pure (eitherDecodeStrict' bytes))
    intro <- ExceptT (startRTM (slack_api_token conf))
    liftIO (print intro)
    endpoint <- fromWSSURI (introUrl intro)
              & either throwE pure
    (workChan,workerAction) <- liftIO (worker handler)
    (chatState,source) <- liftIO (makeChatState (introChat intro))
    let theEventFold = eventFold workChan chatState
    liftIO (_runConceit (_Conceit (loopRTM theEventFold source endpoint) 
                         *> _Conceit workerAction))

mainWith :: (Text -> IO Text) -> IO ()
mainWith handler = do
    final <- runExceptT (exceptionalMain handler)
    case final of
        Left err -> print err
        Right () -> pure ()

