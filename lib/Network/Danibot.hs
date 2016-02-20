{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Danibot (
        defaultMain
    ) where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson (Value(..),eitherDecodeStrict',FromJSON,toJSON)
import Options.Applicative 
import qualified Options.Applicative as Options
import Control.Lens
import qualified Data.ByteString as Bytes
import qualified Network.Wreq as Wreq
import qualified Network.HTTP.Client.TLS as TLS

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

authOptions :: Text -> Wreq.Options
authOptions token = 
      set (Wreq.param "token") [token]
    . set Wreq.manager (Left TLS.tlsManagerSettings)
    $ Wreq.defaults

defaultMain :: IO ()
defaultMain = do
    args <- Options.execParser parserInfo
    bytes <- Bytes.readFile (confPath args)
    case eitherDecodeStrict' bytes of
        Left e -> print e
        Right conf -> do
            print conf 
            let options = authOptions (slack_api_token conf)
            resp <- Wreq.postWith options "https://slack.com/api/rtm.start" (toJSON ())
            respJSON :: Value <- view Wreq.responseBody <$> Wreq.asJSON resp
            print respJSON


