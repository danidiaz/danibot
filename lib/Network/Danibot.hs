{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Danibot (
        defaultMain
    ) where

import GHC.Generics
import qualified Data.ByteString as Bytes
import Data.Text (Text)
import qualified Data.Monoid.Cancellative as Textual
import qualified Data.Monoid.Textual as Textual
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Lens
import qualified Network.Wreq as Wreq
import qualified Wuss
import qualified Network.WebSockets as Webs
import Data.Aeson.Lens
import Data.Aeson (Value(..),eitherDecodeStrict',FromJSON,toJSON)
import Options.Applicative 
import qualified Options.Applicative as Options

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
      Wreq.defaults
    & set (Wreq.param "token") [token]

checkResp :: Value -> Either String Text
checkResp v =
    case (v^?key "ok"._Bool,v^?key "url"._String,v^?key "error"._String) of
        (Just True ,Just url,_       ) -> Right url
        (Just False,_       ,Just err) -> Left (Textual.fromText err)
        _                              -> Left "malformed response"

-- copied from wuss examples
ws :: Webs.ClientApp ()
ws connection = do
    putStrLn "Connected!"
    forever (do
        message <- Webs.receiveData connection
        print (message :: Text))

exceptionalMain :: ExceptT String IO ()
exceptionalMain = do
    args  <- liftIO (Options.execParser parserInfo)
    bytes <- liftIO (Bytes.readFile (confPath args))
    conf  <- eitherDecodeStrict' bytes 
           & either throwE pure
    let options = authOptions (slack_api_token conf)
    respJSON <- liftIO (do
        resp <- Wreq.postWith options "https://slack.com/api/rtm.start" (toJSON ())
        view Wreq.responseBody <$> Wreq.asJSON resp)
    url  <- checkResp respJSON 
          & either throwE pure
    rest <- Textual.stripPrefix "wss://" url 
          & maybe (throwE "oops") pure
    let (host,web) = Textual.break_ True (=='/') rest
    liftIO (do
        print (host,web)
        Wuss.runSecureClient (Textual.fromText host) 443 (Textual.fromText web) ws))

defaultMain :: IO ()
defaultMain = do
    case runExceptT exceptionalMain of
        Left err -> print err
        Right () -> pure ()

