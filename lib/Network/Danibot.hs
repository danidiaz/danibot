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
      set (Wreq.param "token") [token]
    $ Wreq.defaults

checkResp :: Value -> Either Text Text
checkResp v =
    case (v^?key "ok"._Bool,v^?key "url"._String,v^?key "error"._String) of
        (Just True ,Just url,_       ) -> Right url
        (Just False,_       ,Just err) -> Left err
        _                              -> Left "malformed response"

-- copied from wuss examples
ws :: Webs.ClientApp ()
ws connection = do
    putStrLn "Connected!"
    forever (do
        message <- Webs.receiveData connection
        print (message :: Text))

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
            case checkResp respJSON of
                Left err  -> print ("there was an error" <> err)
                Right url -> do
                    print url 
                    case (Textual.stripPrefix "wss://" url) of
                        Nothing -> print "oops!"
                        Just rest -> do
                            let (host,web) = Textual.break_ True (=='/') rest
                            print (host,web)
                            Wuss.runSecureClient (Textual.fromText host) 443 (Textual.fromText web) ws

