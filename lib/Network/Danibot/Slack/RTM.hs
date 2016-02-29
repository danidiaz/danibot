{-# LANGUAGE OverloadedStrings #-}

module Network.Danibot.Slack.RTM (
          fromWSSURI
        , loopRTM
    ) where

import qualified Data.Monoid.Cancellative as Textual
import qualified Data.Monoid.Textual as Textual
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString 
import Data.Aeson (eitherDecode',encode)
import Control.Monad
import Control.Monad.Trans.Except
import Control.Exception
import Control.Concurrent.Conceit
import qualified Wuss
import qualified Network.WebSockets as Webs

import Network.Danibot.Slack.Types(Wire(..),Event(..),OutboundMessage(..))

data WSSEndpoint = WSSEndpoint
    {
      host :: Text
    , path :: Text 
    } deriving (Show)

fromWSSURI :: Text -> Either String WSSEndpoint
fromWSSURI uri = 
    case Textual.stripPrefix "wss://" uri of
        Nothing   -> Left "malformed wss uri"
        Just uri' -> case Textual.break_ True (=='/') uri' of
            (host,web) -> Right (WSSEndpoint host web)

ws :: (Event -> IO ()) 
   -> IO OutboundMessage 
   -> Webs.ClientApp ()
ws handler emitter connection = 
    let conceited =
            (_Conceit (forever (do
                outbound <- emitter
                let outboundBytes = encode (Wire outbound)
                Webs.sendBinaryData connection outboundBytes)))
            *> 
            (_Conceit (forever (do
                message <- Webs.receiveData connection
                case eitherDecode' message of
                    Left errmsg -> 
                        throwIO (userError ("Malformed msg: " ++ errmsg))
                    Right (Wire event) -> 
                        handler event)))
    in _runConceit conceited

loopRTM :: WSSEndpoint -> (Event -> IO ()) -> IO OutboundMessage -> IO ()
loopRTM (WSSEndpoint host path) eventHandler messageEmitter = do  
    print (host,path)
    Wuss.runSecureClient (Textual.fromText host) 
                         443
                         (Textual.fromText path) 
                         (ws eventHandler messageEmitter) 
