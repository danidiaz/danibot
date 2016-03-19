{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Danibot.Slack.RTM (
          fromWSSURI
        , loopRTM
    ) where

import qualified Data.Monoid.Cancellative as Textual
import qualified Data.Monoid.Textual as Textual
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString 
import qualified Data.ByteString.Lazy.Char8 as Bytes.Lazy.Char8
import Data.Aeson (eitherDecode',encode)
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Control.Concurrent.Conceit
import Control.Foldl (impurely,FoldM)
import Streaming (Stream)
import Streaming.Prelude (Of)
import qualified Streaming as Streaming
import qualified Streaming.Prelude as Streaming
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

{-| Connect to the Slack RTM websocket.		

-}
ws :: FoldM IO Event () -- ^ Fold for processing incoming messages.
   -> Stream (Of OutboundMessage) IO () -- ^ Stream that produces outbound messages.
   -> Webs.ClientApp ()
ws eventFold messageStream connection = 
    let conceited =
            (_Conceit (impurely Streaming.foldM_ eventFold eventStream))
            *> 
            (_Conceit (Streaming.mapM_ sendMessage messageStream))
        eventStream = forever (do
            message <- liftIO (Webs.receiveData connection)
            case eitherDecode' message of
                Left errmsg -> 
                    liftIO (throwIO (userError ("Malformed msg: " ++ errmsg)))
                Right (Wire event) -> do
                    liftIO (Bytes.Lazy.Char8.putStrLn (encode event))
                    Streaming.yield event)
        sendMessage = 
            Webs.sendTextData connection . encode . Wire
    in _runConceit conceited


loopRTM :: FoldM IO Event ()
        -> Stream (Of OutboundMessage) IO ()
        -> WSSEndpoint 
        -> IO ()
loopRTM eventHandler messageEmitter (WSSEndpoint host path) = do  
    Wuss.runSecureClient (Textual.fromText host) 
                         443
                         (Textual.fromText path) 
                         (ws eventHandler messageEmitter) 
