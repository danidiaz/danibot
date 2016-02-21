{-# LANGUAGE OverloadedStrings #-}

module Network.Danibot.Slack.RTM (
          fromWSSURI
        , loopRTM
    ) where

import Control.Monad
import qualified Data.Monoid.Cancellative as Textual
import qualified Data.Monoid.Textual as Textual
import Data.Text (Text)
import qualified Wuss
import qualified Network.WebSockets as Webs

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

-- copied from wuss examples
ws :: Webs.ClientApp ()
ws connection = do
    putStrLn "Connected!"
    forever (do
        message <- Webs.receiveData connection
        print (message::Text))

loopRTM :: WSSEndpoint -> IO ()
loopRTM (WSSEndpoint host path) = do  
    print (host,path)
    Wuss.runSecureClient (Textual.fromText host) 
                         443
                         (Textual.fromText path) 
                         ws
