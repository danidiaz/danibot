{-# LANGUAGE OverloadedStrings #-}

module Network.Danibot.Slack (
      worker
    , makeChatState
    , eventFold
    ) where

import Data.Text (Text)
import Data.Char
import qualified Data.Attoparsec.Text as Atto

import Control.Lens
import Control.Exception
import Control.Monad
import Streaming (Stream)
import Streaming.Prelude (Of)
import qualified Streaming.Prelude as Streaming
import Control.Foldl (FoldM(..))
import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

import Network.Danibot.Slack.Types 

worker :: (Text -> IO Text) -> IO (TChan (Text,Text -> IO ()), IO ()) 
worker handler = do
    chan <- atomically newTChan  
    let go = forever (do (task,post) <- atomically (readTChan chan)
                         forkIO (do
                             result <- handler task
                             post result))
    pure (chan,go)                            

eventFold :: TChan (Text,Text -> IO ()) 
          -> ChatState
          -> FoldM IO Event ()
eventFold pool cs =
    FoldM reactToEvent (pure InitialState) (\_ -> pure ())
    where
    reactToEvent protocolState event =
        case (protocolState,event) of
            (InitialState,HelloEvent) -> 
                pure NormalState
            (InitialState,_) -> 
                throwIO (userError "wrong start")
            (NormalState,MessageEvent (Message _ (Right (UserMessage channel_ user_ text_ NotMe)))) -> do
                currentcs <- atomically (readTVar (chatVar cs)) 
                let whoami = identity (self currentcs) 
                    send = sendMessageToChannel cs channel_
                if has (ims.ix channel_) currentcs 
                  then -- IM message?
                    case isDirectedTo text_ of
                        Just (target,text') | user_ /= whoami && target == whoami -> do
                            atomically (writeTChan pool 
                                                   (text',send))
                        Nothing             | user_ /= whoami -> do
                            atomically (writeTChan pool 
                                                   (text_,send))
                        _ -> pure ()
                  else -- message in general channel? 
                    case isDirectedTo text_ of
                        Just (target,text') | user_ /= whoami && target == whoami -> do
                            atomically (writeTChan pool 
                                                   (text',send . addressTo user_))
                        _ -> pure ()
                pure NormalState
            _ -> do
                pure NormalState

data ChatState = ChatState
    {
       chatVar :: TVar Chat 
    ,  nextMsgIdVar :: TVar Integer 
    ,  outboundChan :: TChan OutboundMessage 
    } 

makeChatState :: Chat -> IO (ChatState, Stream (Of OutboundMessage) IO ())
makeChatState c = do
    chatVar' <- atomically (newTVar c)
    nextMsgIdVar' <- atomically (newTVar 0)
    outboundChan' <- atomically newTChan  
    pure (ChatState chatVar' nextMsgIdVar' outboundChan'
         ,Streaming.repeatM (atomically (readTChan outboundChan')))

data ProtocolState = 
      InitialState
    | NormalState

isDirectedTo :: Text -> Maybe (Text,Text)
isDirectedTo txt = case Atto.parse mentionParser txt of
        Atto.Done rest userId_ -> Just (userId_,rest)
        _                     -> Nothing
    where
    mentionParser = 
        Atto.string "<@" 
        *>  
        Atto.takeWhile isAlphaNum 
        <* 
        Atto.string ">:"
        <* 
        Atto.skipSpace

addressTo :: Text -> Text -> Text
addressTo uid msg = mconcat ["<@",uid,">: ",msg]

sendMessageToChannel :: ChatState -> Text -> Text -> IO () 
sendMessageToChannel cstate channelId_ msg = do
    atomically (do
         i <- readTVar (nextMsgIdVar cstate)
         modifyTVar' (nextMsgIdVar cstate) succ 
         writeTChan (outboundChan cstate) 
                    (OutboundMessage i channelId_ msg)) 
       
