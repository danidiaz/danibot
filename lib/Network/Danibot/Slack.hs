{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumDecimals #-}

module Network.Danibot.Slack (isDirectedTo,worker,dumbHandler,makeChatState,eventFold) where

import Data.Text (Text)
import Data.Char
import Data.Monoid
import qualified Data.Attoparsec.Text as Atto

import Control.Applicative
import Control.Lens
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Streaming (Stream)
import Streaming.Prelude (Of)
import qualified Streaming.Prelude as Streaming
import Control.Foldl (FoldM(..))
import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

import Streaming (Stream)

import Network.Danibot.Slack.Types 

dumbHandler :: Text -> IO Text
dumbHandler _ = do 
    threadDelay 1e6
    return "I don't do anything yet."

worker :: (Text -> IO Text) -> IO (TChan (Text,Text -> IO ()), IO ()) 
worker handler = do
    chan <- atomically newTChan  
    let worker = forever (do
                            (task,post) <- atomically (readTChan chan)
                            forkIO (do
                                result <- handler task
                                post result))
    pure (chan,worker)                            

eventFold :: TChan (Text,Text -> IO ()) 
          -> ChatState
          -> FoldM IO Event ()
eventFold pool chatState =
    FoldM (reactToEvent pool chatState) (pure InitialState) coda    
    where
    coda = \_ -> pure ()

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

reactToEvent :: TChan (Text,Text -> IO ()) 
             -> ChatState 
             -> ProtocolState 
             -> Event 
             -> IO ProtocolState
reactToEvent pool cs protocolState event =
    case (protocolState,event) of
        (InitialState,HelloEvent) -> 
            pure NormalState
        (InitialState,_) -> 
            throwIO (userError "wrong start")
        (NormalState,MessageEvent (Message _ (Right (UserMessage ch user txt NotMe)))) -> do
            currentcs <- atomically (readTVar (chatVar cs)) 
            let whoami = selfId (self currentcs) 
                send = sendMessage cs ch
            if has (ims.ix ch) currentcs  
              then do 
                case isDirectedTo txt of
                    Just (target,txt') | target == whoami -> do
                        atomically (writeTChan pool 
                                               (txt',send))
                    Nothing -> do
                        atomically (writeTChan pool 
                                               (txt ,send))
                    _ -> pure ()
              else 
                case isDirectedTo txt of
                    Just (target,txt') | target == whoami -> do
                        atomically (writeTChan pool 
                                               (txt',send . addressTo user))
                    _ -> pure ()
            pure NormalState
        _ -> print event *> pure NormalState

isDirectedTo :: Text -> Maybe (Text,Text)
isDirectedTo txt = case Atto.parse mentionParser txt of
        Atto.Done rest userId -> Just (userId,rest)
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

sendMessage :: ChatState -> Text -> Text -> IO () 
sendMessage cstate channelId msg = do
    atomically (do
         i <- readTVar (nextMsgIdVar cstate)
         modifyTVar' (nextMsgIdVar cstate) succ 
         writeTChan (outboundChan cstate) 
                    (OutboundMessage i channelId msg)) 
       
