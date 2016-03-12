{-# LANGUAGE OverloadedStrings #-}

module Network.Danibot.Slack (isDirectedTo,discardWorker,makeChatState,eventFold) where

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
import Streaming.Prelude (Of,unfoldr)
import qualified Streaming.Prelude as Streaming
import Control.Foldl (FoldM(..))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

import Streaming (Stream)

import Network.Danibot.Slack.Types 


discardWorker :: IO (TChan (Text,Text -> IO ()), IO ()) 
discardWorker = do
    chan <- atomically newTChan  
    let worker = forever (do
                            _ <- atomically (readTChan chan)
                            pure ())
    pure (chan,worker)                            

eventFold :: TChan (Text,Text -> IO ()) 
          -> ChatState
          -> FoldM IO Event ()
eventFold pool chatState =
    FoldM (reactToEvent pool) (pure (Pair chatState InitialState)) coda    
    where
    coda = \_ -> pure ()

data ChatState = ChatState
    {
       chatVar :: TVar Chat 
    ,  nextMsgIdVar :: TVar Int 
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

data Pair a b = Pair !a !b

type FoldState = Pair ChatState ProtocolState

reactToEvent :: TChan (Text,Text -> IO ()) -> FoldState -> Event -> IO FoldState
reactToEvent pool (Pair c s) event =
    case (s,event) of
        (InitialState,HelloEvent) -> 
            pure (Pair c NormalState)
        (InitialState,_) -> 
            throwIO (userError "wrong start")
        (NormalState,MessageEvent (Message _ (Right (UserMessage ch user txt NotMe)))) -> do
            currentcs <- atomically (readTVar (chatVar c)) 
            let whoami = selfId (self currentcs) 
            if has (ims.ix ch) currentcs  
              then do 
                case isDirectedTo txt of
                    Just (target,txt') | target == whoami -> 
                        print ("message directed to me! " <> txt') 
                    Nothing ->
                        print ("message implicitly directed to me!" <> txt)
                    _ ->
                        print "message in private channel not directe to me (!?)"
                print "hey, a message!"  
              else 
                case isDirectedTo txt of
                    Just (target,txt') | target == whoami -> 
                        print ("message directed to me! " <> txt') 
                    _ -> do      
                        print "ignoring message in public channel"
            pure (Pair c NormalState)
        _ -> 
            print event *> pure (Pair c NormalState)

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

