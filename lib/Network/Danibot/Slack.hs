{-# LANGUAGE OverloadedStrings #-}

module Network.Danibot.Slack (isDirectedTo,eventFold) where

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
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan

import Streaming (Stream)

import Network.Danibot.Slack.Types 

data Pair a b = Pair !a !b

eventFold :: Chat -> IO (FoldM IO Event (),Stream (Of OutboundMessage) IO ())
eventFold chat = do
    chan <- atomically newTChan  
    let pair = 
         (,)
         (FoldM reactToEvent (pure (Pair chat InitialState)) coda)    
         (Streaming.repeatM (atomically (readTChan chan)))
    pure pair
    where
    coda = \_ -> pure ()

data ProtocolState = 
      InitialState
    | NormalState

type ChatState = Pair Chat ProtocolState

--reactToEvent :: TChan (Text,Text -> IO ()) -> ChatState -> Event -> IO ChatState
reactToEvent :: ChatState -> Event -> IO ChatState
reactToEvent (Pair c s) event =
    case (s,event) of
        (InitialState,HelloEvent) -> 
            pure (Pair c NormalState)
        (InitialState,_) -> 
            throwIO (userError "wrong start")
        (NormalState,MessageEvent (Message _ (Right (UserMessage ch user txt NotMe)))) -> do
            let whoami = selfId (self c) 
            if has (ims.ix ch) c  
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

