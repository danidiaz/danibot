{-# LANGUAGE OverloadedStrings #-}

module Network.Danibot.Slack (isDirectedTo,eventFold) where

import Data.Text (Text)
import Data.Char
import Data.Profunctor (Star(..))
import qualified Data.Attoparsec.Text as Atto

import Control.Applicative
import Control.Lens
import Control.Exception
import Control.Foldl (FoldM(..))
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar

import Streaming (Stream)

import Network.Danibot.Slack.Types 

data Pair a b = Pair !a !b

eventFold :: Chat -> FoldM IO Event ()
eventFold chat = FoldM reactToEvent (pure (Pair chat InitialState)) coda
    where
    coda = \_ -> pure ()

data ProtocolState = 
      InitialState
    | NormalState

type ChatState = Pair Chat ProtocolState

reactToEvent :: ChatState -> Event -> IO ChatState
reactToEvent (Pair c s) event =
    case (s,event) of
        (InitialState,HelloEvent) -> 
            pure (Pair c NormalState)
        (InitialState,_) -> 
            throwIO (userError "wrong start")
        (NormalState,MessageEvent (Message _ (Right (UserMessage ch user txt NotMe)))) -> do
            if has (ims.ix ch) c  
              then  
                print "hey, a private message!"  
              else 
                print "hey, a message!"  
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

