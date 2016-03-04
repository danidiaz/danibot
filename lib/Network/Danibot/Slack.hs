{-# LANGUAGE OverloadedStrings #-}

module Network.Danibot.Slack (isDirectedTo,eventFold) where

import Data.Text (Text)
import Data.Char
import Data.Profunctor (Star(..))
import qualified Data.Attoparsec.Text as Atto

import Control.Exception
import Control.Foldl (FoldM(..))
import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar

import Streaming (Stream)

import Network.Danibot.Slack.Types (Event,
                                    UserMessage(..),
                                    Chat(..),
                                    Event(..),
                                    ChannelUser(..))

eventFold :: Chat -> FoldM IO Event ()
eventFold chat = FoldM stateTransition (pure InitialState) coda
    where
    coda = \_ -> pure ()

data ChatState = 
      InitialState
    | NormalState

stateTransition :: ChatState -> Event -> IO ChatState
stateTransition s event =
    case (s,event) of
        (InitialState,HelloEvent) -> pure NormalState
        (InitialState,_         ) -> throwIO (userError "wrong start")
        _                         -> print event *> pure NormalState

--handlers :: [Event -> IO ()] -> Event -> IO () 
--handlers = runStar . foldr (*>) (pure ()) . map Star
--
--imUpdatesHandler :: TVar Chat -> Event -> IO ()  
--imUpdatesHandler tchat event = case event of
--   IMOpen (ChannelUser ch urs) -> undefined
--   IMClose (ChannelUser ch urs) -> undefined
--   _ -> pure ()

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

