{-# LANGUAGE OverloadedStrings #-}

module Network.Danibot.Slack (isDirectedTo,mute) where

import Data.Text (Text)
import Data.Char
import Data.Profunctor (Star(..))
import qualified Data.Attoparsec.Text as Atto

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar

import Network.Danibot.Slack.Types (Event,
                                    UserMessage(..),
                                    Chat(..),
                                    Event(..),
                                    ChannelUser(..))

mute :: IO a
mute = newEmptyMVar >>= takeMVar

handlers :: [Event -> IO ()] -> Event -> IO () 
handlers = runStar . foldr (*>) (pure ()) . map Star

imUpdatesHandler :: TVar Chat -> Event -> IO ()  
imUpdatesHandler tchat event = case event of
   IMOpen (ChannelUser ch urs) -> undefined
   IMClose (ChannelUser ch urs) -> undefined
   _ -> pure ()

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

