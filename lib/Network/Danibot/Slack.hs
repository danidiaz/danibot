{-# LANGUAGE OverloadedStrings #-}

module Network.Danibot.Slack (isDirectedTo,mute) where

import Data.Text (Text)
import Data.Char
import qualified Data.Attoparsec.Text as Atto

import Control.Applicative
import Control.Concurrent.MVar

import Network.Danibot.Slack.Types (UserMessage(..))

mute :: IO a
mute = newEmptyMVar >>= takeMVar

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

