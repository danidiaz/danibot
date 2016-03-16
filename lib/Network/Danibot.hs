{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Danibot (
       dumbHandler
    ) where

import Data.Monoid
import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Concurrent
import Network
import System.IO
import Control.Exception

dumbHandler :: Text -> IO Text
dumbHandler _ = do 
    threadDelay 0.5e6
    return "I don't do anything yet."

upHandler :: Text -> IO Text
upHandler (Text.break isSpace . Text.strip -> (Text.unpack . Text.strip -> host,Text.unpack . Text.strip -> port)) = do 
    case read port of
        Just numericport ->
                (catch :: IO a -> (SomeException -> IO a) -> IO a)
                (bracket (Network.connectTo host (PortNumber (fromInteger numericport)))
                         hClose
                         (\_ -> pure "The port is listening."))
                (\e -> pure ("Exception when trying to connect: " <> Text.pack (show e)))
        _ -> pure "Expected arguments: host port"
