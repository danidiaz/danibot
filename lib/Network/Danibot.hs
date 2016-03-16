{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumDecimals #-}

module Network.Danibot (
       dumbHandler
    ) where

import Data.Text (Text)
import Control.Concurrent

dumbHandler :: Text -> IO Text
dumbHandler _ = do 
    threadDelay 0.5e6
    return "I don't do anything yet."


