{-# LANGUAGE OverloadedStrings #-}

module Main (
        main
    ) where

import Data.Monoid
import Network.Danibot
import Network.Danibot.Main (mainWith)

import Options.Applicative 
import qualified Options.Applicative as Options

main :: IO ()
main = mainWith (pure (Right (dispatch handlersWithHelp)))
    where
    handlers = [("nop",dumbHandler),("up?",isUpHandler),("lookup",lookupHandler mempty)]
    handlersWithHelp = [("help", helpHandler handlers)] ++ handlers 

