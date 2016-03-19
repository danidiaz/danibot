{-# LANGUAGE OverloadedStrings #-}

module Main (
        main
    ) where

import Data.Monoid
import Network.Danibot
import Network.Danibot.Main (mainWith,readJSON)

import Options.Applicative 
import qualified Options.Applicative as Options

data Args = Args
    {
        dictPath :: Maybe String
    } deriving (Show)

parserInfo :: Options.ParserInfo Args
parserInfo = 
    info (helper <*> parser) infoMod
  where
    parser = 
        Args <$> optional (strOption (help "json dict file" <> 
                                      long "dict" <> 
                                      metavar "DICT"))
    infoMod = 
        fullDesc <> header "program desc" 

main :: IO ()
main = do
    args  <- Options.execParser parserInfo
    mdict <- mapM readJSON (dictPath args) 
    let dict = maybe mempty id mdict
        handlers = [("nop",dumbHandler)
                   ,("up?",isUpHandler)
                   ,("lookup",lookupHandler dict)]
        handlersWithHelp = [("help", helpHandler handlers)] ++ handlers 
    mainWith (pure (Right (dispatch handlersWithHelp)))

