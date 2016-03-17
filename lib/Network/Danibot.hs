{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Danibot (
       dumbHandler
    ,  isUpHandler
    ) where

import Data.Monoid
import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Attoparsec.Text as Atto
import Text.Read
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Network
import System.IO
import Control.Exception

dumbHandler :: Text -> IO Text
dumbHandler _ = do 
    threadDelay 0.5e6
    return "Nothing was done."

isUpHandler :: Text -> IO Text
isUpHandler (Text.break isSpace . Text.strip -> (Text.strip -> host,Text.strip -> port)) = do 
    print host
    print (extractAddress host)
    case (extractAddress host, readMaybe (Text.unpack port)) of
        (Right host', Just numericport) -> do
            let attempt =
                    catchAny
                    (bracket (Network.connectTo (Text.unpack host') (PortNumber (fromInteger numericport)))
                             hClose
                             (\_ -> pure "The port is listening."))
                    (\e -> pure ("Exception when trying to connect: " <> Text.pack (show e)))
            either id id <$> race attempt (threadDelay 3e6 *> pure "Timeout.")
        _ -> pure "Expected arguments: host port"
    where
    catchAny :: IO a -> (SomeException -> IO a) -> IO a
    catchAny = catch

    extractAddress :: Text -> Either String Text
    extractAddress txt = Atto.parseOnly extractor txt
        where
        extractor = 
            (Atto.string "<" *> Atto.takeTill (\c -> c == '|') 
                             *> Atto.string   "|"
                             *> Atto.takeTill (\c -> c == '>') 
                             <* Atto.string ">")
            <|>
            Atto.takeText

