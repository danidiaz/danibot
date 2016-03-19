{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Danibot (
       Command
    ,  Description
    ,  dumbHandler
    ,  lookupHandler
    ,  isUpHandler
    ,  helpHandler
    ,  dispatch
    ) where

import Data.Monoid
import Data.Char
import Data.Map
import Data.List
import qualified Data.Map as Map
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

type Command = Text

type Description = Text

{-| Waits half a second, does nothing.		

-}
dumbHandler :: (Text -> IO Text,Description)
dumbHandler = 
    let go _ = do 
            threadDelay 0.5e6
            return "Nothing was done."
    in  (go,"")

{-| Performs lookups against an in-memory map.		

-}
lookupHandler :: Map Text Text -> (Text -> IO Text,Description) 
lookupHandler aMap = 
    let go key = pure (maybe "Entry not found." id (Map.lookup key aMap))
    in  (go,"key") 

{-| Is the given port listening on the given host?		

-}
isUpHandler :: (Text -> IO Text,Description)
isUpHandler =
    let go (Text.break isSpace . Text.strip -> (Text.strip -> host,Text.strip -> port)) = do 
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
    in  (go, "host port")
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

{-| Builds the help message.		

-}
helpHandler :: [(Command,(Text -> IO Text,Description))] -> (Text -> IO Text, Description)
helpHandler handlers = 
    let helptext = mconcat (intersperse "\n" [k <> " " <> v | (k,(_,v)) <- handlers])
    in  (\_ -> pure helptext,"")

splitWord :: Text -> (Text,Text)
splitWord (Text.break isSpace . Text.strip -> (Text.strip -> word,Text.strip -> rest)) =  (word,rest)

{-| Builds a handler from a list of command names and sub-handlers.		

-}
dispatch :: [(Command,(Text -> IO Text,x))] -> Text -> IO Text
dispatch (Map.fromList -> handlers) (splitWord -> (key,request)) =   
    case Map.lookup key handlers of
        Just (f,_) -> f request
        Nothing    -> pure "Unknown command."
    
