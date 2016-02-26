{-# LANGUAGE OverloadedStrings #-}

module Network.Danibot.Slack.API (
          AuthToken
        , startRTM
    ) where

import Control.Lens
import Data.Aeson (Value(..),fromJSON,toJSON,Result(Error,Success))
import Data.Aeson.Lens
import Data.Text (Text)
import qualified Data.Monoid.Textual as Textual
import qualified Network.Wreq as Wreq

import Network.Danibot.Slack.Types (Wire(..),Initial)

type AuthToken = Text

startRTM :: AuthToken -> IO (Either String Initial)
startRTM authToken = do 
    resp <- Wreq.postWith (withToken authToken) 
                          "https://slack.com/api/rtm.start" 
                          (toJSON ())
    respJSON <- view Wreq.responseBody <$> Wreq.asJSON resp
    pure (checkResp respJSON)

withToken :: AuthToken -> Wreq.Options
withToken token = 
      Wreq.defaults
    & set (Wreq.param "token") [token]

checkResp :: Value -> Either String Initial
checkResp v =
    case (v^?key "ok"._Bool,fromJSON v,v^?key "error"._String) of
        (Just True ,Success (Wire info),_) -> Right info
        (Just False,_,Just err) -> Left (Textual.fromText err)
        (_,Error err,_) -> Left err
        _ -> Left "malformed response"

