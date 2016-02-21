{-# LANGUAGE OverloadedStrings #-}

module Network.Danibot.Slack.API (
          AuthToken
        , RTMURI
        , startRTM
    ) where

import Control.Lens
import Data.Aeson (Value(..),toJSON)
import Data.Aeson.Lens
import Data.Text (Text)
import qualified Data.Monoid.Textual as Textual
import qualified Network.Wreq as Wreq

type AuthToken = Text

type RTMURI = Text

startRTM :: AuthToken -> IO (Either String RTMURI)
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

checkResp :: Value -> Either String RTMURI
checkResp v =
    case (v^?key "ok"._Bool,v^?key "url"._String,v^?key "error"._String) of
        (Just True ,Just url,_       ) -> Right url
        (Just False,_       ,Just err) -> Left (Textual.fromText err)
        _                              -> Left "malformed response"

