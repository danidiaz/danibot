{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.Danibot.Slack.Types where

import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Monoid.Cancellative as Textual
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative

import GHC.Generics

newtype Wire a = Wire { unwire :: a } deriving (Show,Functor)

instance Identified i => Identified (Wire i) where
    identity (Wire i) = identity i

class Identified i where
    identity :: i -> Text

class Named n where
    name :: n -> Text

data Intro = Intro
    {
        introUrl :: Text
    ,   introChat :: Chat
    } deriving (Generic,Show)

instance ToJSON Intro

instance FromJSON (Wire Intro) where
    parseJSON (Object v) = 
        let
        introParser = Intro
            <$> v .: "url"
            <*> chatParser 
        chatParser = Chat
            <$> (unwire <$> v .: "self")
            <*> (unwire <$> v .: "team")
            <*> (mapify <$> v .: "users")
            <*> (mapify <$> v .: "channels")
            <*> (mapify <$> v .: "groups")
            <*> (mapify <$> v .: "ims")
        mapify es = 
            Map.fromList (zip (map identity es) (map unwire es))
        in
        Wire <$> introParser
    parseJSON _ = empty

data Chat = Chat
    {
        self :: !Self
    ,   team :: !Team
    ,   users :: !(Map Text User)
    ,   channels :: !(Map Text Channel)
    ,   groups :: !(Map Text Group)
    ,   ims :: !(Map Text IM)
    } deriving (Generic,Show,ToJSON)


--data ChatWrap = ChatWrap
--    {
--        chat  :: !Chat
--    ,   nextMessageId :: !Integer
--    } deriving (Generic,Show,ToJSON)


data Self = Self
    {
        selfId :: Text
    ,   selfName :: Text
    } deriving (Generic,Show,ToJSON)

instance Identified Self where
    identity = selfId

instance Named Self where
    name = selfName

instance FromJSON (Wire Self) where
    parseJSON (Object v) = Wire <$> (Self
        <$> v .: "id"
        <*> v .: "name")
    parseJSON _ = empty

data Team = Team
    {
        teamId :: Text
    ,   teamName :: Text
    } deriving (Generic,Show,ToJSON)

instance Identified Team where
    identity = teamId

instance Named Team where
    name = teamName

instance FromJSON (Wire Team) where
    parseJSON (Object v) = Wire <$> (Team
        <$> v .: "id"
        <*> v .: "name")
    parseJSON _ = empty

data User = User
    {
        userId :: Text
    ,   userName :: Text
    } deriving (Generic,Show,ToJSON)

instance Identified User where
    identity = userId

instance Named User where
    name = userName

instance FromJSON (Wire User) where
    parseJSON (Object v) = Wire <$> (User
        <$> v .: "id"
        <*> v .: "name")
    parseJSON _ = empty

data Channel = Channel
    {
        channelId :: Text
    ,   channelName :: Text
    ,   isMember :: Bool
    ,   isGeneral :: Bool
    } deriving (Generic,Show,ToJSON)

instance Identified Channel where
    identity = channelId

instance Named Channel where
    name = channelName

instance FromJSON (Wire Channel) where
    parseJSON (Object v) = Wire <$> (Channel
        <$> v .: "id"
        <*> v .: "name"
        <*> v .: "is_member"
        <*> v .: "is_general")
    parseJSON _ = empty

data Group = Group
    {
        groupId :: Text
    ,   groupName :: Text
    ,   members :: [Text]
    } deriving (Generic,Show,ToJSON)

instance Identified Group where
    identity = groupId

instance Named Group where
    name = groupName

instance FromJSON (Wire Group) where
    parseJSON (Object v) = Wire <$> (Group
        <$> v .: "id"
        <*> v .: "name"
        <*> v .: "members")
    parseJSON _ = empty

data IM = IM
    {
        imId :: Text
    ,   user :: Text
    } deriving (Generic,Show,ToJSON)

instance Identified IM where
    identity = imId

instance FromJSON (Wire IM) where
    parseJSON (Object v) = Wire <$> (IM
        <$> v .: "id"
        <*> v .: "user")
    parseJSON _ = empty

data Message = Message {
        messageTs :: Text
    ,   messageValue :: Either Value UserMessage
    } deriving (Generic,Show,ToJSON)

data Me = Me | NotMe deriving (Generic,Show)

instance ToJSON Me

data UserMessage = UserMessage 
    {
        messageChannel :: Text
    ,   messageUser :: Text
    ,   messageText :: Text
    ,   messageMe :: Me
    } deriving (Generic,Show,ToJSON)

instance FromJSON (Wire Message) where
    parseJSON (Object v) = Wire <$> (do
        ts <- v .: "ts" 
        subtype <- v .:? "subtype" .!= Text.empty
        let me = if subtype == "me_message" 
            then Me 
            else NotMe 
        msgval <- if any (==subtype) ["","me_message"]
            then do
                channel_ <- v .: "channel"
                user_    <- v .: "user"
                text_    <- v .: "text" 
                pure (Right (UserMessage channel_ user_ text_ me))
            else 
                pure (Left (Object v))
        pure (Message ts msgval))
    parseJSON _ = empty

data ChannelUser = ChannelUser
    {
        cuChannel :: Text
    ,   cuUser :: Text
    } deriving (Generic,Show,ToJSON)

instance FromJSON (Wire ChannelUser) where
    parseJSON (Object v) = Wire <$> (ChannelUser
        <$> v .: "channel"
        <*> v .: "user")
    parseJSON _ = empty

data Event =
      HelloEvent
    | MessageEvent Message
    | UserTypingEvent ChannelUser
    | IMOpen ChannelUser
    | IMClose ChannelUser
    | GeneralEvent Value 
    deriving (Generic,Show,ToJSON)

instance FromJSON (Wire Event) where
    parseJSON (Object v) = Wire <$> (do
        eventType <- v .: "type"
        case eventType :: Text of
            "hello" -> 
                pure HelloEvent 
            "message" -> 
                MessageEvent . unwire <$> parseJSON (Object v)
            "user_typing" -> 
                UserTypingEvent . unwire <$> parseJSON (Object v)
            "im_open" -> 
                IMOpen . unwire <$> parseJSON (Object v)
            "im_close" -> 
                IMClose . unwire <$> parseJSON (Object v)
            _ -> pure (GeneralEvent (Object v)))   
    parseJSON _ = empty

data OutboundMessage = OutboundMessage
    {
        outboundMessageId :: Integer
    ,   outboundMessageChannel :: Text 
    ,   outboundMessageText :: Text 
    } deriving (Generic,Show,ToJSON) 

instance ToJSON (Wire OutboundMessage) where
    toJSON (Wire (OutboundMessage msgid ch txt)) = object [
          "type" .= ("message" :: Text)
        , "id" .= msgid
        , "channel" .= ch 
        , "text" .= txt
        ]

