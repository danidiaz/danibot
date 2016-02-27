{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Network.Danibot.Slack.Types where

import Control.Applicative
import Control.Monad
import Data.String (fromString)
import qualified Data.Map as Map
import Data.Text (Text)
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
        url :: Text
    ,   chat :: Chat
    } deriving (Generic,Show)

instance ToJSON Intro

instance FromJSON (Wire Intro) where
    parseJSON (Object v) = 
        let
        introParser i = Intro
            <$> v .: "url"
            <*> chatParser v
        chatParser c = Chat
            <$> (unwire <$> c .: "self")
            <*> (unwire <$> c .: "team")
            <*> (mapify <$> c .: "users")
            <*> (mapify <$> c .: "channels")
            <*> (mapify <$> c .: "groups")
            <*> (mapify <$> c .: "ims")
        mapify es = 
            Map.fromList (zip (map identity es) (map unwire es))
        in
        Wire <$> introParser v
    parseJSON _ = empty

data Chat = Chat
    {
        self :: Self
    ,   team :: Team
    ,   users :: Map.Map Text User
    ,   channels :: Map.Map Text Channel
    ,   groups :: Map.Map Text Group
    ,   ims :: Map.Map Text IM
    } deriving (Generic,Show)

instance ToJSON Chat

data Self = Self
    {
        selfId :: Text
    ,   selfName :: Text
    } deriving (Generic,Show)

instance Identified Self where
    identity = selfId

instance Named Self where
    name = selfName

instance ToJSON Self

instance FromJSON (Wire Self) where
    parseJSON (Object v) = Wire <$> (Self
        <$> v .: "id"
        <*> v .: "name")
    parseJSON _ = empty

data Team = Team
    {
        teamId :: Text
    ,   teamName :: Text
    } deriving (Generic,Show)

instance Identified Team where
    identity = teamId

instance Named Team where
    name = teamName

instance ToJSON Team

instance FromJSON (Wire Team) where
    parseJSON (Object v) = Wire <$> (Team
        <$> v .: "id"
        <*> v .: "name")
    parseJSON _ = empty

data User = User
    {
        userId :: Text
    ,   userName :: Text
    } deriving (Generic,Show)

instance Identified User where
    identity = userId

instance Named User where
    name = userName

instance ToJSON User

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
    } deriving (Generic,Show)

instance Identified Channel where
    identity = channelId

instance Named Channel where
    name = channelName

instance ToJSON Channel

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
    } deriving (Generic,Show)

instance Identified Group where
    identity = groupId

instance Named Group where
    name = groupName

instance ToJSON Group

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
    } deriving (Generic,Show)

instance Identified IM where
    identity = imId

instance ToJSON IM

instance FromJSON (Wire IM) where
    parseJSON (Object v) = Wire <$> (IM
        <$> v .: "id"
        <*> v .: "user")
    parseJSON _ = empty

