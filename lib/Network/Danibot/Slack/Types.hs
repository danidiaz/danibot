{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Network.Danibot.Slack.Types where

import Control.Monad
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Monoid.Cancellative as Textual
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative

import GHC.Generics

newtype Wire a = Wire { unwire :: a } deriving (Show,Functor)

(.:$) :: FromJSON (Wire b) => Object -> Text -> Parser b
v .:$  name = fmap unwire (v .: name)

(.:$$) :: (Functor f,FromJSON (f (Wire b))) => Object -> Text -> Parser (f b)
v .:$$ name = fmap (fmap unwire) (v .: name)

data Initial = Initial
    {
        url :: Text
    ,   self :: Self
    ,   team :: Team
    ,   users :: [User]
    ,   channels :: [Channel]
    ,   groups :: [Group]
    ,   ims :: [IM]
    } deriving (Generic,Show)

instance ToJSON Initial

instance FromJSON (Wire Initial) where
    parseJSON (Object v) = Wire <$> (Initial
        <$> v .:   "url"
        <*> v .:$  "self"
        <*> v .:$  "team"
        <*> v .:$$ "users"
        <*> v .:$$ "channels"
        <*> v .:$$ "groups"
        <*> v .:$$ "ims")
    parseJSON _ = empty
    
data Self = Self
    {
        selfId :: Text
    ,   selfName :: Text
    } deriving (Generic,Show)

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

instance ToJSON IM

instance FromJSON (Wire IM) where
    parseJSON (Object v) = Wire <$> (IM
        <$> v .: "id"
        <*> v .: "user")
    parseJSON _ = empty

