{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Danibot.Slack.Types where

import Control.Monad
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Monoid.Cancellative as Textual
import Data.Aeson
import Data.Aeson.Types

import GHC.Generics

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

instance FromJSON Initial

data Self = Self
    {
        self_id :: Text
    ,   self_name :: Text
    } deriving (Generic,Show)

instance ToJSON Self where
    toJSON = genericToJSON aesonOptions

instance FromJSON Self where
    parseJSON = genericParseJSON aesonOptions

data Team = Team
    {
        team_id :: Text
    ,   team_name :: Text
    } deriving (Generic,Show)

instance ToJSON Team where
    toJSON = genericToJSON aesonOptions

instance FromJSON Team where
    parseJSON = genericParseJSON aesonOptions

data User = User
    {
        user_id :: Text
    ,   user_name :: Text
    } deriving (Generic,Show)

instance ToJSON User where
    toJSON = genericToJSON aesonOptions

instance FromJSON User where
    parseJSON = genericParseJSON aesonOptions

data Channel = Channel
    {
        channel_id :: Text
    ,   channel_name :: Text
    ,   is_member :: Bool
    ,   is_general :: Bool
    } deriving (Generic,Show)

instance ToJSON Channel where
    toJSON = genericToJSON aesonOptions

instance FromJSON Channel where
    parseJSON = genericParseJSON aesonOptions

data Group = Group
    {
        group_id :: Text
    ,   group_name :: Text
    ,   members :: [Text]
    } deriving (Generic,Show)

instance ToJSON Group where
    toJSON = genericToJSON aesonOptions

instance FromJSON Group where
    parseJSON = genericParseJSON aesonOptions

data IM = IM
    {
        im_id :: Text
    ,   user :: Text
    } deriving (Generic,Show)

instance ToJSON IM where
    toJSON = genericToJSON aesonOptions

instance FromJSON IM where
    parseJSON = genericParseJSON aesonOptions

aesonOptions :: Options
aesonOptions = defaultOptions 
    { 
        fieldLabelModifier =
            let 
            modified field = do
                (suffix,replacement) <- [("_id","id"),("_name","name")]
                guard (Textual.isSuffixOf (suffix::Text) (fromString field))
                pure replacement 
            in  \f -> head (modified f ++ [f])
            
    }
