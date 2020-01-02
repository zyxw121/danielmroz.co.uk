{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Definitions where
import Data.Aeson
import GHC.Generics

data Post a = Post {
  postName :: String,
  postTitle :: String,
  postDescription :: String,
--  postDate :: DateTime,
--  postModDate :: Maybe DateTime,
  postBody :: a}
  deriving (Show)

data Note = Note {
  noteTitle :: String,
  notePath :: String }
  deriving (Show, Generic)

instance Functor Post where
  fmap f p = p {postBody = f $ postBody p}

data Config a = Config {
  intro :: a,
  about :: a,
  notes :: [Note]
}
  deriving (Show, Generic)

instance Functor Config where
  fmap f c = c{intro = f $ intro c, about = f $ about c}

c = Config {intro = "intro", about = "about", notes = [Note{noteTitle="a", notePath = "b"}]}
instance FromJSON Note
instance ToJSON Note
instance (FromJSON a) => FromJSON (Config a)
instance (ToJSON a) => ToJSON (Config a)

