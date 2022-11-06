{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Definitions where
import Data.Aeson
import GHC.Generics
import Data.Time

data Post a = Post {
  postName :: String,
  postTitle :: String,
  postDescription :: String,
  postDate :: Day, 
--  postModDate :: Maybe DateTime,
  postBody :: a}
  deriving (Show, Eq)


instance Eq a => Ord (Post a) where
  (<=) p q = postDate p <= postDate q

data Note = Note {
  noteTitle :: String,
  notePath :: String }
  deriving (Show, Generic)

instance Functor Post where
  fmap f p = p {postBody = f $ postBody p}

data Config a = Config {
  intro :: a,
  about :: a,
  notes :: [Note],
  hstyle :: Maybe String
}
  deriving (Show, Generic)

instance Functor Config where
  fmap f c = c{intro = f $ intro c, about = f $ about c}

instance FromJSON Note
instance ToJSON Note
instance (FromJSON a) => FromJSON (Config a)
instance (ToJSON a) => ToJSON (Config a)

