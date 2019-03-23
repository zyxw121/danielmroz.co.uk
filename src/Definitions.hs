{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Definitions where


data Post a = Post {
  postName :: String,
  postTitle :: String,
  postDescription :: String,
--  postDate :: DateTime,
--  postModDate :: Maybe DateTime,
  postBody :: a}
  deriving (Show)

instance Functor Post where
  fmap f p = p {postBody = f $ postBody p}

