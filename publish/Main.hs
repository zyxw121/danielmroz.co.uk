{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import Html
import Definitions
import Data.Text (Text, pack, unpack)
import System.Directory
import System.FilePath.Posix
import System.Environment
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson.Encode.Pretty as P


getFilePath :: IO (Maybe String)
getFilePath = getArgs >>= \case 
  [] -> return Nothing
  (x:xs) -> return . Just $ x

getTitle :: IO String
getTitle = do
  putStr "Title?\n"
  getLine

lookupTitle :: String -> [Note] -> Maybe Note
lookupTitle p [] = Nothing
lookupTitle p (x:xs) = if notePath x == p then Just x else lookupTitle p xs

addNote :: String -> String -> Config a -> Config a
addNote path title config = let n = Definitions.Note{notePath = path, noteTitle = title} in config{notes=n:(notes config)} 

deleteNote' :: String -> [Note] -> [Note]
deleteNote' p [] = []
deleteNote' p (x:xs) = if notePath x == p then xs else x:(deleteNote' p xs)

deleteNote :: String -> Config a -> Config a
deleteNote p config = config{notes = deleteNote' p $ notes config}

webpath = "website/danielmroz-co-uk"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStr "Usage: publish [file]"
    (path:xs) -> do
      let path = head args
      home <- getHomeDirectory
      json <- B.readFile $ home </> webpath </>"config.json"
      case (eitherDecode json :: Either String (Config String)) of
        Left e -> putStr ("Error reading config: \n" ++ e) >> return ()
        Right config -> do
          case lookupTitle path $ notes config of
            Just n -> do 
              putStr $ "Note already exists with title: " ++ noteTitle n ++ "\n" 
              putStr $ "Updating...\n"
            Nothing -> do
              putStr "Title? \n"
              s <- getLine
              B.writeFile (home </> webpath </> "config.json") . P.encodePretty $ addNote path s config
          copyFile path $ home </> webpath </> "public/notes" </> path
          putStr "Done"
