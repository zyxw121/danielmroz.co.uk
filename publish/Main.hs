{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import Html
import Definitions
import Text.Pandoc
import Skylighting
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Highlighting
import Data.Text (Text, pack, unpack)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A 
import Text.Blaze.Html.Renderer.String
import System.Directory
import System.FilePath.Posix
import System.Environment
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson.Encode.Pretty as P

fromJust :: Maybe a -> a
fromJust (Just a) = a

inlineToString :: Inline -> String
inlineToString (Str s) = s
inlineToString (Space) = " "

inlinesToString :: [Inline] -> String
inlinesToString = concat . map inlineToString

makePost :: PandocMonad m => (String, String) -> m (Post Pandoc)
makePost (postName, s) = do 
  p@(Pandoc m _) <- readMarkdown rOpts (pack s)
  let (MetaInlines postTitle') = fromJust $ lookupMeta "title" m
      postTitle = inlinesToString postTitle'
      (MetaInlines postDescription') = fromJust $ lookupMeta "description" m
      postDescription = inlinesToString postDescription'
      postBody = p
  return Post{..}

rOpts :: ReaderOptions
rOpts = def {
  readerExtensions =  myExts (readerExtensions def) }

wOpts :: WriterOptions
wOpts = def {
  writerExtensions = myExts (writerExtensions def),
  writerHighlightStyle = Just kate,
  writerHTMLMathMethod = MathJax "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js"}

myExts e = foldr enableExtension e [Ext_fenced_code_blocks, Ext_fenced_code_attributes, Ext_yaml_metadata_block, Ext_tex_math_dollars, Ext_simple_tables]

postCom :: Monad m => Post (m a) -> m (Post a)
postCom (Post{..}) = postBody >>= (\x -> let postBody = x in return Post{..}) 

postToHtml :: PandocMonad m => Post Pandoc -> m (Post H.Html)
postToHtml = postCom . fmap (writeHtml4 wOpts)

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = \x -> f x >>= g 


publishHighlight :: Style -> IO ()
publishHighlight s = do
  let css = styleToCss s
  writeFile ("public/css/highlight.css") css


publishPost :: (String, Either PandocError (Post H.Html)) -> IO (Maybe (Post ())) 
publishPost (name, Left e) = do 
  putStr $ "Post " ++ name ++ " had an error: "
  print e
  return Nothing
publishPost (name, Right p) = do
  writeFile ("public/posts" </> name <.> "html") (renderHtml . postToPage $ p)
  return . Just $ fmap (const ()) p
  
filterMaybes :: [Maybe a] -> [a]
filterMaybes [] = []
filterMaybes ((Just x):xs) = x : filterMaybes xs
filterMaybes (Nothing : xs) = filterMaybes xs

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
addNote path title config = let n = Definitions.Note{notePath = path, noteTitle = title} 
                                in config{notes=n:(notes config)} 

webpath = "website/danielmroz-co-uk"

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  putStr "Title? \n"
  s <- getLine
  home <- getHomeDirectory
  json <- B.readFile $ home </> webpath </>"config.json"
  case (eitherDecode json :: Either String (Config String)) of
    Left e -> putStr ("Error reading config: \n" ++ e) >> return ()
    Right config -> do
      case lookupTitle path $ notes config of
        Just n -> putStr $ "Note already exists with title: " ++ noteTitle n
        Nothing -> return ()
      let config' = addNote path s config
      B.writeFile (home </> webpath </> "config.json") . P.encodePretty$ config'
      copyFile path $ home </> webpath </> "public/notes" </> path
      putStr "Done"
