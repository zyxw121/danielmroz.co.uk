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
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.IO.Error
import qualified Data.DateTime as D

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

makeConfig :: PandocMonad m => Config String -> m (Config Pandoc)
makeConfig Config{..} = do
  intro <- readMarkdown rOpts (pack intro)
  about <- readMarkdown rOpts (pack about)
  return Config{..}

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
postCom (Post{..}) = do
  x <- postBody
  let postBody = x
  return Post{..}
--postBody >>= (\x -> let postBody = x in return Post{..}) 

configCom :: Monad m => Config (m a) -> m (Config a)
configCom (Config{..}) = do
  i <- intro
  a <- about
  let intro = i
      about = a
  return Config{..}

styleFromFile :: String -> IO (Maybe Style)
styleFromFile path = catchIOError 
  (do
    json <- B.readFile path
    case (eitherDecode json :: Either String Style) of
      Left e -> putStr e >> (ioError $ userError "Failed to parse style" )
      Right s -> putStr ("Parsed style " ++ path) >> return (Just s)) 
  (\e -> print e >> return Nothing)


postToHtml :: PandocMonad m => Post Pandoc -> m (Post H.Html)
postToHtml = postCom . fmap (writeHtml4 wOpts)

configToHtml :: Config String -> Either PandocError (Config H.Html)
configToHtml = runPure . (makeConfig >=> configToHtml')

configToHtml' :: PandocMonad m => Config Pandoc -> m (Config H.Html)
configToHtml'  = configCom . fmap (writeHtml4 wOpts) 

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
  putStr $ "Post " ++ name ++ " published\n"
  return . Just $ fmap (const ()) p
  
filterMaybes :: [Maybe a] -> [a]
filterMaybes [] = []
filterMaybes ((Just x):xs) = x : filterMaybes xs
filterMaybes (Nothing : xs) = filterMaybes xs

fromMaybe :: Maybe a -> a -> a
fromMaybe (Just x) _ = x
fromMaybe Nothing y = y

predefStyles = 
 ["pygments",
  "espresso",
  "zenburn",
  "monochrome",
  "breezeDark",
  "haddock",
  "kate"]

toStyle :: String ->  Style
toStyle "pygments" = pygments
toStyle "espresso" = espresso
toStyle "zenburn" = zenburn
toStyle "monochrome" = monochrome
toStyle "breezeDark" = breezeDark
toStyle "haddock" = haddock
toStyle "kate" = kate

makeStyle :: Maybe String -> IO Style
makeStyle Nothing = return kate
makeStyle (Just name) = if name `elem` predefStyles then return $ toStyle name else styleFromFile name >>= \case 
  Just s -> return s
  Nothing -> return kate


doPost :: String -> IO (Maybe (Post ()))
doPost path = do
  let name = dropExtension . takeBaseName $ path
  file <- readFile $ "posts" </> path
  let post = runPure . (makePost >=> postToHtml) $ (name, file)
  publishPost (name, post)

main :: IO ()
main = do 
  json <- B.readFile "config.json" 
  case (eitherDecode json :: Either String (Config String)) of
    Left e -> putStr ("Error reading config: \n" ++ e) >> return ()
    Right config -> do
      case (configToHtml config) of
        Left e -> print e
        Right config' -> do
          postpaths <- listDirectory "posts/" 
          posts <- fmap filterMaybes . sequence . map doPost $ postpaths 
          -- :: [Post ()], all posts that have been published successfully 
          today <- D.getCurrentTime
          let index = makeIndex today posts config'
          writeFile "public/index.html" (renderHtml index)
          s <- makeStyle (hstyle config') 
          publishHighlight s 


