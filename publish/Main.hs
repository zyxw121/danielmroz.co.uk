{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
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

main :: IO ()
main = do
  postpaths <- listDirectory "posts/" 
  let postnames = map (dropExtension . takeBaseName) postpaths
  files <- sequence . map (readFile . ("posts/" </>)) $ postpaths
  let posts = map (runPure . (makePost >=> postToHtml)) $ zip postnames files
  published <- sequence . map publishPost $ zip postnames posts
  let published' = filterMaybes published
      index = makeIndex published' undefined
  writeFile "public/index.html" (renderHtml index)
  publishHighlight kate


