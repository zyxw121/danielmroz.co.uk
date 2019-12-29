{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Html where
import Definitions
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A 

type Template a = a -> H.Html



postToPage :: Post H.Html -> H.Html
postToPage  (Post {..}) = html $ do
  H.head $ do
    H.title $ toHtml $ "Daniel Mroz | " ++ postTitle
    link ! href "../css/main.css" ! rel "stylesheet" ! type_ "text/css"
    link ! href "https://fonts.googleapis.com/css?family=Kanit|Lora|Seymour+One|Modak" ! rel "stylesheet"
    script ! src "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML" ! type_ "text/javascript" ! async "async" $ mempty  
  body $ do 
    H.div ! class_ "container" $ 
      H.div ! class_ "mainbar" $ 
      H.div ! class_ "maincontainer container" $ do
        H.div ! class_ "main box" $ do
          H.div ! class_ "center" $
            h1 $ toHtml postTitle
          H.div ! class_ "inside" $ do
            hr  
            postBody
            H.div ! class_ "footer" $ 
              a ! href "http://danielmroz.co.uk" $  "Click here to go back."
        H.div ! class_ "footer" $ 
          p $ preEscapedToHtml $ ("Copyright &copy; 2019 Daniel Mroz" :: String)
         
intro :: H.Html
intro = do 
  h1 $ "Daniel Mroz"
  h4 $ "Computer Science PhD student, Manchester University"
  h4 $ "Research Interests: Functional programming, logic, category theory"
  h4 $ do 
    a ! href "Daniel_Mroz_CV.pdf" $ "CV"
    " ~ "
    a ! href "http://github.com/zyxw121" $ "Github"
  h4 $ "Email: me (at) danielmroz.co.uk"  

about :: H.Html
about = do
  h2 $ "About"
  p $ "After studying Maths and Computer Science at Oxford, I'm now a PhD student at Manchester. I'm a big fan of functional programming, especially in Haskell, because it makes it possible to write very high-level, elegant and understandable programs, and easily reason about the correctness of programs."

posts :: [Post a] -> H.Html
posts ps = do
  h2 $ "Posts"
  ul $ do
    mapM_ makeListItem  ps 

makeIndex :: [Post a] -> H.Html
makeIndex ps = html $ do
  H.head $ do
    H.title $ "Daniel Mroz" 
    link ! href "css/main.css" ! rel "stylesheet" ! type_ "text/css"
    link ! href "https://fonts.googleapis.com/css?family=Kanit|Lora|Seymour+One|Modak" ! rel "stylesheet"
  body $ do 
    H.div ! class_ "container" $ 
      H.div ! class_ "mainbar" $ 
      H.div ! class_ "maincontainer container" $ do
        H.div ! class_ "main box" $ do
          H.div ! class_ "center" $ do
            intro
          H.div ! class_ "inside" $ do
            about
            posts ps
            --h2 $ "Status"
            --p $ "I am currently working on my dissertation, on the cateorical semantics of Homotopy Type Theory"
            --p $ "Next year I will be studying for a PhD at Manchester"
        H.div ! class_ "footer" $ do 
          p $ preEscapedToHtml $ ("Copyright &copy; 2019 Daniel Mroz" :: String)
          p $ "Last modified: July 18, 2019"
 
makeListItem :: Post a -> H.Html
makeListItem Post{..} = html $ li $ do
  a ! href (stringValue ("posts/" ++ postName ++ ".html" )) $ h4 $ toHtml postTitle
  preEscapedToHtml ("&mdash;" :: String)
  p $ toHtml postDescription
