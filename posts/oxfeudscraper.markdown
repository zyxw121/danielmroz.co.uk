---
title: "Scraping Facebook posts with Haskell"
description: "An exploration of the Oxfeud.it backend"
date: "2018-01-12"
lastmod: "2018-02-10"
---
## Deprecated  
Due to changes in the Facebook Graph API, most of this doesn't work anymore.

## Introduction
The main back-end component of ~~Oxfeud.it~~(Now deprecated) is a Haskell program that grabs content from Facebook and puts it in a database. It makes use of the [Facebook Graph Api](https://developers.facebook.com/docs/graph-api/) to make requests that return JSON objects from which data is extracted and inserted into the DB.

It is used in two different ways: 
 
1. Once, grab all posts (and associated data) from a page to initially populate the database.  
2. Repeatedly, grab the last n posts (and associated data) to update the database.

The specific data we need to get is: 
 
* All the posts to a certain page. For each post we store  
     * The id, message, and time of posting 
     * All comments and reactions
* For a comment we store
     * The id, message, time of posting, and author
     * All reply comments and reactions
     * All tags
* For a reaction we store
     * The type (Like, Angry, etc), and author
* We also store all users who have interacted with the page by
     * Reacting to a post or comment
     * Making a comment
     * Being tagged in a comment

It's important that the program be:

* Modular, so that we can get fetch content from arbitrary pages and do arbitrary things to it.  
* Reliable. If one part of the program fails, for example parsing one JSON object out of 20, the rest of the program should continue as best as it can. 
* Log what it does, and any failures that occur. 
* Easy to understand and extend.

## Why Haskell?

There are a few reasons I decided to write this in Haskell.

The underlying general pattern of "Get some input, parse it into objects, then process the objects" lends itself well to Haskell. Haskell also handles recursive data types very cleanly, as we'll see in the **Paging** section. Finally, the Haskell development style of "Write a library with functions that manipulate objects in your problem space, and then use those to write a small executable that solves a specific problem" will suit this rapidly changing project nicely.

## Types

A good place to start is to think of what types of objects you're going to deal with. We can define the types of `Post`s, `Comment`s, `Reaction`s and `User`s in the obvious way. 

~~~ {.haskell}
data Post = Post { postID :: Integer, postmessage :: String, posttime :: UTCTime, postcomments :: [Comment], postreactions :: [Reaction] } deriving Show

data Comment = Comment { commentid :: Integer, parentid :: Integer, commenttime::String, commentmessage :: String, commentauthor :: User, commenttags :: [User] } deriving Show

data User = User { userid :: Integer, username :: String } deriving Show

data Reaction = Reaction { reactiontype :: String, reactionauthor :: User } deriving Show
~~~

## Facebook Graph API
If you have the ID for a post you can use the Facebook Graph API to get all sorts of information about it. An [access token](https://developers.facebook.com/docs/facebook-login/access-tokens/) is also required for each query. The standard query we'll be making will be an HTTPS GET request on a URL that looks like 
````
https://graph.facebook.com/v2.10/[POST_ID]?fields=reactions%2Ccomments%7Bmessage%2Cfrom%2Cmessage_tags%2Ccreated_time%7D%2Cmessage%2Ccreated_time&access_token=[TOKEN]
````

This will return a JSON object that looks like the anonymized example below.

{{<highlight json>}}
{
  "reactions": {
    "data": [
      {
        "id": "0123456789",
        "name": "John Doe"
        "type": "LIKE"
      }
    ],
    "paging": {
      "cursors": {
        "before": "TVRBd01EQXdORGd5TXpRM016ZAzJPakUxTVRZAd016TTBORFE2TWpVME1EazJNVFl4TXc9PQZDZD",
        "after": "TVRBd01EQTJNVEUwTURrd01UazNPakUxTVRZAd01qazVNakk2TnpnNE5qUTRNRE0zT1RFek16RXkZD"
      }
    }
  },
  "comments": {
    "data": [
      {
        "message": "Jane Doe oh. My. God.",
        "from": {
          "name": "John Doe",
          "id": "0123456789"
        },
        "message_tags": [
          {
            "id": "9876543210",
            "length": 16,
            "name": "Jane Doe",
            "offset": 0,
            "type": "user"
          }
        ],
        "created_time": "2018-01-15T13:42:10+0000",
        "id": "561927510810787_562062857463919"
      },
    ],
    "paging": {
      "cursors": {
        "before": "WTI5dGJXVnVkRjlqZAFhKemIzSTZAOVFl5TURZAeU9EVTNORFl6T1RFNU9qRTFNVFl3TWpNM016QT0ZD",
        "after": "WTI5dGJXVnVkRjlqZAFhKemIzSTZAOVFl5TVRVMk56QTBNVEl4TWpBeE9qRTFNVFl3TXpjME5EYz0ZD"
      }
    }
  },
  "message": "#Oxfeud_7254

To the girl outside cellar who stole my burger, that wasn’t ok.",
  "created_time": "2018-01-15T05:40:50+0000",
  "id": "409018456101694_561927510810787"
}
~~~

How easy! Everything we need is right there. Now how do we write a function that gets this for us? 

Haskell has a few http libraries, but I like the simplicity of [req](https://hackage.haskell.org/package/req-1.0.0/docs/Network-HTTP-Req.html). Looking at the documentation, we see that something like this will work:

~~~ {.haskell}
{-# LANGUAGE OverloadedStrings#-}

import Network.HTTP.Req
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

instance MonadHttp IO where
  handleHttpException = throwIO

options :: Token -> Option scheme
options token =  
  (mconcat [ "fields" =: ("reactions,comments{message,from,message_tags,created_time},message,created_time" :: String), "access_token" =: (token :: String) ] )

postFromFB :: Token -> PostID -> IO B.ByteString
postFromFB token p = do 
    r <- (req 
      GET 
      (https "graph.facebook.com" /: "v2.10" /: (T.pack p)) 
      NoReqBody 
      lbsResponse 
      options token)
  return (responseBody r :: B.ByteString)
~~~
Here `Token` and `PostID` are type synonyms for `String`.

Now all we have to do is use the [Aeson](https://hackage.haskell.org/package/aeson) library to parse this response into a `Post`.

## Parsing
With the Aeson library, you can easily parse JSON into a Haskell object if you jsut define an instance of `FromJSON` for it. I found [this guide](https://artyom.me/aeson) very valuable.

Let's start by parsing a `User`, and building up to `Post` from there.

~~~ {.haskell}

{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

instance FromJSON User where
  parseJSON = withObject "user'" $ \o -> do
    userid' <- o .: "id"
    username <- o.: "name"
    let userid = read userid'
    return User{..} 
~~~

This is completely standard. Now, notice how a `Reaction` JSON object also has "from" and "id" fields, corresponding to the reacting user? We can actually parse a `Reaction` object by reading the reaction type, and getting the user by *parsing the object again, this time as a user*. 

We don't even need to annotate the type of `ParseJSON :: Value -> Parser a` as `Value -> Parser User`, Haskell can infer the type by seeing that we bind it to `reactionauthor`, which it knows is of type `User`. Neat!

~~~ {.haskell}
instance FromJSON Reaction where
  parseJSON = withObject "reaction" $ \o -> do
    reactiontype <- o.: "type"
    reactionauthor <- parseJSON (Object o)
    return Reaction{..}
~~~

We do something similar for parsing `Comment`s. Not every Comment JSON object *has* a `message_tags` field (when the comment doesn't contain any tags the Facebook API doesn't even return a `message_tags` field), so we need to be careful. Thankfully, Aeson allows us to provide a default value if a key doesn't appear. Finally, we can parse the array of tags directly into a list of `User`s. 

~~~ {.haskell}
instance FromJSON Comment where
  parseJSON = withObject "comment" $ \o -> do
    cmessage <- (o .: "message")
    commentid' <- (o.: "id")
    from <- o .: "from"
    author <- parseJSON (Object from)
    commenttime <- o.: "created_time"
    ttags <- o .:? "message_tags" .!= V.empty 
    tags <- parseJSON (Array ttags)
    let commentid = read $ tail . dropWhile (/='_')$ cid' 
        parentid = read $ takeWhile (/='_') $ cid'
    return Comment{..}
~~~

Finally, we can parse a `Post` like this.


~~~ {.haskell}

instance FromJSON PostResponse where
  parseJSON = withObject "response" $ \o -> do
    rs <- o .:? "reactions" .!= (H.empty :: H.HashMap T.Text Value)
    rd <- rs .:> "data" .!=  V.empty 
    postreactions <- parseJSON (Array rd)
    cs <- o .:? "comments" .!= (H.empty :: H.HashMap T.Text Value)
    cd <- cs .:? "data" .!= V.empty
    postcomments <- parseJSON (Array cd)  
    postmessage <- o .: "message"
    time_string <- o.: "created_time"
    postid' <- o.: "id"
    let postid = read $ takeWhile (/='_') postid'
        posttime = read time_string :: UTCTime
    return Post{..} 
~~~

To tidy things up we can change the response type of our `req` request to a `jsonResponse` and actually parse the JSON immediately, like so:

~~~ {.haskell}
postFromFB :: Token -> PostID -> IO Post
postFromFB token p = do 
    r <- (req 
      GET 
      (https "graph.facebook.com" /: "v2.10" /: (T.pack p)) 
      NoReqBody 
      jsonResponse 
      options token)
  return (responseBody r :: Post)
~~~

Or can we...

## Paging

The Facebook Graph API, by default, only returns the most recent 25 things of each type. That is, when making a request on a post with 26 comments and 26 reactions, only the first 25 reactions will appear in the `reactions.data` field, and same for the comments. This limit can be increased to 100, but that's just kicking the can down the road. Given that many posts have more that 100 reactions and/or comments, how do we get *all* of them?

The answer is in the `paging` field. This (sometimes, if there are any extra things) contains a `next` field that you can put into your query string to get the next n things. This means we need to rethink our `postFromFB` function, because the response body of an API call *is not a `Post`*, but some top level `Post` information (the message and time), the first 25 `Comment`s and *possibly a pointer to some more `Comments`*, and the first 25 `Reaction`s and *possibly a pointer to some more `Reaction`s*.

We can abstract this pattern of "Some things, and possibly instructions on how to get more things" as a new data type 

~~~ {.haskell}
data Paging a = Paging {content :: [a], next :: Maybe String}
~~~
and class,
~~~ {.haskell}
class CanPage a where getNext :: Token -> PostID -> Paging a -> IO (Maybe (Paging a))
~~~

First we make an instance of `FromJSON (Paging a)` whenever we have an instance of `FromJSON a`:
~~~ {.haskell}
instance (FromJSON a) => FromJSON (Paging a) where 
  parseJSON = withObject "" $ \o-> do
    adata <- o.: "data"
    content <- parseJSON (Array adata)
    pg <- o .:? "paging" .!= (H.empty :: H.HashMap T.Text Value)
    cr <- pg .:? "cursors" .!= (H.empty :: H.HashMap T.Text Value)
    next <- cr .:? "after"
    return Paging{..}
~~~

Then we make a type for initial responses:

~~~ {.haskell}
--(id, message, time, comments, reactions)
data PostResponse = PostResponse (PostID, String, UTCTime, Paging Comment, Paging Reaction)
~~~

and an instance of `FromJSON`

~~~ {.haskell}
instance FromJSON PostResponse where
  parseJSON = withObject "response" $ \o -> do
    r <- o .:? "reactions" .!= (H.empty :: H.HashMap T.Text Value)
    rs <- parseJSON (Object r)
    c <- o .:? "comments" .!= (H.empty :: H.HashMap T.Text Value)
    cs <- parseJSON (Object c)  
    message <- o .: "message"
    time_string <- o.: "created_time"
    ids <- o.: "id"
    return $  PostResponse (ids, message, time_string, cs, rs)
~~~

So the function `postFromFB` now has signature `Token -> PostID -> PostResponse`. We just need a way to elaborate a `PostResponse` into a `Post`.

We define some helper methods to make making API requests simpler.

~~~ {.haskell}
jsonRequest ::  (FromJSON c) => Url scheme -> Option scheme ->  IO ( c)
jsonRequest url opts=  
    (req GET 
    url
    NoReqBody 
    jsonResponse
    opts ) >>= (\r ->   return (responseBody r )  )

options':: String -> Token -> String -> Option scheme
options' what token next = mconcat ["fields" =: ("message,message_tags,from,created_time,type,id,name"::String),"access_token" =: (token :: String),
              "limit" =: ("100" :: String),
              "after" =: (next :: String)] 

makePostURL :: PostID -> String -> Url 'Https
makePostURL p what = (https "graph.facebook.com" /: "v2.10" /: (T.pack p) /: (T.pack what) )  

pagingFromFB :: (FromJSON c)=> Token -> String -> String ->  PostID -> IO (Paging c)
pagingFromFB token what n  parent = do
  let url = makePostURL parent what
      opts = options' what token n
  jsonRequest url opts
~~~

Now we need to deal with paging comments and reactions.

~~~ {.haskell}
nextPaging ::  (FromJSON a) => Token -> String -> PostID -> Paging a ->IO (Maybe (Paging a))
nextPaging token what postid (Paging{..}) = case next of 
    Nothing -> return Nothing
    Just n -> do
      p <- pagingFromFB token what n postid
      return $ Just p

instance CanPage Comment where
  getNext token  = nextPaging token "comments" 
    
instance CanPage Reaction where
  getNext token = nextPaging token "reactions"
~~~

So given a `Paging a` we can use `nextPaging` to get the next `Paging a`, if it exists. We just need some function to repeat this until there are no more to get, and collect all the `a`s along the way. 
    
~~~ {.haskell}
elab :: (CanPage c) =>  Token -> PostID -> Paging c -> IO [c]
elab token postid (p@Paging{..}) = case next of
  Nothing -> return content
  Just n -> do
    p' <- getNext token postid p
    case p' of
      Nothing -> return content
      Just (p'') -> do
        cs <- elab token postid p''
        return $ content++cs
~~~

We finish up here by defining a function to make a `Post` from a `PostResponse` (and a `Token`).

~~~ {.haskell}
makePost :: Token ->  PostResponse -> IO Post
makePost token (PostResponse(ids,m,t,pcs,prs)) = do
  let postmessage = m
      postID = ids 
      postnum = Nothing
      posttime = t 
      postrefs = [] 
  postcomments <- elab token postID pcs
  postreactions <- elab token postID prs
  return Post{..}
     
makePostFromFB :: Token -> PostID -> IO Post   
makePostFromFB token p  = postFromFB token p >>= makePost token
~~~

## Cleaning up
One thing to notice is that nearly every function we've defined takes a `Token` and just passes it to another function. We can hide this by defining a new monad type `FB a = Token -> IO a`, so that a value of type `FB a` represents a computation that, given a `Token`, makes some calls to the Facebook API and returns an `a`.

~~~ {.haskell}
newtype FB a = FB (Token -> IO a)

instance Monad FB where
  return  x  = FB (\t -> return x)
  FB xm >>= f  = FB (\t -> xm t >>= (\a ->  let FB ym = f a in ym t) )
instance Functor FB where
  fmap f (FB xm) = FB (\t -> xm t >>= (\a -> return $ f a))
instance Applicative FB where
  pure = return
  (FB xmm) <*> (FB ym) = FB (\t -> ym t >>= (\a -> xmm t >>= (\f -> return  $ f a)))
instance MonadIO FB where
  liftIO xm = FB( \t -> xm >>= \x -> return x)

runFB ::  FB a -> Token -> IO a
runFB (FB xm) token = xm token 
~~~

Then we can change most of our functions to return `FB a`, and in `main` call them like:

~~~ {.haskell}
main :: IO ()
main = do
  token <- getToken
  postid <- getID
  post <- runFB (makePostFromFB postid) token
  print post
~~~

(where `getToken :: IO Token, getID :: IO PostID, makePostFromFB :: PostID -> FB Post`.)

## Database
Now that we can get any post we want, we need some place to put them all. A MYSQL database will do. We'll use the following schema, with all the obvious foreign key constraints.

 Table   | Field       | Type
---------|-------------|---------
comments |	id_comment |	bigint
comments |	id_parent  |	bigint
comments |	id_author  |	bigint
comments |	message	   |text
comments |	time	     |timestamp
posts    |	id_post	   |bigint
posts    |	message	   |text
posts    |	feudnum	   |int
posts    |	time	     |timestamp
reactions|	type	     |varchar
reactions|	id_author  |	bigint
reactions|	id_post	   |bigint
refs		 |id_src	     |bigint
refs		 |num_dest	   |int
tags 		 |id_comment   |	bigint
tags 	 	 |tagged_user  |	bigint
users	   |id_user	     |bigint
users	   |name	       |tinytext


We're going to use the [mysql-simple](https://hackage.haskell.org/package/mysql-simple-0.4.4/docs/Database-MySQL-Simple.html) library for putting posts in the database.

The idea here is to construct query templates like `myQuery = "INSERT INTO table(v1,v2) VALUES(?,?)"`, then execute them by calling `execute connection myQuery (x1,x2)`.

Mostly we're just going to be `INSERT`ing rows into a table. Sometimes we'll want to `UPDATE`, like if the content of a post or comment changes.

So, to insert the data from a comment we could write a function like:

~~~ {.haskell}
insertComment :: Connection -> Comment -> IO ()
insertComment cx Comment{..} = catch 
  (execute cx "INSERT INTO comments..." ... )
  (\e -> let err = show (e :: SomeException) in (print err) >> catch 
    (execute cx "UPDATE comments..." ...)
    (\e' -> let err' = show (e' :: SomeException) in print err'))
~~~
This will try inserting, and if that fails print the exception and try updating, and if *that* fails too, print the exception and exit gracefully. It works, but it's not ideal.

There's a lot of code reuse inside `insertComment`, and given that we'll need something similar for inserting `Post`s, `Reaction`s, etc, we should try and pull out all the boilerplate, because fundamentally, there's not a lot of difference between putting a `Post` or a `Comment` into the database, the only thing that really changes is what table it goes in, and what query you use.

So let's start by keeping track of all the tables we'll be accessing, and let's make an appropriate pair of `INSERT` and `UPDATE` queries for each of them.

~~~ {.haskell}
data Table = Po | Co | Us | Re | Reac | Ta deriving Show

queries :: Table -> (Query, Query)
queries Po = ("INSERT INTO posts(id_post, feudnum, message, time) VALUES(?,?,?,?)", "UPDATE posts SET feudnum = ?, message = ?, time = ? WHERE id_post = ?")
queries Co = ("INSERT INTO comments(id_comment, id_author, message, time, id_parent) VALUES(?,?,?,?,?)", "UPDATE comments SET id_author=?, message=?, time=?,id_parent=? WHERE id_comment=?")
queries Us = ("INSERT INTO users(id_user,name) VALUES(?,?)", "UPDATE users SET name=? WHERE id_user=?")
queries Re = ("INSERT INTO refs(id_src,num_dest) VALUES(?,?)", "")
queries Reac = ("INSERT INTO reactions(id_post, type, id_author) VALUES(?,?,?)", "")
queries Ta = ("INSERT INTO tags(id_comment, tagged_user) VALUES(?,?)", "")
~~~

Now we can define a helper method to encapsulate the functionality of "try this, and if that fails try this instead":

~~~ {.haskell}
tryBoth :: IO a -> IO a -> (SomeException -> IO a) -> IO a
tryBoth a1 a2 handler = catch 
  a1 
  (\e -> let e' = show (e :: SomeException) in catch a2 handler)
~~~

And now we can make a generic function to put things into the database.

~~~ {.haskell}
put :: (QueryParams a, QueryParams b) => Table -> a -> b -> Connection -> IO ()
put t a b cx = let (ins, ups) = queries t in tryBoth
  (execute_ cx ins a)
  (execute_ cx ups b)
  (\e -> print $ "error" ++ show t ++ " " ++ show e)
~~~

Finally, we need a function that actually puts a post into the database. We proceed inductively, starting at the component types and building up to `Post`. 

There is something to beware of. As we have foreign key constraints in the database, we need to be careful with the order in which we insert things. For example, since the table `tags` has a column `id_comment` with a foreign key constraint to the `id_comment` column in the `comments` table, *we better not try to insert a comment's tags before we insert the comment itself.*

Thankfully we can guarantee this quite easily. If `xm, ym :: IO ()` then in the program

~~~ {.haskell}
main :: IO ()
main = do
  xm
  ym
~~~

any side effects of `xm` will take happen before those of `ym`. Why is that the case? Well, the `do` notation is just syntactic sugar for 

~~~ {.haskell}
main = xm >>= const ym
~~~

We can [think of the `IO` monad](https://wiki.haskell.org/IO_inside) as a special instance of the `State` monad, defined as 
~~~ {.haskell}
type IO a = RealWorld -> (a, RealWorld)

return :: a -> IO a
return x r = (x, r)

(>>=) :: IO a -> (a -> IO b) -> IO b
(xm >>= f) r = let (y, r') = xm r in f y $ r'
~~~

So our program above simplifies to

~~~ {.haskell}
main r = let (r', ()) = xm r in ym r'
~~~

And we see that because `xm` has to be evaulated before `ym` is. 

Putting a user into the database is simple.

~~~ {.haskell}
putUser :: User -> Connection -> IO ()
putUser (User{..}) = put Us (userid,username) (username,userid) 
~~~

To insert a reaction, we put in the reacting user first and then the reaction itself.

~~~ {.haskell}
putReaction :: String -> Reaction -> Connection -> IO ()
putReaction p (Reaction{..}) cx = do
  put Us (userid reactor, username reactor) (username reactor, userid reactor) cx
  put Reac(p, rtype, userid reactor) () cx
~~~

Inserting a comment is a little more involved. We insert the author, the comment itself, and then any tagged users.

~~~ {.haskell}
putComment ::  Comment -> Connection -> IO ()
putComment (Comment{..}) cx = do
  putUser author cx
  put Co (commentid, userid author, commentmessage, commenttime, parentid) (userid author, commentmessage, commenttime, parentid, commentid) cx
  sequence_ $ map (\u -> (putUser u cx) >> (put Ta (commentid, userid u) () cx)) tags
~~~

And from here it's straightforward to define how to insert posts.

~~~ {.haskell}
putPost :: ConnectInfo -> Post -> IO ()
putPost ci (Post{..}) = do
  cx <- connect ci
  sequence_ $ map (\c -> putComment c cx) postcomments
  sequence_ $ map (\r -> putReaction postid r cx) postreactions
  sequence_ $ map (\r -> put Re (postid, r) () cx) postrefs 
  put Po (postid, feudnum, postmessage, posttime) (feudnum, postmessage, posttime, postid) cx
~~~

## Getting Post IDs
So, given a post ID number, we can fetch the data from Facebook and put it into a database. One might wonder, however, how to actually *get the post IDs* in the first place...

Well, if you make a Graph API query on a pages `feed` value, you get a list of the last 25 posts made. Then, in a slight modification from what was discussed above, you can use the paging features to get the 25 previous posts, and the previous 25 posts, and so on. Then, because a page has only a finite amount of posts we can argue by induction that this process terminates, so QED we can get all the post IDs.

Actually, there's quite a serious issue with the argument above that we haven't taken into account.

No, not the possibility that in the several hundred milliseconds between making one Graph API call and processing the 25 posts we get and the next one, more than 25 new posts have been submitted.

Rather, it's the fact that the Graph API is *really* buggy. Once you page sufficiently far back (roughly 1000 posts, I found), Graph API will start missing posts, and only returning about 10% of them. This is a [known issue](https://developers.facebook.com/bugs/1907082866287389/), and can be fixed by querying the `published_posts` field instead of `feed`. Unfortunately, that requires a page access token which I obviously don't have.

Instead we can exploit the fact that every Oxfeud post starts with `#Oxfeud_[n]`, for some unique natural number `n`. So, if you wanted to view the 1000th Oxfeud post, you could simply type `#Oxfeud_1000` and Facebook's nifty hashtag search feature will display it as the first result.

From here the solution is simple. Just use Graph API's hashtag search function to search for posts by their Feud Number, and get everything you need from there, because of course Graph API will let you search for any post by hashtag, right? [Oh..](https://developers.facebook.com/docs/public_feed).

Our old friend `wget` will have to do. We can just download the search results page for every n between 1 and the current number. Extracting the actual post IDs is a [nice exercise in Regex.](http://willmatthews.xyz/posts/facebook-hashtag-search/)

On the other hand, we can actually get the last ~1000 posts using the Graph API. Using the query string

````
[PAGEID]/?fields=feed{reactions,comments{message,message_tags,from,created_time},message,created_time}&access_token=[TOKEN]
````

we get a nice response that we can straightforwardly parse into a `Paging PostResponse` object. If we like, we can call `elab` on it to fully page it, resulting in about 2000 `PostResponse`s. Or, we can define a function `elab' :: Int -> PageID -> Paging a -> FB [a]` so that `elab' n` pages back n times, and take n to be around 10 or so.

## Putting it all together

A program to read a list of post IDs from a text file and add the corresponding posts to the database could look like this:

~~~ {.haskell}
main :: IO ()
main = do
  postids' <- readFile "post_ids.txt"
  token <- readFile "token.txt"
  let ci = defaultConnectInfo
      postids = lines postids'
  posts <- runFB (sequence $ map makePostFromFB postids) token
  sequence_ $ map (putPost ci) posts 
{{</highlight >}}

We can also relax the type signature of, and slightly modify the function `pagingFromFB` to 

~~~ {.haskell}
jsonFromFB :: (FromJSON c) => PageID -> FB c
{{</highlight >}}

(where `PageID` is just a synonym for `String`) so that it uses the query string above, and hence returns a `FB (Paging PostResponse)`.

Then a program to update the last 1000 posts might look like:

~~~ {.haskell}
main :: IO ()
main = do
  token <- readFile "token.txt"
  let ci = defaultConnectInfo
      pageid = "..."
  posts <- runFB (jsonFromFB pageid >>= elab 10 pageid >>= map makePost) token
  sequence_ $ map (putPost ci) posts
~~~


## Handling Exceptions

The next step is to handle failure. There are three main ways our program can fail:

* We can fail to make a valid `Req` request
* We can fail to parse a JSON object
* We can fail to connect to/make a query on the database

Let's start with the `FB` type. Given that *any* call to Graph API can potentially fail, it would make sense to redefine it as `newtype FB a = FB (Token -> IO (Maybe a))`. For readability, we'll prefer to use the standard [monad transformer](https://hackage.haskell.org/package/transformers-0.5.5.0/docs/Control-Monad-Trans-Maybe.html), so define `type MFB a = MaybeT FB a`.

It's useful to make a helper function for promoting `Maybe a`s to `MFB a`s.

~~~ {.haskell}
liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return 
~~~

Now we just go around and change every function that returns `FB a` to return `MFB a`. This is all pretty standard. One nice thing pops out from the fact that `MFB a` is an instance of `Applicative`: we can rewrite `elab` in a rather more natural way.

~~~ {.haskell}
elab :: CanPage c => PostID -> Paging c -> MFB [c]
elab postid (p@Paging{..}) = 
  (liftMaybe next >> getNext postid p >>= elab postid >>= return . (content++)) <|> (return content)
~~~




