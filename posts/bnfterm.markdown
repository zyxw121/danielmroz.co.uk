---
title: "A lambda-term that finds beta-normal forms"
description: "Compiling to the lambda calculus"
date: "2018-03-22"
lastmod: "2018-04-04"
latex: true
---

The untyped lambda calclus is Turing Complete, in the sense that every program can be defined by a $\lambda$-term. Furthermore, there is an algorithm that finds the $\beta$-normal form of a $\lambda$-term (if it has one): repeated left-most reduction.

So, let's try to find the term that defines that algorithm!

The strategy is:

1. Write a program to find $\beta$-normal forms in Haskell
2. Translate this into a small toy language 
3. Write a compiler (in Haskell) from said language into λ-terms 


## Finding $\beta$-normal forms in Haskell
We start by defining our types.
 
~~~ {.haskell}
newtype Name = Name String deriving (Eq)
data Term = Var Name | Abs Name Term | App Term Term 
~~~

We have some choice of how to handle variables. It is important that we are able to generate a "fresh" variable from any finite collection - this will be needed when we implement substitution. This would be easily done if we used integers to name variables ("one plus the biggest one"), but it is not so hard to do with strings (as we will see below), and string variables are nice to use.


~~~ {.haskell}
gen :: Int -> [Name]
gen 0 =[Name "a"]
gen 1 = map (Name . (:[])) ['a'..'z']
gen n = let ns = gen (n-1) in [Name (s:ss) | s <- ['a'..'z'], (Name ss) <- ns]

fresh :: [Name] -> Name
fresh ns =
  let n = length ns
      k = length $ takeWhile (<=n) $ iterate (*26) 1 
      ps = take (n+1) $ gen k in 
        head $ filter (not . (flip elem) ns) ps 
~~~

The expression `gen n` returns the list of all variables consisting of lowercase letters of length `n`. Crucially they are all distinct. We use it in `frees ns` to get a list of `(length ns)+1` distinct names so that we can find one which is not in `ns`.

~~~ {.haskell}
frees :: Term -> [Name]
frees (Var n) = [n]
frees (App s t) = frees s ++ frees t
frees (Abs x s) = filter (/=x) $ frees s        
~~~

The function `frees` is used to find the free variables in a term. Our approach here is certainly naive. Why not put the free variables in a `Set`, or at least an ordered list? As it turns out, the overhead induced by more efficient data structures actually makes `frees` noticably slower for reasonably sized inputs. 

~~~ {.haskell}
sub (Var y) t x  = if x==y then t else (Var y)
sub (App u v) t x  = App (sub u t x) (sub v t x)
sub (Abs y s) t x = let ts = frees t in
  if y==x || y `elem` ts then let z = fresh (ts ++ frees s ++ [x]) in Abs z $ sub (sub s (Var z) y) t x 
  else Abs y (sub s t x)

bred :: Term -> Maybe Term
bred (App (Abs x s) v) =  Just $ sub s v x
bred _ = Nothing
~~~

Substitution and β-reduction are now straightforward.

~~~ {.haskell}
lred :: Term -> Maybe Term
lred (Var x) = Nothing
lred u@(App s t) = bred u
  <|> do
    x <- lred s 
    return $ App x t
  <|> do
    x <- lred t
    return $ App s x 
lred (Abs x s) = lred s >>= Just . Abs x
~~~

We can use the `Alternative` instance of `Maybe` to cleanly implement left-most reduction.

~~~ {.haskell}
bnf :: Term -> Term
bnf t = case lred t of
  Just t' -> bnf t'
  Nothing -> t 
~~~
And repeat.


## A small language
[Click here to read about LamE (The LAMbda Evaluater).](posts/lame.html)
Most of the rest of this post will be written in LamE. The syntax is a blend of Lisp and Haskell, and is hopefully mostly obvious.

## Translating to LamE
LamE doesn't have all the high-level features of Haskell. So we'll have to get our hands dirty and implement them ourselves.

Let's start by defining some functions about lists: `append`, `filter`, `map`, `concat`, `length`, `iterate`, `take`, `takeWhile`, and `elem` are what we'll need.


~~~ {.haskell}
rec append = func (x y) (
  if empty x 
  then y 
  else cons (head x) (append (tail x) y));

rec filter = func (p xs) (
  if empty xs
  then xs
  else (if p (head xs) 
    then cons (head xs) (filter p (tail xs)) 
    else filter p (tail xs)));

rec map = func (f xs) (
  if empty xs 
  then xs
  else cons (f (head xs)) (map f (tail xs)) );

rec concat = func (xss) (
  if empty xss 
  then xss
  else append (head xss) (concat (tail xss)));

rec length = func (xs) (
  if empty xs 
  then 0 
  else + 1 (length (tail xs)));

rec takeWhile = func (p xs) (
  if empty xs 
  then xs 
  else if p (head xs) 
    then cons (head x) (takeWhile p (tail x)) 
    else (takeWhile p (tail x)));

rec iterate = func (f x) (
  cons x (iterate f (f x)));

rec take = func (n xs) (
  if == 0 n 
  then nil 
  else cons (head x) (take (- n 1) (tail xs)));

~~~

Now `elem :: Eq a => a -> [a] -> Bool` is a little troublesome. We don't have typeclasses in LamE, so we can't overload the `==` operator. So we'll need to write a specialized function for checking if a string is a member of a list of strings.


~~~ {.haskell}
rec elemStr = func (x xs) (
  if empty xs 
  then false 
  else if =s x (head xs) 
    then true 
    else elemStr x (tail xs));

~~~

Now we can start recreating our program in LamE.

~~~ {.haskell}
rec frees = func (t) (match t as
  (Var n) ([n])
  (App u v) (append (frees u) (frees v))
  (Abs n v) ( filter (func (m) (not (=s n m))) (frees v))); 

~~~

The function `frees` is quite straightforward.

~~~ {.haskell}
val names = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"];
val chars = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'];

rec gen = func (n) (
  if == n 0 
  then ["a"]
  else (if == n 1 
    then names
    else let val ns = gen (- n 1) in (
      let val f = func (s) (
        let val g = func (t) (
          +s s t) 
        in concat (map g names)) 
      in (concat (map f chars)))));

~~~

We can rewrite list comprehensions in terms of `concat` and `map` to get this rather verbose definition of `gen`.

~~~ {.haskell}
val fresh = func (ns) (
  let val n = length ns in (
  let val k = length (takeWhile (leq n) (iterate (func (x)  (* x 26)) 1)) in (
  let val ps = take (+ 1 n) (gen k) in (
    head (filter (func (x) (not (elemStr x ns))) ps)))));

rec sub = func (s t x) (match s as
  (Var a) (if =s x a then t else s)
  (App a b) (App (sub a t x) (sub b t x))
  (Abs a b) (let val ts = frees t in (
    if or (=s x a) (elemStr a ts) 
    then let val z = fresh (concat [ts, frees b, [a]]) in (Abs z (sub (sub s (Var z) y) t x ) )
    else Abs a (sub t x b))));

~~~

Defining `fresh` and `sub` is straightforward.


~~~ {.haskell}
val bred = func (t) (match t as 
  (Var x) (nil)
  (App p q) (match p as
    (Var x) (nil)
    (App a b) (nil)
    (Abs x s) ([sub s q x]))
  (Abs x s) (nil));

~~~

We don't have `Maybe`, but we can simulate it with lists, where `[] = Nothing` and `[a] = Just a`.  


~~~ {.haskell}
rec lred = func (t) (match t as
  (Var x) (nil)
  (App p q) (
    if empty (bred (App p q)) 
    then (if empty (lred p) 
      then (if empty (lred q) 
        then nil  
        else App p (head (lred q)))  
      else App (head (lred p)) q ) 
    else head (bred (App p q)))
  (Abs x s) (if empty (lred s) 
    then nil 
    else Abs x (head (lred x))));

~~~

Leftmost reduction is rather tedious to define.

~~~ {.haskell}
rec bnf = func (s) ( 
  let val xs = lred s in 
  (if empty xs 
  then s 
  else bnf (head xs))
~~~

Finally, we get $\beta$-reduction!

The program compiles into [this very long term](/bnf.txt)

