---
title: "Type level Haskell: FizzBuzz"
description: "Some fun with Haskell's type system"
date: "2017-12-14"
lastmod: "2017-12-22"
---
## Introduction 
We'll start with some arithmetic. We can define `Nat` to be the type of natural numbers, and implement an addition function. By evaluating this function in GHCi we can compute additions of natural numbers. If we try to add, say, a string and a float, we quite naturally get a type error.

~~~ {.haskell}
data Nat = Zero | Succ Nat  

add :: Nat -> Nat -> Nat
add Z x = x
add (S n) x = S (add n x)


*Main> add (Succ Zero) (Succ Zero) 
Succ (Succ Zero)

*Main> add "Hello World!" 4.5
    Couldn't match expected type Nat with actual type [Char]
    In the first argument of add, namely "Hello World!"
    In the expression: add "Hello World!" 4.5
    In an equation for 'it': it = add "Hello World!" 4.5
~~~


 
Moving on, we try something else. 


~~~ {.haskell}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

data Z 
data S n

class Add a b c | a b -> c where add :: a -> b -> c
instance Add Z b b 
instance (Add a b c) => Add (S a) y (S c)


*Main> :type add (undefined :: S Z) (undefined :: S Z)
add (undefined :: S Z) (undefined :: S Z) :: S (S Z)

*Main> :type add (undefined :: String) (undefined :: Float)
add (undefined :: String) (undefined :: Float) :: Add String Float c => c
~~~

What's going on here? It looks like we've computed the sum of two numbers. But we haven't defined any functions, or even values, just types and instances of type classes.

Let's take a closer look. We first define two types, `Z` and `S n` with no constructors, so the only value they have is the bottom value `undefined`. Then we declare a type class `Add a b c` that implements a single method `add :: a- > b -> c`. There are two things to note here:

1. Unlike the type class `Eq x`, `Add a b c` has three parameters. This isn't supported in base Haskell, so we have to add a language extension. Now, just as we can think of single-paramater type classes as *sets* of types, we can think of n-paramater type classes as *n-ary relations* on types.

2. We've given `Add a b c` the functional dependency `a b -> c`. This means that, given any two types `x` and `y`, Haskell will only allow an instance `Add x y z` for one type `z`. In other words, the 3-ary relation on types `Add` is actually a (partial) function `Type -> Type -> Type`. This means that Haskell is ably to fully infer the type of the first expression. Type checking the second expression doesn't give a type error, since it is possible that there is an instance of `Add String Float c` defined somewhere in another module, but because it can't see any such instance, it isn't able to fully infer the type.

In this way, we have translated our computation to the *type level*. Can we do this with more complicated computations?


## FizzBuzz
The FizzBuzz test is a much-blogged-about interview question. It goes like this: 

> Write a program that prints the numbers from 1 to 100. But for multiples of three print “Fizz” instead of the number and for the multiples of five print “Buzz”. For numbers which are multiples of both three and five print “FizzBuzz”. 

 Here's a Haskell solution: 

~~~ {.haskell}
fizzes :: Integer -> Bool
fizzes n = n `mod` 3 == 0

buzzes :: Integer -> Bool
buzzes n = n `mod` 5 == 0

decide :: Int -> String
decide n | fizzes n && buzzes n = "FizzBuzz"
         | fizzes n = "Fizz"
         | buzzes n = "Buzz"
         | otherwise = show n

fizzbuzz :: Int -> [String]
fizzbuzz n = map decide [1..n]
~~~

We get the answer by evaluating `fizzbuzz 100`

~~~ {.haskell}
*Main> fizzbuzz 100
["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz","16","17","Fizz","19","Buzz","Fizz","22","23","Fizz","Buzz","26","Fizz","28","29","FizzBuzz","31","32","Fizz","34","Buzz","Fizz","37","38","Fizz","Buzz","41","Fizz","43","44","FizzBuzz","46","47","Fizz","49","Buzz","Fizz","52","53","Fizz","Buzz","56","Fizz","58","59","FizzBuzz","61","62","Fizz","64","Buzz","Fizz","67","68","Fizz","Buzz","71","Fizz","73","74","FizzBuzz","76","77","Fizz","79","Buzz","Fizz","82","83","Fizz","Buzz","86","Fizz","88","89","FizzBuzz","91","92","Fizz","94","Buzz","Fizz","97","98","Fizz","Buzz"]
~~~

It's not a particularly nice solution, but it lends itself nicely to translation to the type level. Let's jump right in!


It looks like we're going to need:

* Integers 
* Lists 
* Booleans
* Strings 

Although we can actually get away with only natural numbers, and just three strings... (The latter because type level lists as defined above are heterogeneous, so we can put "numbes" and "strings" in the same list.)
 
~~~ {.haskell}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

data Z
data S n

data Nil
data Cons x xs

data True
data False

data Fizz
data Buzz
data FizzBuzz
~~~


 Now the functions we have to translate are `fizzes`, `buzzes`, `&&`, `decide`, `map`, list comprehensions, and `fizzbuzz`. 
 The first few of these are straightforward: 

~~~ {.haskell}
class Fizzes n b | n -> b
instance Fizzes Z True
instance Fizzes (S Z) False
instance Fizzes (S (S Z)) False
instance (Fizzes n b) => Fizzes (S (S (S n))) b

class Buzzes n b | n -> b
instance Buzzes Z True
instance Buzzes (S Z) False
instance Buzzes (S (S Z)) False
instance Buzzes (S (S (S Z))) False
instance Buzzes (S (S (S (S Z)))) False
instance (Buzzes n b) => Buzzes (S (S (S (S (S n))))) b

class And a b c | a b -> c
instance And True True True
instance And True False False
instance And False True False
instance And False False False
~~~


 We do `decide` in two steps: 

~~~ {.haskell}
class Decide1 n f b r | n f b -> r 
instance Decide1 n True True FizzBuzz
instance Decide1 n True False Fizz
instance Decide1 n False True Buzz
instance Decide1 n False False n

class Decide n r | n -> r 
instance (Fizzes n b, Buzzes n c, Decide1 n b c r) => Decide n r
~~~


 The case of `map :: (a -> b) -> [a] -> [b]` seems a little trickier, but thankfully we don't really need `map`, just `map decide`, which makes things simpler: 


~~~ {.haskell}
class MapDecide xs ys | xs -> ys
instance MapDecide Nil Nil
instance (Decide n r, MapDecide ns rs) => MapDecide (Cons n ns) (Cons r rs)
~~~


 Similarly, we don't need full list comprehensions, just a function `first :: Integer -> [Integer]` that generates the first n natural numbers (`first n = [1..n]`). We'll need a helper function that adds elements to the back of a list for this. 


~~~ {.haskell}
class Snoc x xs ys | x xs -> ys
instance Snoc x Nil (Cons x Nil)
instance (Snoc y xs ys) => Snoc y (Cons x xs) (Cons x ys)

class First n xs | n -> xs
instance First Z (Cons Z Nil)
instance (First n xs, Snoc (S n) xs ys) => First (S n) ys
~~~


Finally we put it all together: 

~~~ {.haskell}
class Answer n rs | n -> rs where answer :: n -> rs
instance (From n ns, MapDecide ns rs) => Answer n rs
~~~

 Let's check our work in GHCi. Starting with some small numbers, everything looks good&mdash;or at least, correct.

~~~ {.haskell}
*Main> :type answer (undefined::Z)
answer (undefined::Z) :: Cons FizzBuzz Nil
*Main> :type answer (undefined::S (S (S (S (S Z)))))
answer (undefined::S (S (S (S (S Z)))))
  :: Cons
       FizzBuzz
       (Cons
          (S Z)
          (Cons
             (S (S Z)) (Cons Fizz (Cons (S (S (S (S Z)))) (Cons Buzz Nil)))))
~~~

 Going up to 100 takes a little longer - about 34 seconds on my machine. Trying n=150 takes even more time (3:26 minutes), and n=200 took around 15 minutes. This suggests that our algorithm has exponential time complexity. Although Haskell's type system is very powerful, we might be better off performing computations in Haskell itself.


