---
title: "LamE"
description: "A small functional language"
date: "2018-04-04"
lastmod: "2018-04-04"
latex: true
---
# The LAMbda Evaluater

## [On GitHub](https://github.com/zyxw121/LamE)

## Defining the syntax
The first thing to do is nail down the syntax of the language. It's important that the concrete syntax is:
* Easy for the interpreter to parse
* Easy for humans to read and write

Requiring lots of parantheses and keywords gives us the first, at a slight hit to the second. 
The syntax is more fully described [here](https://github.com/zyxw121/LamE/blob/master/SYNTAX.md). 

This naturally gives rise to the following data types of expressions and definitions. 

~~~ {.haskell}
data Expr   = VarExp Name
            | CharExp Char
            | ListExp [Expr]
            | StringExp String
            | BoolExp Bool 
            | NumExp Int
            | Match Name (Name, Expr) (Name, Name, Expr) (Name, Name, Expr)
            | If Expr Expr Expr
            | Func [Name] Expr
            | Apply Expr [Expr]
            | Let Defn Expr
            deriving (Eq) 

data Defn   = Val Name Expr 
            | Rec Name Expr
            deriving (Eq) 
~~~

Where `Match t (x,e1) (s,t,e2) (y,p,e3)` comes from `match` expressions:

~~~
match t as
  (Var x) (e1)
  (App s t) (e2)
  (Abs y p) (e3)
~~~

and `If c l r` comes from `if` expressions:

~~~
if c then l else r
~~~
and the meaning of the rest is clear. We may as well define the primitive functions too.

~~~ {.haskell}
data Prim = Plus | Minus | Times | Div | Mod ...
~~~

## Parsing
We use [Parsec](https://hackage.haskell.org/package/parsec) to parse strings into expressions. This comes with a [handy method](https://hackage.haskell.org/package/parsec-3.1.13.0/docs/Text-Parsec-Token.html#t:GenLanguageDef) to tokenize a string. Unfortunately, this doesn't work straight out of the box.

The builtin lexer consumes spaces after a token, but spaces are an important feature of our syntax when it comes to function application. So we must recreate the lexer so that it doesn't consume trailing whitespace.

This is not too hard, and from here it is quite straightforward to build parsers for constants, functions, etc.

We run into another snag when we try to parse applications, however. Consider the production rules for a fragment of our grammar:

~~~
<expr>  ::= <variable> | <expr> <expr> | (<expr>)
~~~

It's left-recursive! So the naive parser will sometimes loop. Thankfully, we can fix this by redefining the grammar.

~~~
<expr>  ::= <expr'> | <expr>' <expr>   
<expr'> ::= <variable> | (<expr>)
~~~

The obvious parser now works. We extend this to the full grammar.

## Partial evaluation

Now that we can turn strings into expressions we need to decide what to do with them. Constants (variables, strings, integers, etc) can be turned into into lambda-terms straightforwardly (more on this in the section on Church encodings). But what about an expression like `+ 1 2`? 

We could translate each subexpression (`+`, `1`, and `2`) into a lambda term, and return the application. Or, *given that we know what `+` means*, we can notice that this is equal to the expression `3`.

So this will be our strategy: translate an expression into an intermediate form by reducing it "as much as we can". Then we translate that into a lambda-term.

We need to do a few things all at once now, with the end goal of defining a function `eval :: Expr -> Env -> Value` for some suitable types `Env` and `Value`.

Consider the expression `let val x = 1 in x`. This should certainly evaluate to `1`. To allow for this we'll need some type of environment, that is a mapping from names to values.

~~~ {.haskell}
type Environment a = [(Name, a)]  
type Env = Environment Value
~~~
A list of pairs will do. This will have the added benefit of acting like a stack, so that newer definitions override older ones.

~~~ {.haskell}
find :: Environment a -> Name -> Maybe a
find env x = case (filter (\(a,b) -> a==x) env) of
  [] -> Nothing
  ((a,b):xs) -> Just b 

define :: Name -> a -> Environment a -> Environment a
define x v = define' (x,v)

define' :: (Name, a) -> Environment a -> Environment a
define' = (:)

new_env :: Environment a
new_env = []
~~~

We add some helper functions for manipulating and creating environments.

Let's think about the definition of `Value` now. Constants should pass through eval, so we'll need a value constructor for each of those. Variables should be looked up in the environment. If there's a match we can return that. But what if a variable isn't defined in the environment?

We have a choice. Either throw an error, or just return the variable itself. For simplicity we'll do the latter. This gives us a good start on defining `Value` and `eval`.

~~~ {.haskell}
data Value  = VarVal Name
            | CharVal Char
            | ListVal [Action]
            | StringVal String
            | BoolVal Bool 
            | NumVal Int
            | TermVal Term 

eval :: Expr -> Env -> Value
eval (VarExp n) env = case find env n of
  Just v -> v
  Nothing -> VarVal n
eval (BoolExp b) env = BoolVal b
eval (NumExp n) env = NumVal n
eval (CharExp c) env = CharVal c
eval (StringExp s) env = StringVal s
eval (ListExp xs) env = ListVal (map (flip eval $ env) xs)
~~~

We can handle `If` expressions by hoping that the condition evaluates to a boolean and then choosing the appropriate branch (we leave the case where the condition _is not_ a boolean to later):

~~~ {.haskell}
eval (If c e1 e2) env = case (eval c env) of
  BoolVal b -> if b then (eval e1 env) else (eval e2 env)
~~~

`Match` expressions are a little more complicated but it's a similar idea. Again, we hope that the thing to match on evaluates to a term. Then match in Haskell to pick a branch. The semantices of `match` are that if `t` is the term `Var y` then the expression: 

~~~
match t as
  (Var x) (e1)
  (App s t) (e2)
  (Abs y p) (e3)
~~~

evaluates to `e1` but with the variable `x` set to the value of `y`, and something similar for the other cases.

~~~ {.haskell}
act m@(Match x (y,e1) (s,t,e2) (z,u,e3)) env = case act x env of 
  (TermAct (Var (Name y'))) -> act e1 (define env y (StringAct y') )
  (TermAct (App s' t')) -> act e2 (define (define env s (TermAct s')) t (TermAct t') )
  (TermAct (Abs (Name z') u')) -> act e3 (define (define env u (TermAct u')) z (StringAct z'))
~~~

What about functions? We neatly side-step the issue by wrapping everything up into a closure.

~~~ {.haskell}
data Value = ...
           | Closure [Name] Expr Env

eval (Func xs e) env = Closure xs e env
~~~

We also put off applications and let expressions till later:

~~~ {.haskell}
eval (Apply e es) env = apply (eval e env) (map (flip eval $ env) es)
eval (Let d e) env = eval e (elab d env)
~~~

where the functions `apply :: Value -> [Value] -> Value` and `elab :: Defn -> Env -> Env` are going to do exactly what you'd think.

We will handle the primitive operations by adding a new constructor to `Value`
~~~ {.haskell}
data Value = ...
           | Primitive Prim
~~~

Recall that the epxression `+ 1 2` parses to `Apply (VarExp "+") [NumExp 1, NumExp 2]`. That is, rather than parsing "+" directly in the primitive `Plus` we just parse it as a variable. This is similar to how Haskell does it, and it means we can redefine primitives (`let val + = - in + 1 1 ` will evaluate to `0`).  

We do this by defining a default environment that contains the primitives. 

~~~ {.haskell}
prims = map (\(n,p) -> (Name n, Primitive p)) 
  [ ("+", Plus)
  , ("-", Minus)
  ... 
  ]
~~~

Let's get back to `apply` and `elab`. To apply a `Closure xs e env` to the evaluated arguments `vs` we should evalute `e` in the environment where the paramaters `xs` are bound to the arguments `vs`. We'll have a helper function to apply primitives. Applying anything else will result in a syntactic `Application` value.

~~~ {.haskell}
data Value = ...
           | Application Value [Values]

apply :: Value -> [Values] -> Value
apply (Closure xs e env) vs = eval v (defargs xs vs env)
  where defargs = foldr (define') (id) . zip 
apply (Primitive p) vs = applyPrim p vs
apply v vs = Application v vs 
~~~

We use `applyPrim` to try and simplify things if we can, and just do a formal application if we can't.

~~~ {.haskell}
applyPrim :: Prim -> [Value] -> Value
applyPrim (Plus) [NumVal n, NumVal m] = NumVal (n + m)
applyPrim (Plus) [NumVal n, NumVal m] = NumVal (n - m)
...
applyPrim p vs = Application (Primitve p) vs
~~~

Now we define `elab`, a function which adds a definition to an environment. We have two types of definitions, recursive and value (non-recursive). Value definitions are straightforward. Because the value on the right doesn't reference the name on the left, we can just evaluate it. We won't fully deal with recursive definitions yet, we'll just wrap them up in a new case of `Value`.

~~~ {.haskell}
data Value = ...
           | DefRec Name Expr Env

elab :: Defn -> Env -> Env
elab (Val x e) env = define x (eval e env) env
elab (Rec x e) env = DefRec x e env
~~~

And we're mostly done here, just a few loose strings to be wrapped up later.


## Church encodings (and decodings)

Let's take a step back. The goal is to turn programs into lambda terms. Such a transformation is called a [Church encoding](https://en.wikipedia.org/wiki/Church_encoding).

We'll start by encoding some base values, and using those to build up to encodings for entire programs. The first thing to do is to encode the constant values and primitive functions.

Lets define a typeclass for things that have Church encodings.

~~~ {.haskell}
class Church a where church :: a -> Term
~~~

Church encodings of the boolean values `True = \ab.a` and `False = \ab.b` and the boolean operators `and`, `not`, and `or` are well known. As are encodings for the natural numbers and addition, subtraction, multiplication, comparisons, and test for zero. Division and modulo is a little more involved, but not unbearable. 

Another standard construction is the ordered pair, with terms `pair = \xyz.zxy`, `fst = \p.p(\ab.a)` and `snd = \p.p(\ab.b)` such that `fst (pair x y) = x` and `snd (pair x y) =yq`. 

From here we can define integers as pairs of natural numbers, where the pair `(n,m)` represents the integer `n-m`. It is not hard to extend arithmetic and comparisons from naturals to integers. The only complication is again division and modulo, in which case we have to normalize an integer (put it in the form `(n,0)` or `(0,n)`) and then extract the absolute value and sign.

How could we encode a list? Recall the Haskell definition of a list, `List a = Nil | Cons a (List a)`. When we write a function that takes a list as argument, we usually pattern match on the structure of the list, like so:

~~~ {.haskell}
map :: (a-> b) -> List a -> List b
map f l = case l of
  Nil -> Nil
  (Cons x xs) -> Cons (f x) (map f xs)
~~~

So we can think of a function on a list as consisting of two functions. The first is a constant, and it's returned in the case that list is `Nil`. The second takes two arguments (the head and tail) and is called _with the arguments `x` and `xs`_ if the list is `Cons x xs`. 

Hence we may encode a list `l` as the term `[l]`, where `[Nil] = \ab.a` and `[Cons x y] = \ab.bxy`. Then a function with a list argument may be written like:

~~~ {.haskell}
f :: List a -> List b
f l = case l of
  Nil -> e1
  (Cons x xs) -> e2 x xs
~~~
and we can translate it to `f = \l.l(e1)(e2)`.

So we have the terms `cons = \xyab.bxy` and `nil = ab.a`. Then if we can encode values of type `a` we can also encode values of type `[a]`:

~~~ {.haskell}
instance Church a => Church [a] where
  church = foldr (\a b -> App (App cons a) b) nil . map church
~~~

[In fact this generalizes to arbitrary abstract data types.](https://en.wikipedia.org/wiki/Mogensen–Scott_encoding)

So we can encode lambda terms too.

~~~ {.haskell}
instance Church Term where
  church (Var (Name n)) = abss "abc" $ App (v"a") (church n)
  church (App s t) = abss "abc" $ app2 (v"b") (church s) (church t)
  church (Abs (Name n) s) = abss "abc" $ app2 (v "c") (church n) (church s)
~~~

This has the consequence that if `t` is an encoded term, we can represent a match expression:

~~~
match t as
  (Var x) (e1)
  (App s t) (e2)
  (Abs y p) (e3)
~~~
as the application `t(e1)(e2)(e3)`, which reduces to (e1)x if `t` encodes the term `Var x`, and so on. We have something similar for `if` expressions: if `c` is an encoded boolean then the expression `if c then a else b` can be encoded as `cab`. So we can finish our definiton of `eval`:


~~~ {.haskell}
act m@(Match x (y,e1) (s,t,e2) (z,u,e3)) env = case act x env of 
  ...
  v -> Application v [Closure [y] e1 env, Closure [s,t] e2 env, Closure [z,u] e3 env]
act (If c e1 e2) env = case (act c env) of
  ...
  v -> Application v [act e1 env, act e2 env]
~~~


We can encode characters directly as natural numbers, using Haskell's `Enum` typeclass. This, combined with the previous, gives us an encoding of strings.

We can also go back the other way, from encoded terms to Haskell values. We must be careful about alpha conversion though. In all cases we assume that the term to be decoded is in normal form.

A natural number looks like `\fx.f(f(...f(fx)...))`. So we can decode a term in that form by just counting how many `f`s there are. The `unfoldr` function lets us do this in a nice way.

~~~ {.haskell}
unApp' :: Name -> Term -> Maybe (Name, Term)
unApp' x (App (Var y) s) = if x==y then Just (x, s) else Nothing
unApp' _ _ = Nothing

unNat :: Term -> Int 
unNat (Abs f (Abs x t)) = length . unfoldr (unApp' f) $ t 
~~~

And from here it is straightforward to decode every other type we have introduced.

## Lambda terms with holes

To turn `Value`s into `Term`s, we'll find it helpful to introduce another intermediate form. It is quite common to write lambda-terms like `w = \x.x, W = ww`. Now in the definition of `W`, `w` is not a variable, but a reference to the previously defined term. We will want to do something similar, represent things with a term-like structure but possibly containing other values as well, so that the value `Application (VarVal "+") [VarVal "x", NumVal "3"]` will turn into something like `+x3`, and then we can translate the `+` and `3` into lambda-terms.

We can hence consider a type of "almost terms", things with a term-like structure but possibly containing other values.

~~~ {.haskell}
data Partial a = PVar Name
               | PApp (Partial a) (Partial a)
               | PAbs Name (Partial a)
               | Hole a
~~~

What types of things will we want to put in? For now, the constant values and primitive functions.

~~~ {.haskell}
data Const = CInt In
           | CBool Bool
           | CChar Char
           | CList [Partial Const]
           | CString String
           | CPrim Prim
~~~

It is clear how to make `Const` an instance of `Church`. And in fact, we can do something more. If we have an instance of `Church a`, then we can define an instance of `Church (Partial a)` in the obvious way!

~~~ {.haskell}
instance Church a => Church (Partial a) where
  church p = case p of
    (PVar x) -> Var x
    (PApp s t) -> App (church s) (church t)
    (PAbs x s) -> Abs x (church s)
    (Hole x) -> church x 
~~~

We are very close to being done, we just need to actually turn `Value`s into `Partial Const`s. The constant values translate in the obvious way.

~~~ {.haskell}
partial :: Value -> Partial Const 
partial (VarVal x) = PVar x
partial (NumVal n) = Hole (CInt n)
partial (BoolVal a) = Hole (CBool a)
partial (CharVal c) = Hole (CChar c)
partial (StringVal s) = Hole (CString s)
partial (ListVal xs) = Hole (CList (map partial xs))
partial (Primitive p) = Hole (CPrim p)
~~~

Applications are straightforward too. For the closure `Closure [x1...xn] e env`, which we recall represents a function taking the arguments `x1, ... xn` to the value of `e` in the environment `env`, we partial-ize the body and bind the arguments. 

~~~ {.haskell}
partial (Application f es) = foldl (PApp) (partial f) . map partial $ ex 
partial (Closure xs e env) =  foldr (PAbs) (partial $ eval e env) xs 
~~~

We can think of a recursive definition `rec x = e` as wanting `x` to satisfy the equation `x = e`, where `x` may be a free variable in `e`. The standard trick here is to take x as the term `Y (\x.e)`, where Y is Church's fixed point combinator. So we define such a term `y` and add `Y` as a `Const`.

~~~ {.haskell}
data Const = ...
           | Y

partial (DefRec x e env) = PApp (Hole Y) (partial $ Closure [x] e env)
~~~

So now we have a full chain, `parse :: String -> Expr`, `eval :: Expr -> Env -> Value`, `partial :: Value -> Partial Const`, and `church :: Partial Const -> Term`! 

A simple compiler could be defined as:

~~~ {.haskell}
main = do
  [source] <- getArgs
  print . church . partial $ eval (parse source) prims
~~~

It is not too hard to add some optional flags for allowing reading from input files, writing to output files, etc. But how might we write a REPL?

## Reduction
Before we do that however, let's take a small diversion. The compiler above will, given the input
~~~
 let rec f = func (x) (x) in f true
~~~
produce the term
~~~
((\f.((\x.(f (x x))) (\x.(f (x x))))) (\f x.x)) \a b.a
~~~

This is correct, but we'd like it to produce the fully reduced version instead, simply `'a b.a`. We have [already seen]({{<relref "bnfterm.markdown">}}) how to find beta-normal forms of terms. 

Let's try to implement head reduction. We can head-reduce a term by stripping off the abstractions, and then unpeeling the applications until we get to the head. If it's an abstraction then we can beta-reduce, otherwise we can't do anything. 

~~~ {.haskell}
unabs :: Term -> [Name] -> Maybe (Term, [Name])
unabs t@(App p q) ns = Just (t, ns)
unabs (Abs n s) ns =  unabs s (n:ns) 
unabs _ ns = Nothing

unapp :: Term -> [Term] -> Maybe (Name, Term, Term, [Term])
unapp (App (Abs x t) u) ts = Just (x, t, u, ts) 
unapp (App p q) ts = unapp p (q:ts) 
unapp _ ts = Nothing

hred :: Term -> Maybe Term
hred p = do
  (app, is) <- unabs p []
  (x,t,u,ts) <- unapp app []
  let h = sub t u x
      h' = foldl App h ts
      h'' = foldr Abs h' is
  return h''

hnf :: Term -> Term
hnf t = case hred t of
  Just t' -> hnf t'
  Nothing -> t 
~~~

## Monad of environments

Consider a simple GHCi-like REPL. There is a persistent environment that can be modified by entering definitions. Entering an expression will evaluate it in the current environment. You can load definitions from files, and refresh them.

We can model this behaviour by defining a monad of environments. Or rather, a monad transformer.

First though, we should refine our definition of environments. We will want to seperate the environment into several base environments that have been loaded from files, and the current user environment. A reload command should wipe the user-added definitions, and reload the files.


~~~ {.haskell}
data Module a = Module
  { module_name :: String
  , module_env :: Environment a
  , module_reload :: IO (Maybe (Environment a))
  }
data Environments a = Environments 
  { current_env :: Environment a
  , loaded_modules ::  Map.Map String (Module a)
  }
  deriving Eq
~~~

A module will have a name, a bunch of definitions that get turned into an environment, and a way of reloading itself. An `Environments` object will contain a bunch of modules and a current environment.

~~~ {.haskell}
newtype EnvT v m a = EnvT {runEnvT :: Environments v -> m (a, Environments v) }
~~~

If `m` is a monad then `EnvT v m` wraps `m` in an environment with values of type `v`. This is a specialized version of the `StateT` transformer. We now add some instances.

~~~ {.haskell}
instance Functor m => Functor (EnvT v  m) where
  fmap f xm = EnvT ( \e -> fmap (\(a,e) -> (f a, e)) $ runEnvT xm e )

instance Monad m => Applicative (EnvT v m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (EnvT v m) where
  return x = EnvT (\e -> return (x,e))
  xm >>= f = EnvT (\e ->  runEnvT xm e >>= (\(a,e') -> runEnvT (f a) e' )  )  

instance MonadTrans (EnvT v) where
  lift xm = EnvT (\e -> xm >>= (\a -> return (a,e)))

instance MonadIO m => MonadIO (EnvT v m) where
  liftIO = lift . liftIO
~~~

Now we define some helper functions. Some things we'll want to with an `Environments a` are:
* Define a default one
* Add a new module
* Reload all modules
* Lookup a name
* Add a definition to the current env
* Return a list of module names
* Replace the current env with a different one
So let's get to it!

The default will be empty.


~~~ {.haskell}
new_env_ :: Environment a
new_env_ = []

new_env :: Environments a
new_env = Environments
  { current_env = new_env_
  , loaded_modules = Map.empty
  }
~~~

Adding a new module is not hard.

~~~ {.haskell}
addEnv :: Environments a -> Module a -> Environments a
addEnv envs m= envs{loaded_modules = Map.insert (module_name m) m (loaded_modules envs)}
addM :: Monad m => Module a -> EnvT a m ()
addM m = EnvT (\e -> return ((), addEnv e m))
~~~

To reload the loaded modules we try to call each of their reload methods.

~~~ {.haskell}
reload :: Module a -> IO (Maybe (Module a))
reload m = module_reload m >>= \case 
  Just env -> return . Just $ m{module_env=env}
  Nothing -> return Nothing
reloadM :: MonadIO m => Environments a -> m (Environments a)
reloadM envs = do
  ms <- liftIO . sequence $ Map.map reload (loaded_modules envs) 
  return $ new_env {loaded_modules = Map.foldr f (Map.empty) ms, current_env = new_env_} 
  where
  f x ys = case x of
    Nothing -> ys
    Just y -> Map.insert (module_name y) y ys
~~~

For simplicity, we do not enforce unique definitions. So to look up a name we just iterate through the loaded modules in order.

~~~ {.haskell}
find1 :: Environment a -> Name -> Maybe a
find1 env x = case (filter (\(a,b) -> a==x) env) of
  [] -> Nothing
  ((a,b):xs) -> Just b 
find :: Environments a -> Name -> Maybe a
find envs x = let 
  y = find1 (current_env envs) x 
  ys = map (\m -> find1 (module_env m) x) . Map.elems . loaded_modules $ envs in
  foldr (<|>) (Nothing) (y:ys)
findM :: Monad m => Name -> EnvT v m (Maybe v)
findM n = EnvT (\e -> return (find e n, e))
~~~


~~~ {.haskell}
define1 :: Environment a -> Name -> a -> Environment a
define1 env x v = (x,v):env
define :: Environments a -> Name -> a -> Environments a
define envs x v = envs{current_env = define1 (current_env envs) x v}
defineM :: Monad m => Name -> v -> EnvT v m ()
defineM x v = EnvT (\e -> return((), define e x v ))

defs1 :: Environment a -> [(Name, a)] -> Environment a
defs1 = flip (++) 
defs :: Environments a -> [(Name, a)] -> Environments a
defs envs ds = envs{current_env = defs1 (current_env envs) ds}
defsM :: Monad m => [(Name, v)] -> EnvT v m ()
defsM xs = EnvT (\e -> return ((), defs e xs))
~~~

We have to unwrap a few layers to add definitions to the current environment, but it's all rather standard.

~~~ {.haskell}
listM :: Monad m => EnvT v m [String]
listM = EnvT (\e ->return (Map.keys . loaded_modules $ e  ,e) )

inM :: Monad m => Environments v -> EnvT v m ()
inM env = EnvT (\e -> return ((), env))
~~~

Finally we have these two functions that return a list of the loaded modules, and replace the current environment with a provided one.

We will also redefine the type synonym `Env` to:

~~~ {.haskell}
type Env = Environments Value
~~~

So that our functions `eval` and `elab` remain monad-free, with the same type signatures.
## Wrapping up

Now let's put this all together into a simple REPL. The structure is to:
* Read instructions from the console
* Parse them into commands
* Execute them
* Repeat

Suppose we had a function `rep :: EnvT Action IO ()` that, in an environment, does the first three steps above. We could turn this into a straight `IO ()` by calling `runEnvT rep prim`. But we want to keep looping, and importantly, _keep passing the environment around_. So we'll have to make the loop inside the `EnvT`, like so:

~~~ {.haskell}
main = runEnvT (forever rep) prim
~~~

Now we'll try to define `rep` appropriately.

We'll start by thinking about what kind of commands we should accept. We'll want to be able to add definitions to the environment, evaluate expresions and optionally reduce/decode them, load and reload modules, and quit. So we can define a type for commands as:

~~~ {.haskell}
data Process = Bnf | Hnf | ToInt | ToBool | ToChar | ToString | None 
data Command = Define Defn 
             | Evaluate Expr Process 
             | Reload 
             | Load String
             | Quit
~~~

It is not hard to define a parser for these, so let's define a function `parseCommand :: String -> Command` to parse them.

How do we execute a command?
 
~~~ {.haskell}
execute :: Command -> EnvT Action IO ()
execute = \case 
  Define d -> elabM d
~~~

Adding a definition to the environment is pretty simple now. To evaluate an expression, and then process it afterwards is straightforward too:

~~~ {.haskell}
process :: Process -> Term -> String
process = \case
  Bnf -> show . bnf
  Hnf -> show . hnf
  ToInt -> show . (unchurch :: Term -> Int)
  ToBool -> show . (unchurch :: Term -> Bool)
  ToChar -> show . (unchurch :: Term -> Char)
  ToString -> show . (unchurch :: Term -> String)
  None -> show

execute :: Command -> EnvT Action IO ()
execute = \case 
  ...
  Evaluate e p -> eval e >>= (lift . putStrLn . (process p) church . partial)
~~~

Quitting and reloading are simple as well.

~~~ {.haskell}
execute :: Command -> EnvT Action IO ()
execute = \case 
  ...
  Quit -> lift $ putStrLn "Quiting LamER" >> exitSuccess
  Reload -> envM >>= lift . reloadM >>= inM
~~~

To load a module from a file path we will find it helpful to define a function `loadFile :: MonadIO , => String -> EnvT Action m ()`, that, in an environment, adds a new module from the given file path. From there it is straightforward to define the appropriate command:


~~~ {.haskell}
execute :: Command -> EnvT Action IO ()
execute = \case 
  ...
  Load path -> loadFile path
~~~

Now we'd like to display a prompt on each line. We have to make sure to flush the buffer so that the prompt actually appears first, but it's not too difficult.  

~~~ {.haskell}
prompt' :: String -> IO String
prompt' text = do
    putStr text
    hFlush stdout
    getLine 
~~~

Remember how we defined a function `listM :: Monad m => Envt a m [String]` that returned a list of module names from the current environment? We can now use that to define a GHCi-like prompt, that prints the names of the currently loaded modules.

~~~ {.haskell}
prompt ::  EnvT Action IO String 
prompt = do
  ms <- listM 
  lift $ prompt' (intercalate " " ms ++ "> ")
~~~

Now we can simply define `rep` as:

~~~ {.haskell}
rep :: EnvT Action IO ()
rep = prompt >>= execute . parseCommand
~~~

And we are done!

## References
The interpreter was inspired by Mike Spivey's Principles of Programming Languages lecture notes.

Here are some resources I found useful:

*  http://matt.might.net/articles/compiling-up-to-lambda-calculus/
*  http://okmij.org/ftp/Computation/lambda-calc.html#neg
*  http://jwodder.freeshell.org/lambda.html
*  https://blog.jez.io/variables-and-binding/
*  https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

