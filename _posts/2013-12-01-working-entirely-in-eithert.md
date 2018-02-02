---
layout: post
title: "Working entirely in EitherT"
date: 2013-12-01 15:21:42 -0700
author: "Chris Schneider"
comments: true
categories: 
  - Haskell
---

This is the last post in my series of stuff on the Either monad.

1. [Playing with the Either Monad]({% post_url 2013-11-28-playing-with-the-either-monad %})
2. [Using the Either Monad Inside Another Monad]({% post_url 2013-11-30-using-the-either-monad-inside-another-monad %})
3. [EitherT Inside of IO]({% post_url 2013-11-30-eithert-inside-of-io %})

It's a smallish change to the code, where I get rid of a lot of the annoying
casting code to go `Either -> EitherT`, and instead just write everything in
EitherT.

The biggest change was the type signature of my failure code.  See how I add
the Monad constraint, and update the return value to be EitherT wrapped around
whatever monad you have.

What's cool about this is that it'll work right for both IO, and every other
monad we want to embed this `eitherFailure` code into.  Which means that as a
hypothetical application's monad transformer stack builds up, it would be easy
to just plug this code in and go.

```haskell
eitherFailure :: Monad m => Flag -> String -> EitherT String m String
eitherFailure Pass  val = right $ "-> Passed " ++ val
eitherFailure Error val = left  $ "-> Failed " ++ val
```

One other gotcha is that I had to change `Right` to `right`, which is a
function that returns a hoisted version of the Either value. No biggie, just
wouldn't typecheck till I did.

If you read this code, you'll see that the transformation from `Maybe` to
`MaybeT` is very similar, right down to using `just` and `nothing` as
functions, rather than the `Just` and `Nothing` data constructors.

```haskell
import Control.Error
import Control.Monad.Trans

-- A type for my example functions to pass or fail on.
data Flag = Pass | Error

main :: IO ()
main = do
  putStrLn "Starting to do work:"

  result <- runEitherT $ do
      lift $ putStrLn "Give me the first input please:"
      initialText <- lift getLine
      x <- eitherFailure Error initialText

      lift $ putStrLn "Give me the second input please:"
      secondText <- lift getLine
      y <- eitherFailure Pass (secondText ++ x)

      noteT ("Failed the Maybe: " ++ y) $ maybeFailure Pass y

  case result of
    Left  val -> putStrLn $ "Work Result: Failed\n " ++ val
    Right val -> putStrLn $ "Work Result: Passed\n " ++ val

  putStrLn "Ok, finished. Have a nice day"

eitherFailure :: Monad m => Flag -> String -> EitherT String m String
eitherFailure Pass  val = right $ "-> Passed " ++ val
eitherFailure Error val = left  $ "-> Failed " ++ val

maybeFailure :: Monad m => Flag -> String -> MaybeT m String
maybeFailure Pass  val = just $ "-> Passed maybe " ++ val
maybeFailure Error _   = nothing
```
