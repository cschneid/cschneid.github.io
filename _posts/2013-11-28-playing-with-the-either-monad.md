---
layout: post
title: "Playing with the Either monad in Haskell"
author: "Chris Schneider"
date: 2013-11-28 22:50:09 -0700
comments: true
categories:
  - Haskell
---

After playing with the bitcoin price fetcher, I was disappointed at how... hard
it was to deal with the multiple layers of potential errors.  I started looking
into the `errors` package on Hackage for a way out.  It is a one-stop-shop for
all the standard error handling mechanisms in Haskell. It reexports the
standard Either and Maybe types, and also adds many helper functions to move
between Either and Maybe types, in addition to helping out with the various
transformer versions of both (MaybeT and EitherT)

I will play with MaybeT and EitherT later, for now I'm happy to have figured out
the Either monad, and want to share the annotated example I've cobbled together.

Grab the code into a file, `cabal install errors`, and start toying with the
various places I use the `Pass` and `Error` types in the `doWork` function.
You'll see how nicely Haskell handles a long string of things, where any one of
them could fail out.

I'll have to go rewrite the bitcoin scraper with my newfound knowledge...

```haskell
import Control.Error

-- A type for my example functions to pass or fail on.
data Flag = Pass | Error

main :: IO ()
main = do
  putStrLn "Starting to do work:"
  let result = doWork
  case result of
    Left  val -> putStrLn $ "Work Result: Failed\n " ++ val
    Right val -> putStrLn $ "Work Result: Passed\n " ++ val
  putStrLn "Ok, finished. Have a nice day"

-- This is a driver function, simulating an error prone path
-- through the app.  Each step could possibly error out, and
-- when any of them do, we want to just bail out.
--
-- Remember the definition of the Either monad is:
-- instance Monad (Either e) where
--   return = Right
--   Right m >>= k = k m
--   Left e  >>= _ = Left e
--
-- So a Left value short circuits the rest of the Monad, and a Right value
-- passes the value off to the next step.
doWork :: Either String String
doWork = do -- use do notation syntax sugar for the Either monad

    -- First, do something that may or may not work. We get back a type of
    -- Either String String (since that's the type of the example
    -- eitherFailure function here)
    x <- eitherFailure Pass "Initial Thing"

    -- Based on what we get in x, just go ahead and attempt it.
    -- Note that the function eitherFailure takes a simple
    -- String as its argument.  So we didn't have to unwrap the
    -- first Either value.
    y <- eitherFailure Error ("Second Thing " ++ x)

    -- We can't just wire a Maybe value in the middle here,
    -- since it doesn't typecheck. (Maybe isn't an Either),
    -- even though they play similarly. If we just tried, we'd get:

    -- z <- maybeFailure Error
    -- Couldn't match type `Maybe' with `Either String'

    -- But instead, we can use Control.Error.Util.note to convert
    -- an "empty" Nothing value into a Left value with a descriptive
    -- error.  So now we'd get a proper Either value we can chain
    -- into this overall monad.
    note ("Failed the Maybe: " ++ y) $ maybeFailure Pass y

    -- Since the last line of this `do` block is the type we plan on
    -- returning, there's no `return` call needed.


-- Simple function that we can use to force it to error out with a Left, or
-- pass with a Right value. It just includes some helper text as its content,
-- showing what happened.
eitherFailure :: Flag -> String -> Either String String
eitherFailure Pass  val = Right $ "-> Passed " ++ val
eitherFailure Error val = Left  $ "-> Failed " ++ val

-- Simlar to eitherFailure, but return a (Just String) or a Nothing based on
-- if we told it to fail.
maybeFailure :: Flag -> String -> Maybe String
maybeFailure Pass  val = Just $ "-> Passed maybe " ++ val
maybeFailure Error _   = Nothing
```

My favorite part of diving into the error library is that my worry from yesterday,
`except then I’d have to switch the Maybe result out of Lens-Aeson into a “fuller” Either type.` is just the `note` function I demoed above.

