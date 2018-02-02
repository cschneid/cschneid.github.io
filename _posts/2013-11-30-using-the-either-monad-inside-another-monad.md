---
layout: post
title: "Using the Either monad inside another monad"
date: 2013-11-30 14:29:07 -0700
author: "Chris Schneider"
comments: true
categories:
  - Haskell
---

After yesterday's post about the
[Either Monad]({% post_url 2013-11-28-playing-with-the-either-monad %})
I wanted to see if it was easy to embed that bit of `doWork` stuff right
into the main function.

This was mostly about learning the syntax, I would suggest keeping stuff
separate as much as possible in real code.

The biggest gotcha I found was that the indentation of the `x <-
eitherFailure...` bit needed to be deeper than the `r` in the `result` token.
This ended up being more than my normal 2 space indent.

```haskell
import Control.Error

-- A type for my example functions to pass or fail on.
data Flag = Pass | Error

main :: IO ()
main = do
  putStrLn "Starting to do work:"

  -- The inner monad here is Either. But note that we have
  -- no IO ability inside of it.
  let result = do
      x <- eitherFailure Pass "Initial Thing"
      y <- eitherFailure Error ("Second Thing " ++ x)
      note ("Failed the Maybe: " ++ y) $ maybeFailure Pass y

  case result of
    Left  val -> putStrLn $ "Work Result: Failed\n " ++ val
    Right val -> putStrLn $ "Work Result: Passed\n " ++ val
  putStrLn "Ok, finished. Have a nice day"

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

You can see it's the same code, except the `result` in main is calculated
directly there, rather than calling another function.

Note that this isn't the transformer library, so you can't be clever and do
stuff like `lift` and friends to do IO in that Either workflow.

