---
layout: post
title: "EitherT inside of IO"
author: "Chris Schneider"
date: 2013-11-30 14:46:12 -0700
comments: true
categories:
  - Haskell
---

Keeping with our series of posts about using the Either Monad in various ways:

1. [Playing with the Either Monad]({% post_url 2013-11-28-playing-with-the-either-monad %})
2. [Using the Either Monad Inside Another Monad]({% post_url 2013-11-30-using-the-either-monad-inside-another-monad %})

This time, I expand from Either to EitherT, which allows us to interleave an
outer monad with an inner one.

When we call `runEitherT` with a `do` block, we are making a new context, where
we make an EitherT type, wrapped around an inner IO type.  I am not sure what
the exact type there is, I'll have to look into that later.

I import `Control.Monad.Trans` to get access to the `lift` function. That lets
us go down a layer into that EitherT wrapped around the IO to run IO commands.

You can see how in the workflow of the EitherT section, it asks for some text,
does some "work" that may fail, and then asks for the next bit of text to work
on.

The coolest part is that if the first bit fails, it bails out of the whole
workflow with the correct `Left` value, not even asking for the second bit
of input.

The only other gotcha is that `EitherT` isn't quite the same as a normal
`Either` type, so you have to use functions to convert between them.
`hoistEither` and `hoistMaybe` take a normal version of `Either/Maybe` and turn it
into `EitherT/MaybeT`.

Similarly, we had to use `noteT` instead of `note`.  Same behavior, but it just
works on the transformed versions of the types.


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
      x <- hoistEither $ eitherFailure Error initialText

      lift $ putStrLn "Give me the second input please:"
      secondText <- lift getLine
      y <- hoistEither $ eitherFailure Pass (secondText ++ x)

      noteT ("Failed the Maybe: " ++ y) $ hoistMaybe $ maybeFailure Pass y

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
