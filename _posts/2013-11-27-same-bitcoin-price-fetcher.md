---
layout: post
title: "Same Bitcoin price fetcher, but in Haskell"
author: "Chris Schneider"
date: 2013-11-27 20:45:27 -0700
comments: true
categories:
  - Bitcoin
  - Haskell
---

This one isn't quite as one-line (at all!) as my Ruby implementation. But it
does handle errors nicer.  I both love and hate that Haskell doesn't let you just
skim past errors. You've gotta unwrap your potential errors and handle both cases.

But even with that, the code ended up short, and I think fairly easy to follow.

I bet there are ways to shorten it up further too. I sort of want to use the
Either monad to make the error handling cleaner, except then I'd have to switch
the Maybe result out of Lens-Aeson into a "fuller" Either type.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Control.Lens.Aeson
import Data.Aeson (Value(String))
import System.Exit
import qualified Data.Text as T
import qualified Network.Curl.Download as Curl

main :: IO ()
main = do
  -- Fetch the whole page
  result <- Curl.openURIString "https://coinbase.com"

  case result of
    Left e -> do
      putStrLn "Failed fetching page!"
      putStrLn e
      exitFailure
    Right s ->
      -- Use lens-aeson to grab out the key we want. It comes out in several
      -- layers of wrappers, so undo them to get a float out, suitable to print.
      case (T.unpack . strip . findLine) s ^? key "btc_to_usd" of
        Just (String val) -> print (read $ T.unpack val :: Float)
        Nothing           -> putStrLn $ "Misparse of JSON: " ++ s
      where
        -- Grab the first line that has this text in it.
        findLine html = head $ filter (T.isInfixOf "exchangeRates") (T.lines $ T.pack html)

        -- Strip the stuff before & after the curlies.
        strip :: T.Text -> T.Text
        strip = T.dropWhile (/= '{') . T.dropWhileEnd ( /= '}')
```
