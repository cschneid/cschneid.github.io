---
layout: post
title: "Spock basics"
date: 2014-12-23 18:46:19 -0600
author: "Chris Schneider"
comments: true
categories:
  - Haskell
  - Spock
---

## Spock Intro - Minimal Web Framework

Spock is a slick little web framework in Haskell that builds off of Scotty's
legacy - although it apparently doesn't share code any more.

It supports middleware, and a nice routing api.

For instance, to setup logging and static file serving, you just wire up two Wai middlewares.

```haskell
appMiddleware :: SpockT IO ()
appMiddleware = do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "static")
```

Then the routes get built up (referencing the actual handler functions defined elsewhere)

```haskell
appRoutes :: SpockT IO ()
appRoutes = do
  get "/"          $ Static.root
  get "/users"     $ Users.index
  get "/users/:id" $ Users.show
```

Then connect the pieces up and run on port 3000.

```haskell
main = runSpock 3000 (appMiddleware >> appRoutes)
```


## Handlers

I found myself repeating the specific ActionT type (the Spock route handler),
so I type aliased it to be specific to my app (wrapping IO).  This has the
benefit of letting me change it in only one spot if/when I decide that I need a
different monad transformer stack.

```haskell
type HandlerM = ActionT IO
```

Then the actual handlers just have `HandlerM` and the return value (mostly just unit)

```haskell
root :: HandlerM ()
root = text "Hello!"
```

There are a TON of helper functions to use in the context of a handler -
`redirect`, `json`, `html`, `setHeader`, etc, etc.

## More and More

Spock claims to support sessions, database connection pooling and more, but I
haven't had a chance to dive into that integration.


