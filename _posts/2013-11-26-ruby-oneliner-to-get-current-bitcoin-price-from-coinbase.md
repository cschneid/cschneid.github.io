---
layout: post
title: "Ruby oneliner to get current bitcoin price from coinbase"
date: 2013-11-26 17:36:49 -0700
author: "Chris Schneider"
comments: true
categories:
  - Ruby
  - Bitcoin
---

This was a fun little hack.  I wanted to be able to grab the current price off of the exchange I use (coinbase)

```ruby
require 'json'
require 'open-uri'

JSON.parse(
      open("https://www.coinbase.com").
      each_line.grep(/window\.exchange/).
      first.sub(/.*?=/, '').sub(/;\n$/, '')
      )["btc_to_usd"]
# => "843.0"
```

Granted this will explode hard if their page ever changes. But who
cares, took like 5 minutes to write.
