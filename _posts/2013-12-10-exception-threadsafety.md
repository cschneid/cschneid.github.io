---
layout: post
title: "$! threadsafety"
date: 2013-12-10 21:15:06 -0700
author: "Chris Schneider"
comments: true
categories: 
  - Ruby
---

I was investigating the `$!` variable in Ruby, specifically if it is truely a
global variable the way the leading `$` implies.

I made a quick test case, where I attempt to raise errors, then print out the
message.  This should detect a race condition after a few attempts.

```ruby
t1 = Thread.new do
  100000.times do
    begin
      raise "T1 Error"
    rescue
      puts "T1 - #{$!}"
    end
  end
end

t2 = Thread.new do
  100000.times do |i|
    begin
      raise "T2 Error"
    rescue
      puts "T2 - #{$!}"
    end
  end
end

t1.join
t2.join
```

But the worse that happens is the newline getting printed out of order:

```
$ ruby globals.rb | grep "T1.*T2"
T1 - T1 ErrorT2 - T2 Error
T1 - T1 ErrorT2 - T2 Error
T1 - T1 ErrorT2 - T2 Error
```

### Result

So the result of all this is that no, the `$!` is not a real global, but instead
thread-local (at least).

Hopefully I can go dig into the code to figure out what scope it really is.
