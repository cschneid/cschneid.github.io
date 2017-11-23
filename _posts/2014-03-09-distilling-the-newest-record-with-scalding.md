---
layout: post
title: "Distilling the Newest Record with Scalding"
date: 2014-03-09 20:43:56 -0600
author: "Chris Schneider"
comments: true
categories:
  - Scala
  - Scalding
  - Hadoop
---


## The Problem Statement

### Background

At Comverge, we are building a new project based on the lambda architecture. One of the core aspects of the lambda architecture revolves around an immutable, always growing store of data.

We store this data as a series of **facts**.  Each fact is a single statement about the state of the world at a given time.  For example, here is a set of facts generated when a new user signs up.

```
{ "subject" => "user:1", "property" => "username", "value" => "cschneid", "asserted_at" => "2014-03-01T06:00:00Z" }
{ "subject" => "user:1", "property" => "realname", "value" => "Chris", "asserted_at" => "2014-03-01T06:00:00Z" }
{ "subject" => "user:1", "property" => "password", "value" => "b4e7a69126ef83206b8db39fb78f2bdf", "asserted_at" => "2014-03-01T06:00:00Z" }
```

It often takes a bunch of facts working in concert to build a consistent view of the world.

The real beauty of this approach is when new records come in, we can rewind time and still see what we knew at what point.

For example, if this user changes their username, we don't change the old username record, but just record the new one, and let the timestamps tell us the current state of the user.

```
{ "subject" => "user:1", "property" => "username", "value" => "ChrisTheWizard", "asserted_at" => "2014-03-05T06:00:00Z" }
```

### Actual Problem Statement

I want a map-reduce job to generate the newest state of everything in our system.  So in the background example, I would want a single record that contained:

```
{ "subject" => "user:1", "username" => "ChrisTheWizard", "realname" => "Chris", "password" => "b4e7a69126ef83206b8db39fb78f2bdf"}
```

We've thrown away the older username fact, and rearranged the data.

## Scalding

So I played around with hadoop in various forms, and ended up with Scalding as an environment to write map-reduce jobs in.

I'll walk you through the code I ended up with, and where I'm still working to finish up.

### A few imports

We of course need scalding's libraries, and our specific use cases need json parsing, and a mutable map for collecting up the final view of the data.

```scala
import com.twitter.scalding._
import scala.util.parsing.json._
import scala.collection.mutable
```

### Setup Input & Output

Next up is the top matter of the code, where we setup the input and output files and types.

The data we have is specifically stored in hadoop sequence files, where the key is the timestamp of when the data was written, and an encoded JSON structure of the fact.  We don't care about the sequence file's timestamp, so we just throw that away.

Similarly, the final output should be stored as JSON. The `JsonLine` class makes that really easy, but is fairly inflexible.  It may be that I'll need to write my own output class at some point.

```scala
class ExtractJSON(args : Args) extends Job(args) {
  val input = WritableSequenceFile("/advanced-apps/master-dataset/Facts.1394232008699", ('sequenceFileKey, 'factJSON))
  val output = JsonLine("/fact_data_output")
```

### JSON Parse

**Everything from here on out is a single pipeline.**

We take the input file, and start reading from it. The first thing we need to do is deserialize the json (a single field of text as far as scalding is concerned) into the set of fields that we actually care about.  We use the built-in JSON parser in scala to do this work for us.

Notice that a new field that hasn't been mentioned shows up too, indicating if the JSON parsed or not.  We will use that in the very next code snippet.

```scala
  input
    .read

    // Parse the json of each fact. Extract out the 4 expected values.
    .map(('factJSON) -> ('parse_status, 'asserted_at, 'subject, 'property, 'value)) {
      line: String => {
        JSON.parseFull(line) match {
          case Some(data: Map[String, Any]) => ("success",
                                                data("asserted_at"),
                                                data("subject"),
                                                data("property"),
                                                data("value")
                                              )
          case None => ("failed", "", "", "", "")
        }
      }
    }
```

### Error check the JSON parse

The JSON parsing code always works, and lets a value through. But some of those values may have "failed" in the `parse_status` field.  If they do, just stop parsing that whole tuple (ie, throw away that whole line of input).

After that's done, we have no more use for that field, so throw it away to keep the dataset small as we continue to move through the input.

```scala
    .filter('parse_status) { status: String => status != "failed" }
    .discard('parse_status)
```

### Find the newest

Now our goal is to find only the newest version of a fact for each pair of subject / property.  Continuing the background example, this would be the newest username that we know.

We do a `groupBy`, then sort the results based on the timestamp.  Then take the first one result of that sort. (the newest item). Scalding provides an all-in-one way to do that with `sortWithTake`, so just use that.

The `_` variable is a bit surprising to me. Mostly my ignorance of scala, it must be an implicit argument being passed into this anonymous function.  In any case, it represents the whole grouping of {subject / property}

The comparison function is tricky since my `value` field is an `Any`, which can't be automatically sorted by the language.  So instead I give it an explicit rule to sort by (just use the timestamp, and ignore the value field).  But I do need the value to be included in that `sortWithTake` so it comes out the other side of the funnel with the value I was looking for.

Once done, flatten out the temporary `items` field that we stored that pair of `asserted_at, value` into, and get rid of it.

```scala
    // Find the newest asserted at for each combo of subject & property
    .groupBy('subject, 'property) {
      _.sortWithTake[(String, Any)](('asserted_at, 'value) -> 'items , 1) {
        case ((asL, _), (asR, _)) => asL > asR
      }
    }
    .flatten[(String, Any)](('items) -> ('asserted_at, 'value))
    .discard('items)
```

### Combine the many facts

At this point, we now have all the newest facts, having removed any outdated ones during the sort.

So the job now is to combine many rows of facts about a subject into a single row that represents all of what we know about that subject.

Once again, we `groupBy`, but this time just on subject.

Then we use `foldLeft` to loop over each property/value pair that we get and save it into a mutable `Map`.  I had a bit of fun here trying to figure out how the syntax for adding to a Map works. See the result below for how I did it (apparently there are 2 or 3 different ways).

The tuple that comes out of this step is {subject, properties(property/value, property/value...)}

```
    .groupBy('subject) {
      _.foldLeft(('property, 'value) -> 'properties)(mutable.Map.empty[String,Any]) {
        (properties: mutable.Map[String,Any], propAndVal: (String, Any)) =>
        val (prop, value) = propAndVal
        properties += prop -> value
      }
    }
```

### Finish Up

So now we have a tuple of data we want, lets serialize it back out to disk and close the class we were working inside of.

```scala
    .write(output)
}
```

### Hopefully that helped!

It took me about 2 days to get the whole stack there working right, and `posco` on IRC was super helpful in getting me unstuck.


### Next Steps

I need to figure out the actual output format I want.  I think it includes streaming the output into Cassandra, rather than simple a JSON format on disk.  That will involve figuring out how to connect to Cassandra and do the insert. I'll try to write a follow-up post about that.
