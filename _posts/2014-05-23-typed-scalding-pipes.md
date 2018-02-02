---
layout: post
title: "Typed Scalding Pipes"
date: 2014-05-23 18:46:19 -0600
author: "Chris Schneider"
comments: true
categories:
  - Scala
  - Scalding
  - Hadoop
---

Quick Recap
===========

A while back I described a Hadoop job that I implemented with Scalding
([Distilling the Newest Record with Scalding]({% post_url 2014-03-09-distilling-the-newest-record-with-scalding %})).
To recap, the goal was to take a huge list of "facts", each containing a single
timestamped fact about a large piece of system data.  The goal is to get a
recombined version of a domain object at a given time.

## A Fact

```json
{ "asserted_at": "2014-05-01T04:02:56Z",
  "subject": "device:123",
  "property": "serial_number",
  "value": "V29B044" }
```

## Argonaut.

The first version of this job I wrote used a built-in JSON parser.  Turns out
that's an iffy approach, so I turned to the [Argonaut](http://argonaut.io/) library
to parse my JSON into well structured Scala structs.

This code is almost literally off the Argonaut examples. I was really impressed
at how easy this was.

```scala
import argonaut._
import Argonaut._

case class Fact(asserted_at: String, subject : String, property: String, value: Json)

object Fact {
  implicit def FactCodecJson : CodecJson[Fact] =
    casecodec4(Fact.apply, Fact.unapply)("asserted_at", "subject", "property", "value")
}
```

This allows me to take a string and call `decodeOption` on it to get a Option[Fact].

## Functions!

One of the things I really wanted to explore was splitting up the large job
into an aggregate of lots of small jobs.  The best way to do that of course is
using functions.

Here's an easy one to do the JSON parsing:

```scala
def parseJsonAsFact(pipe : TypedPipe[String]) : TypedPipe[Fact] = {
  pipe
    .map    { _.decodeOption[Fact] }
    .filter { _.nonEmpty }
    .map    { _.orNull }
}
```

This takes a `TypedPipe[String]` and for each string, transforms it into a
`TypedPipe[Fact]`, or just throws away anything that didn't parse.


## Getting the input and parsing

Actually fetching input, and working with it to make output is easy.  We
assemble our small functions with Scala's `andThen` combinator.  This makes one
large function that is named `job`, which we then run with the input.


```scala
val input_file  = args.getOrElse("input",  "/master-dataset")
val output_file = args.getOrElse("output", "/output")

// Everything is stored as a SequenceFile, where the key is the timestamp it was recorded.
val source   = WritableSequenceFile[DoubleWritable, Text](input_file, ('sequenceFileKey, 'factJSON))
val rawInput = TypedPipe.from(source)
val input    = rawInput.map { _._2.toString } // TypedPipe[String]

// This is a single column, so Tsv is misleading, no tabs will be output
val output = TypedTsv[String](output_file)

// Build up a large function that is our entire pipeline.
val job = parseJsonAsFact _ andThen
          ///// More steps here.

job(input).write(output)
```


## Finishing up the job

Here is an example of the whole pipeline I have written. It did take me a bit
to figure out how `filterByType` could be parameterized by the thing I was filtering.

```scala
// TypedPipe[String] => TypedPipe[String], which is handily our input and output types.
val job = facts.parseJsonAsFact _                        andThen   //    TypedPipe[Fact]
          (facts.filterByType _).curried("observations") andThen   // => TypedPipe[Fact] (only observation related ones)
          facts.filterNewest _                           andThen   // => TypedPipe[Fact] (only the newest of any given subject/property)
          createMeasurementDate _                        andThen   // => TypedPipe[Fact] (new records with measurement_date in the stream)
          mergeObservations _                            andThen   // => TypedPipe[Observation] combine facts into observations
          renderAsJson _                                           // => TypedPipe[String] observations spun out as json

def filterByType(filter : String, pipe : TypedPipe[Fact]) : TypedPipe[Fact] = {
  pipe.filter { _.subject.startsWith(filter) }
}
```

The filtering is easy. But there's a little trick in that the `.groupBy` call
changes a `TypedPipe` into a `Grouped`, which has two type arguments - the
"group key" and the type of the values that match that key.

Second note: the custom sorting of facts was a hurdle I had to get over, turned
out to be easy - just define the sorting, and call it in a rather unintuitive
way.

```scala
def filterNewest(pipe : TypedPipe[Fact]) : TypedPipe[Fact] = {
  pipe
    .groupBy { fact : Fact => (fact.subject, fact.property) } // => Grouped[Fact, (String, String)]
    .sortedReverseTake(1)(AssertedAtOrdering)
    .values
    .flatten
}

object AssertedAtOrdering extends Ordering[Fact] {
  def compare(a:Fact, b:Fact) = a.asserted_at compare b.asserted_at
}
```

I won't go into all the pieces of my whole pipeline, most of it isn't all that
interesting, but I do want to note that you can return more or less records from a pipe than came in. It doesn't have to be a 1:1 tranformation.

For example, I needed both the date and the full datetime in my observation
domain object.  This function does that for me by splitting the pipe in two
with filters, sidelining the uninteresting half (`not_measurement_at_pipe`),
returning multiple values from the `measurement_at_pipe`.  Finally the
sidelined pipe can be merged back into the stream.

```scala
def createMeasurementDate(pipe : TypedPipe[Fact]) : TypedPipe[Fact] = {
  val measurement_at_pipe     = pipe.filter    { fact : Fact => fact.property == "measurement_at" }
  val not_measurement_at_pipe = pipe.filterNot { fact : Fact => fact.property == "measurement_at" }
  val converted_pipe = measurement_at_pipe
    .flatMap { fact : Fact =>
      List(
            fact,
            fact.copy(property = "measurement_date",
                      value    = jString(fact.value.stringOr("0000-00-00").substring(0, "yyyy-mm-dd".length)))
          )
    }

  converted_pipe ++ not_measurement_at_pipe
}
```

## Final Thoughts

I really like the Typed api for writing jobs.  The Scala compiler informs you
of errors (which is a much faster testing cycle than waiting for Hadoop to run
a compiled jar and fail at some point.  That makes it a 10 second response
time, versus a 5 minute response time).

In addition, it's so much easier to keep track of real classes and work on
them, than trying to track sets of untyped, named fields.

So use the TypedApi, and let Scala do more of the work for you.

