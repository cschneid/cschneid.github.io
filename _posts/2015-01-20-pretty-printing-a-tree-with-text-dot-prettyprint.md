---
layout: post
title: "Pretty Printing a Tree with Text.PrettyPrint"
date: 2015-01-20 03:40:25 +0000
author: "Chris Schneider"
comments: true
categories:
  - Haskell
---

I looked into Haskell's
[pretty package][pretty]
(and the [prettyclass package][prettyclass])
to print out a simple tree structure I defined.
I wanted a nicely nested output.

[pretty]: http://hackage.haskell.org/package/pretty
[prettyclass]: http://hackage.haskell.org/package/prettyclass

## Why not Show?

The show typeclass isn't what we want for human readable output.
`read . show` should be the same as `id`.
This means that we can't ever throw away extraneous data
, and we have to be 100% sure to preserve any structure.

These rules get in the way of human readable output.  For example, printing a
`User` record might omit the full address if it gets too long, not include the
bio, and generally lay out the data in a simplified manner.

## The Pretty typeclass

The [prettyclass package][prettyclass] defines a general typeclass for all types
that can be printed out for human consumption.  It comes with some standard types
defined (like Int and similar)

Before we talk about pretty printing, we need to look at our type first.

```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)
```

A simple binary tree, where interior nodes don't store anything, and leaves
hold all the values.

We need to define an instance of our `Pretty` for our type.
As the docs say, `Minimal complete definition is either pPrintPrec or pPrint.`

`pPrintPrec` takes a PrettyLevel, which defines the level of detailed requested.
Since we want to show all the data in the tree, this is unnecessary for us, we can
implement the simpler `pPrint` function.

### The typeclass instance definition

Since our type is parameterized on `a`, we need to limit it somehow.
In this case, we're going to say that if our `a` is a member of `Pretty`
, then we can pretty print a whole tree of `a`

```haskell
import Text.PrettyPrint.HughesPJClass

instance (Pretty a) => Pretty (Tree a) where
  pPrint tree = ...
```

## The Text.PrettyPrint

The [pretty package][pretty] implements a TON of helpers to actually lay out the data.

In this case, our goal is to have the tree look like:

```
Node:
  Leaf: 1
  Node:
    Leaf: 2
    Leaf: 3
```

Each level gets labeled by type, and nesting levels get indented by 2.

### The Leaf Case

So first, lets do the `Leaf` case, where we print the literal `Leaf:` and then
ask the `a` type to be pretty printed itself.

```haskell
  pPrint (Leaf a)   = text "Leaf: " <> pPrint a
```

Pretty implements `<>` the same as a monoid, combining two `Doc`s into one.
Pretty also implements a `<+>` which is like `<>` except it will insert a
space between two non-empty documents.

### The Node case

The `Node` case is much more interesting.

```haskell
  pPrint (Node l r) = vcat [ text "Node:"
                           , nest 2 (pPrint l)
                           , nest 2 (pPrint r)]
```

First we destructure the argument
, then we build a 3 element list
, each containing a `Doc` type.
The first one is the literal `Node:` text
, then the next two are indented by 2 spaces.
Then we recursively call `pPrint` on the left and right sub-trees.

`nest` takes an indent level, and a document and returns a new document with the same content, except indented.

The `vcat` function takes a list of documents, and lays them out vertically.

Fairly straight forward.

## And...

I was impressed by how easy this library was to use.
Although I was rather confused by how hard it was to use the typeclass.
The `pretty` package specifically has a module that defines the Pretty class,
but GHC couldn't find it.

I could see the use of this library in a large project, full of custom types.

A logging function could easily ask to `prettyShow` each individual item it logs.

Full code is available at: https://github.com/cschneid/cschneid-pretty/

