hs2lazy -- Haskell to Lazy K compiler
=====================================

## What is this?

This is a translator from a subset of Haskell to [Lazy K](http://homepages.cwi.nl/~tromp/cl/lazy-k.html).

## How to compile

You can build hs2lazy with GHC by following command:

    ghc -o hs2lazy --make Main.hs

## How to use

    hs2lazy [source files]

If multiple source files are given, they are concatenated in the order specified. Output Lazy K code is written to standard output.
Prelude module is not automatically loaded. So, you may specify examples/Prelude.hs first, like following:

    hs2lazy examples/Prelude.hs foo.hs >foo.lazy

## How to write source code

In Lazy K, input and output streams are represented as lists of numbers (256 represents EOF). Hs2lazy programs handle I/O in similar way.

In Haskell, type of `main` function is `IO ()`. But in hs2lazy, that is:

    main :: Stream -> Stream

The Stream type is defined in Prelude.hs as

    data Stream = Cons Char Stream

The input is a infinite stream of characters. For example:

    Cons 'f' $ Cons 'o' $ Cons 'o' $ Cons eof $ Cons eof ...

`eof` is defined as `(chr 256)`.

Streams can be converted to/from strings by `fromStream` and `toStream` defined in Prelude.hs. For example, following program reverses the input character-by-character.

    main = toStream . reverse . fromStream

## License

Type.hs is based on [Typing Haskell in Haskell](http://web.cecs.pdx.edu/~mpj/thih/). See Lisence.thih for the license of it.
