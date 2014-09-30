Repl
----

Initial work on a lightweight readline library for Haskell based on the ``linenoise`` library. Designed from
the ground up to work more smoothly with modern monad transformers and exceptions libraries.

[![Build Status](https://travis-ci.org/sdiehl/haskell-linenoise.svg?branch=master)](https://travis-ci.org/sdiehl/haskell-linenoise)

```haskell
module Main where

import System.Console.Repl

completer :: String -> [String]
completer ('h':_) = ["hello", "hello there"]
completer _ = []

main :: IO ()
main = replIO ">>> " print completer
```

License
-------

Includes the source code for linenoise. Released under the BSD license.

Copyright (c) 2010-2013, Salvatore Sanfilippo <antirez at gmail dot com>

Copyright (c) 2010-2013, Pieter Noordhuis <pcnoordhuis at gmail dot com>

Copyright (c) 2014, Stephen Diehl

