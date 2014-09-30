Repl
----

Initial work on a lightweight readline library for Haskell based on the ``linenoise`` library. Designed from
the ground up to work more smoothly with modern monad transformers and exceptions libraries.

[![Build Status](https://travis-ci.org/sdiehl/haskell-linenoise.svg?branch=master)](https://travis-ci.org/sdiehl/haskell-linenoise)

```haskell
import System.Console.Repl

type Repl = ReplT IO

completer :: String -> [String]
completer ('h':_) = ["hello", "hello there"]
completer _ = []

repl :: Repl ()
repl = replM ">>> " outputStrLn completer

main :: IO ()
main = runRepl repl undefined
```

Can compose with the regular State monad, for instance to do stateful tab completion. Something which is
painful with Haskeline.

```haskell
import System.Console.Repl
import Control.Monad.State.Strict
import Data.List (isPrefixOf)

type Repl = ReplT (StateT [String] IO)

completer :: String -> Repl [String]
completer line = do
  comps <- get
  return $ filter (isPrefixOf line) comps

action :: String -> Repl ()
action x = do
  modify $ (x:)
  liftIO $ putStrLn x

repl :: Repl ()
repl = replM ">>> " action completer

main :: IO ()
main = evalStateT (runRepl repl undefined) []
```

License
-------

Includes the source code for linenoise. Released under the BSD license.

Copyright (c) 2010-2013, Salvatore Sanfilippo <antirez at gmail dot com>

Copyright (c) 2010-2013, Pieter Noordhuis <pcnoordhuis at gmail dot com>

Copyright (c) 2014, Stephen Diehl

