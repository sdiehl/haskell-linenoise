module Main where

import System.Console.Repl

completer :: String -> [String]
completer ('h':_) = ["hello", "hello there"]
completer _ = []

main :: IO ()
main = replIO ">>> " print completer
