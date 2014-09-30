{-# LANGUAGE OverloadedStrings #-}

module Main where

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
