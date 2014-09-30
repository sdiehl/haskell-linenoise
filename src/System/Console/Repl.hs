{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module System.Console.Repl (
  ReplT,
  runRepl,

  replIO,
  replM,

  MonadRepl(..),
) where

import qualified System.Console.FFI as FFI
import Data.String (IsString(..))

import Control.Monad
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Catch

data Settings

newtype ReplT m a =
  ReplT { unReplT :: ReaderT Settings m a }
  deriving (Functor, Monad, Applicative, MonadIO, MonadReader Settings, MonadFix, MonadTrans, MonadPlus, MonadThrow, MonadCatch)

runRepl :: ReplT m a -> Settings -> m a
runRepl m s = runReaderT (unReplT m) s

class MonadCatch m => MonadRepl m where
  getInputLine  :: IsString s => String -> m (Maybe s)
  outputStr     :: String -> m ()
  outputStrLn   :: String -> m ()
  addHistory    :: String -> m ()
  setCompletion :: (String -> m [String]) -> m ()

instance MonadRepl IO where
  getInputLine  = FFI.getInputLine
  outputStr     = putStr
  outputStrLn   = putStrLn
  addHistory    = FFI.addHistory
  setCompletion = FFI.setCompletion

instance MonadRepl m => MonadRepl (ReplT m) where
  getInputLine  = lift . getInputLine
  outputStr     = lift . outputStr
  outputStrLn   = lift . outputStrLn
  addHistory    = lift . addHistory

  setCompletion :: (String -> ReplT m [String]) -> ReplT m ()
  setCompletion f = do
    settings <- ask
    lift $ setCompletion (flip runRepl settings . f )

instance MonadState s m => MonadState s (ReplT m) where
  get = lift get
  put = lift . put

instance (MonadRepl m) => MonadRepl (StateT s m) where
  getInputLine  = lift . getInputLine
  outputStr     = lift . outputStr
  outputStrLn   = lift . outputStrLn
  addHistory    = lift . addHistory
  setCompletion f = do
    st <- get
    lift $ setCompletion (flip evalStateT st. f )

-- | Simple REPL embedded in IO.
replIO
  :: String                -- ^ Prompt
  -> (String -> IO a)      -- ^ Action
  -> (String -> IO [String])  -- ^ Completion
  -> IO ()
replIO prompt action comp = do
  setCompletion comp
  res <- getInputLine prompt
  case res of
    Nothing   -> return ()
    Just line -> do
      _ <- action line
      FFI.addHistory line
      replIO prompt action comp

replM
  :: (MonadRepl m)
  => String                  -- ^ Prompt
  -> (String -> m a)         -- ^ Action
  -> (String -> m [String])  -- ^ Completion
  -> m ()
replM prompt action comp = do
  setCompletion comp
  res <- getInputLine prompt
  case res of
    Nothing   -> return ()
    Just line -> do
      _ <- action line
      addHistory line
      replM prompt action comp
