{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Console.Monad (
  ReplT(..),
  Repl,
) where

import Control.Monad
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Catch

data Settings

newtype ReplT m a =
  ReplT { unReplT :: ReaderT Settings m a }
  deriving (Functor, Monad, Applicative, MonadIO, MonadFix, MonadTrans, MonadPlus, MonadThrow, MonadCatch)

type Repl = ReplT Identity
