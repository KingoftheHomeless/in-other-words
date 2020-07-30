module Control.Effect.Reader
  ( -- * Effect
    Reader(..)

    -- * Actions
  , ask
  , local

    -- * Interpreters
  , ReaderC
  , runReader

    -- * Threading constraints
  , ReaderThreads

    -- * Threading utilities
  , threadReaderViaRegional
  , threadReaderViaClass
  ) where

import Control.Effect
import Control.Effect.Type.Reader

import Control.Effect.Internal.Reader

import Control.Monad.Trans.Reader (ReaderT(..))


ask :: Eff (Reader i) m => m i
ask = send Ask
{-# INLINE ask #-}

local :: Eff (Reader i) m => (i -> i) -> m a -> m a
local f m = send (Local f m)
{-# INLINE local #-}


runReader :: forall i m a p
           . ( Carrier m
             , Threaders '[ReaderThreads] m p
             )
          => i
          -> ReaderC i m a
          -> m a
runReader i m = runReaderT (unReaderC m) i
{-# INLINE runReader #-}
