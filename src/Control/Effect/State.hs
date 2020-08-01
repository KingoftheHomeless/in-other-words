{-# LANGUAGE BlockArguments #-}
module Control.Effect.State
  ( -- * Effect
    State(..)

    -- * Actions
  , state
  , state'
  , get
  , gets
  , put
  , modify
  , modify'

    -- * Interpretations
  , StateC
  , runState
  , evalState
  , execState

  , StateLazyC
  , runStateLazy
  , evalStateLazy
  , execStateLazy

  , stateToIO
  , runStateIORef

    -- * Simple variants of interpretations
  , stateToIOSimple
  , runStateIORefSimple

    -- * Threading constraints
  , StateThreads
  , StateLazyThreads
  ) where

import Data.IORef
import Data.Tuple

import Control.Effect

import Control.Effect.Internal.State

import qualified Control.Monad.Trans.State.Strict as SSt
import qualified Control.Monad.Trans.State.Lazy as LSt

state :: Eff (State s) m => (s -> (s, a)) -> m a
state f = do
  (s, a) <- f <$> get
  put s
  return a
{-# INLINE state #-}

-- | A variant of 'state' that forces the resulting state (but not the return value)
state' :: Eff (State s) m => (s -> (s, a)) -> m a
state' f = do
  (s, a) <- f <$> get
  put $! s
  return a
{-# INLINE state' #-}

get :: Eff (State s) m => m s
get = send Get
{-# INLINE get #-}

gets :: Eff (State s) m => (s -> a) -> m a
gets = (<$> get)
{-# INLINE gets #-}

put :: Eff (State s) m => s -> m ()
put = send . Put
{-# INLINE put #-}

modify :: Eff (State s) m => (s -> s) -> m ()
modify f = do
  s <- get
  put (f s)

-- | A variant of 'modify' that forces the resulting state.
modify' :: Eff (State s) m => (s -> s) -> m ()
modify' f = do
  s <- get
  put $! f s

runStateIORef :: forall s m a
               . Eff (Embed IO) m
              => IORef s
              -> InterpretReifiedC (State s) m a
              -> m a
runStateIORef ref = interpret $ \case
  Get   -> embed $ readIORef ref
  Put s -> embed $ writeIORef ref s
{-# INLINE runStateIORef #-}

stateToIO :: forall s m a
           . Eff (Embed IO) m
          => s
          -> InterpretReifiedC (State s) m a
          -> m (s, a)
stateToIO s main = do
  ref <- embed $ newIORef s
  a   <- runStateIORef ref main
  s'  <- embed $ readIORef ref
  return (s', a)
{-# INLINE stateToIO #-}

runStateIORefSimple :: forall s m a p
                     . ( Eff (Embed IO) m
                       , Threaders '[ReaderThreads] m p
                       )
                    => IORef s
                    -> InterpretSimpleC (State s) m a
                    -> m a
runStateIORefSimple ref = interpretSimple $ \case
  Get   -> embed $ readIORef ref
  Put s -> embed $ writeIORef ref s
{-# INLINE runStateIORefSimple #-}

stateToIOSimple :: forall s m a p
                 . ( Eff (Embed IO) m
                   , Threaders '[ReaderThreads] m p
                   )
                => s
                -> InterpretSimpleC (State s) m a
                -> m (s, a)
stateToIOSimple s main = do
  ref <- embed $ newIORef s
  a   <- runStateIORefSimple ref main
  s'  <- embed $ readIORef ref
  return (s', a)
{-# INLINE stateToIOSimple #-}

runState :: forall s m a p
          . ( Carrier m
            , Threaders '[StateThreads] m p
            )
         => s
         -> StateC s m a
         -> m (s, a)
runState sInit m = do
  (a, sEnd) <- SSt.runStateT (unStateC m) sInit
  return (sEnd, a)
{-# INLINE runState #-}

evalState :: forall s m a p
           . ( Carrier m
             , Threaders '[StateThreads] m p
             )
          => s
          -> StateC s m a
          -> m a
evalState sInit m = do
  (a, _) <- SSt.runStateT (unStateC m) sInit
  return a
{-# INLINE evalState #-}

execState :: forall s m a p
           . ( Carrier m
             , Threaders '[StateThreads] m p
             )
          => s
          -> StateC s m a
          -> m s
execState sInit m = do
  (_, sEnd) <- SSt.runStateT (unStateC m) sInit
  return sEnd
{-# INLINE execState #-}

runStateLazy :: forall s m a p
              . ( Carrier m
                , Threaders '[StateLazyThreads] m p
                )
             => s
             -> StateLazyC s m a
             -> m (s, a)
runStateLazy sInit m = swap <$> LSt.runStateT (unStateLazyC m) sInit
{-# INLINE runStateLazy #-}

evalStateLazy :: forall s m a p
               . ( Carrier m
                 , Threaders '[StateLazyThreads] m p
                 )
              => s
              -> StateLazyC s m a
              -> m a
evalStateLazy sInit m = fst <$> LSt.runStateT (unStateLazyC m) sInit
{-# INLINE evalStateLazy #-}

execStateLazy :: forall s m a p
               . ( Carrier m
                 , Threaders '[StateLazyThreads] m p
                 )
              => s
              -> StateLazyC s m a
              -> m s
execStateLazy sInit m = snd <$> LSt.runStateT (unStateLazyC m) sInit
{-# INLINE execStateLazy #-}
