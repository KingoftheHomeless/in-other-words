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
  , runState
  , evalState
  , execState

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

    -- * Carriers
  , StateC
  , StateLazyC
  ) where

import Data.IORef
import Data.Tuple

import Control.Effect

import Control.Effect.Internal.State

import qualified Control.Monad.Trans.State.Strict as SSt
import qualified Control.Monad.Trans.State.Lazy as LSt

-- | Read and modify the state.
--
-- The resulting tuple of the computation is forced. You can
-- control what parts of the computation are evaluated by tying
-- their evaluation to the tuple.
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

-- | Runs a @'State' s@ effect by transforming it into non-atomic
-- operations over an 'IORef'.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'runStateIORef' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'runStateIORefSimple', which doesn't have a higher-rank type.
runStateIORef :: forall s m a
               . Eff (Embed IO) m
              => IORef s
              -> InterpretReifiedC (State s) m a
              -> m a
runStateIORef ref = interpret $ \case
  Get   -> embed $ readIORef ref
  Put s -> embed $ writeIORef ref s
{-# INLINE runStateIORef #-}

-- | Runs a @'State' s@ effect by transforming it into non-atomic
-- operations in IO.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'stateToIO' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'stateToIOSimple', which doesn't have a higher-rank type.
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

-- | Runs a @'State' s@ effect by transforming it into non-atomic
-- operations over an 'IORef'.
--
-- This is a less performant version of 'runStateIORef' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
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

-- | Runs a @'State' s@ effect by transforming it into non-atomic
-- operations in IO.
--
-- This is a less performant version of 'stateToIO' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
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

-- | Runs a @'State' s@ effect purely.
--
-- @'Derivs' ('StateC' s m) = 'State' s ': 'Derivs' m@
--
-- @'Control.Effect.Carrier.Prims'  ('StateC' e m) = 'Control.Effect.Carrier.Prims' m@
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

-- | Runs a @'State' s@ effect purely, discarding
-- the end state.
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

-- | Runs a @'State' s@ effect purely, discarding
-- the end result.
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

-- | Runs a @'State' s@ effect purely and lazily.
--
-- @'Derivs' ('StateLazyC' s m) = 'State' s ': 'Derivs' m@
--
-- @'Control.Effect.Carrier.Prims'  ('StateLazyC' e m) = 'Control.Effect.Carrier.Prims' m@
runStateLazy :: forall s m a p
              . ( Carrier m
                , Threaders '[StateLazyThreads] m p
                )
             => s
             -> StateLazyC s m a
             -> m (s, a)
runStateLazy sInit m = swap <$> LSt.runStateT (unStateLazyC m) sInit
{-# INLINE runStateLazy #-}

-- | Runs a @'State' s@ effect purely and lazily,
-- discarding the final state.
evalStateLazy :: forall s m a p
               . ( Carrier m
                 , Threaders '[StateLazyThreads] m p
                 )
              => s
              -> StateLazyC s m a
              -> m a
evalStateLazy sInit m = fst <$> LSt.runStateT (unStateLazyC m) sInit
{-# INLINE evalStateLazy #-}

-- | Runs a @'State' s@ effect purely and lazily,
-- discarding the end result.
execStateLazy :: forall s m a p
               . ( Carrier m
                 , Threaders '[StateLazyThreads] m p
                 )
              => s
              -> StateLazyC s m a
              -> m s
execStateLazy sInit m = snd <$> LSt.runStateT (unStateLazyC m) sInit
{-# INLINE execStateLazy #-}
