{-# LANGUAGE CPP #-}
module Control.Effect.AtomicState
  ( -- * Effect
    AtomicState(..)

    -- * Actions
  , atomicState
  , atomicState'
  , atomicGet
  , atomicGets
  , atomicModify
  , atomicModify'

  , atomicPut

    -- * Interpretations
  , atomicStateToIO
  , runAtomicStateIORef
  , runAtomicStateTVar

  , AtomicStateToStateC
  , atomicStateToState

    -- * Simple variants of interpretations
  , atomicStateToIOSimple
  , runAtomicStateIORefSimple
  , runAtomicStateTVarSimple
  ) where

import Data.IORef
import Control.Concurrent.STM

import Control.Effect
import Control.Effect.State

#if MIN_VERSION_base(4,14,0)
import GHC.IO (atomicModifyIORefP)
#else

data Box a = Box a

atomicModifyIORefP :: IORef s -> (s -> (s, a)) -> IO a
atomicModifyIORefP ref f = do
  Box a <- atomicModifyIORef ref $ \s -> let !(s', a) = f s in (s', Box a)
  return a
{-# INLINE atomicModifyIORefP #-}
# endif

-- | An effect for atomically reading and writing a piece of state.
--
-- Convention: the interpreter for the @AtomicState@ action must force
-- the resulting tuple of the function, but not the end state or returned value.
data AtomicState s m a where
  AtomicState :: (s -> (s, a)) -> AtomicState s m a
  AtomicGet   :: AtomicState s m s

-- | Atomically read and modify the state.
--
-- The resulting tuple of the computation is forced. You can
-- control what parts of the computation are evaluated by tying
-- their evaluation to the tuple.
atomicState :: Eff (AtomicState s) m => (s -> (s, a)) -> m a
atomicState = send . AtomicState
{-# INLINE atomicState #-}

-- | Atomically read and strictly modify the state.
--
-- The resulting state -- but not the value returned -- is forced.
atomicState' :: Eff (AtomicState s) m => (s -> (s, a)) -> m a
atomicState' f = atomicState $ \s -> let (!s', a) = f s in (s', a)
{-# INLINE atomicState' #-}

-- | Read the state.
--
-- Depending on the interperation of 'AtomicState', this
-- can be more efficient than @'atomicState' (\s -> (s,s))@
atomicGet :: Eff (AtomicState s) m => m s
atomicGet = send AtomicGet
{-# INLINE atomicGet #-}

atomicGets :: Eff (AtomicState s) m => (s -> a) -> m a
atomicGets = (<$> atomicGet)
{-# INLINE atomicGets #-}

-- | Atomically modify the state.
--
-- The resulting state is not forced. 'atomicModify''
-- is a strict version that does force it.
atomicModify :: Eff (AtomicState s) m => (s -> s) -> m ()
atomicModify f = atomicState $ \s -> (f s, ())
{-# INLINE atomicModify #-}

-- | Atomically and strictly modify the state.
--
-- This is a strict version of 'atomicModify'.
atomicModify' :: Eff (AtomicState s) m => (s -> s) -> m ()
atomicModify' f = atomicState $ \s -> let !s' = f s in (s', ())
{-# INLINE atomicModify' #-}

-- | Atomically overwrite the state.
--
-- You typically don't want to use this, as
-- @'atomicGet' >>= 'atomicPut' . f@ isn't atomic.
atomicPut :: Eff (AtomicState s) m => s -> m ()
atomicPut s = atomicState $ \_ -> (s, ())
{-# INLINE atomicPut #-}

-- | Run an 'AtomicState' effect in terms of atomic operations in IO.
--
-- Internally, this simply creates a new 'IORef', passes it to
-- 'runAtomicStateIORef', and then returns the result and the final value
-- of the 'IORef'.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- **This makes 'atomicStateToIO' very difficult to use partially applied.**
-- **In particular, it can't be composed using @'.'@.**
--
-- If performance is secondary, consider using the slower
-- 'atomicStateToIOSimple', which doesn't have a higher-rank type.
atomicStateToIO :: forall s m a
                 . Eff (Embed IO) m
                => s
                -> InterpretReifiedC (AtomicState s) m a
                -> m (s, a)
atomicStateToIO sInit main = do
  ref  <- embed $ newIORef sInit
  a    <- runAtomicStateIORef ref main
  sEnd <- embed $ readIORef ref
  return (sEnd, a)
{-# INLINE atomicStateToIO #-}

atomicStateToIOSimple :: forall s m a p
                       . ( Eff (Embed IO) m
                         , Threaders '[ReaderThreads] m p
                         )
                      => s
                      -> InterpretSimpleC (AtomicState s) m a
                      -> m (s, a)
atomicStateToIOSimple sInit main = do
  ref  <- embed $ newIORef sInit
  a    <- runAtomicStateIORefSimple ref main
  sEnd <- embed $ readIORef ref
  return (sEnd, a)
{-# INLINE atomicStateToIOSimple #-}

-- | Run an 'AtomicState' effect by transforming it into atomic operations
-- over an 'IORef'.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- **This makes 'runAtomicStateIORef' very difficult to use partially applied.**
-- **In particular, it can't be composed using @'.'@.**
--
-- If performance is secondary, consider using the slower
-- 'runAtomicStateIORefSimple', which doesn't have a higher-rank type.
runAtomicStateIORef :: forall s m a
                     . Eff (Embed IO) m
                    => IORef s
                    -> InterpretReifiedC (AtomicState s) m a
                    -> m a
runAtomicStateIORef ref = interpret $ \case
  AtomicState f -> embed (atomicModifyIORefP ref f)
  AtomicGet     -> embed (readIORef ref)
{-# INLINE runAtomicStateIORef #-}

-- | Run an 'AtomicState' effect by transforming it into atomic operations
-- over an 'IORef'.
--
-- This is a less performant version of 'runAtomicStateIORef' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
runAtomicStateIORefSimple :: forall s m a p
                           . ( Eff (Embed IO) m
                             , Threaders '[ReaderThreads] m p
                             )
                          => IORef s
                          -> InterpretSimpleC (AtomicState s) m a
                          -> m a
runAtomicStateIORefSimple ref = interpretSimple $ \case
  AtomicState f -> embed (atomicModifyIORefP ref f)
  AtomicGet     -> embed (readIORef ref)
{-# INLINE runAtomicStateIORefSimple #-}

-- | Run an 'AtomicState' effect by transforming it into atomic operations
-- over an 'TVar'.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- **This makes 'runAtomicStateTVar' very difficult to use partially applied.**
-- **In particular, it can't be composed using @'.'@.**
--
-- If performance is secondary, consider using the slower
-- 'runAtomicStateTVarSimple', which doesn't have a higher-rank type.
runAtomicStateTVar :: forall s m a
                    . Eff (Embed IO) m
                   => TVar s
                   -> InterpretReifiedC (AtomicState s) m a
                   -> m a
runAtomicStateTVar tvar = interpret $ \case
  AtomicState f -> embed $ atomically $ do
    (s, a) <- f <$> readTVar tvar
    writeTVar tvar s
    return a
  AtomicGet     -> embed (readTVarIO tvar)
{-# INLINE runAtomicStateTVar #-}

-- | Run an 'AtomicState' effect by transforming it into atomic operations
-- over an 'TVar'.
--
-- This is a less performant version of 'runAtomicStateIORef' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
runAtomicStateTVarSimple :: forall s m a p
                          . ( Eff (Embed IO) m
                            , Threaders '[ReaderThreads] m p
                            )
                         => TVar s
                         -> InterpretSimpleC (AtomicState s) m a
                         -> m a
runAtomicStateTVarSimple tvar = interpretSimple $ \case
  AtomicState f -> embed $ atomically $ do
    (s, a) <- f <$> readTVar tvar
    writeTVar tvar s
    return a
  AtomicGet     -> embed (readTVarIO tvar)
{-# INLINE runAtomicStateTVarSimple #-}

data AtomicStateToStateH

type AtomicStateToStateC s = InterpretC AtomicStateToStateH (AtomicState s)

instance Eff (State s) m
      => Handler AtomicStateToStateH (AtomicState s) m where
  effHandler = \case
    AtomicState f -> state f
    AtomicGet     -> get
  {-# INLINE effHandler #-}

-- | Transform an 'AtomicState' effect into a 'State' effect, discarding atomicity.
atomicStateToState :: Eff (State s) m
                   => AtomicStateToStateC s m a
                   -> m a
atomicStateToState = interpretViaHandler
{-# INLINE atomicStateToState #-}
