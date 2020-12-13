{-# LANGUAGE DerivingVia #-}
module Control.Effect.Fresh
  ( -- * Effects
    Fresh(..)

    -- * Actions
  , fresh

    -- * Interpretations
  , freshToIO

  , runFreshEnumIO

    -- * Unsafe interpretations
  , runFreshEnum

    -- * Simple variants of interpretations
  , runFreshEnumIOSimple

    -- * Threading constraints
  , StateThreads

    -- * Carriers
  , FreshToIOC
  , FreshEnumC
  ) where

import Data.Unique
import Data.IORef

import Control.Effect
import Control.Effect.State

-- For coercion purposes
import Control.Effect.Carrier
import Control.Effect.Internal.Utils
import Control.Effect.Carrier.Internal.Interpret
import Control.Effect.Carrier.Internal.Compose
import Control.Effect.Carrier.Internal.Intro
import Control.Monad.Trans.State.Strict (StateT)


-- | An effect for creating unique objects which may be used as references,
-- a la 'Unique'. Polymorphic code making use of 'Fresh' is expected
-- to place constraints upon @uniq@ as necessary.
--
-- Any interpreter for 'Fresh' has the responsibilty of ensuring
-- that any call to 'fresh' produces an object that __never__
-- compares equal to an object produced by a previous call to 'fresh'.
data Fresh uniq :: Effect where
  Fresh :: Fresh uniq m uniq

fresh :: Eff (Fresh uniq) m => m uniq
fresh = send Fresh
{-# INLINE fresh #-}

data FreshToIOH

instance Eff (Embed IO) m
      => Handler FreshToIOH (Fresh Unique) m where
  effHandler Fresh = embed newUnique
  {-# INLINEABLE effHandler #-}

type FreshToIOC = InterpretC FreshToIOH (Fresh Unique)

-- | Runs a 'Fresh' effect through generating 'Unique's using 'IO'.
--
-- @'Derivs' ('FreshToIOC' m) = 'Fresh' 'Unique' ': 'Derivs' m@
--
-- @'Control.Effect.Primitive.Prims'  ('FreshToIOC' m) = 'Control.Effect.Primitive.Prims' m@
freshToIO :: Eff (Embed IO) m
          => FreshToIOC m a
          -> m a
freshToIO = interpretViaHandler
{-# INLINE freshToIO #-}

-- | Run a 'Fresh' effect through atomic operations in 'IO'
-- by specifying an 'Enum' to be used as the type of unique objects.
--
-- This is a safe variant of 'runFreshEnum'.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'runFreshEnumIO' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'runFreshEnumIOSimple', which doesn't have a higher-rank type.
runFreshEnumIO :: forall uniq m a
                . ( Enum uniq
                  , Eff (Embed IO) m
                  )
               => InterpretReifiedC (Fresh uniq) m a
               -> m a
runFreshEnumIO m = do
  ref <- embed $ newIORef (toEnum @uniq 0)
  (`interpret` m) $ \case
    Fresh -> embed $ atomicModifyIORef' ref (\s -> (succ s, s))
{-# INLINE runFreshEnumIO #-}

-- | Run a 'Fresh' effect though atomic operations in 'IO'
-- by specifying an 'Enum' to be used as the type of unique objects.
--
-- This is a less performant version of 'runFreshEnumIO' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
runFreshEnumIOSimple :: forall uniq m a p
                      . ( Enum uniq
                        , Eff (Embed IO) m
                        , Threaders '[ReaderThreads] m p
                        )
                     => InterpretSimpleC (Fresh uniq) m a
                     -> m a
runFreshEnumIOSimple m = do
  ref <- embed $ newIORef (toEnum @uniq 0)
  (`interpretSimple` m) $ \case
    Fresh -> embed $ atomicModifyIORef' ref (\s -> (succ s, s))
{-# INLINE runFreshEnumIOSimple #-}

data FreshEnumH

instance (Enum uniq, Eff (State uniq) m)
      => Handler FreshEnumH (Fresh uniq) m where
  effHandler Fresh = state' (\s -> (succ s, s))
  {-# INLINEABLE effHandler #-}

newtype FreshEnumC uniq m a = FreshEnumC {
    unFreshEnumC ::
        ReinterpretC FreshEnumH (Fresh uniq) '[State uniq]
      ( StateC uniq
      ( m
      )) a
  } deriving ( Functor, Applicative, Monad
             , Alternative, MonadPlus
             , MonadFix, MonadFail, MonadIO
             , MonadThrow, MonadCatch, MonadMask
             , MonadBase b, MonadBaseControl b
             )
    deriving (MonadTrans, MonadTransControl)
    via CompositionBaseT
     '[ ReinterpretC FreshEnumH (Fresh uniq) '[State uniq]
      , StateC uniq
      ]

deriving instance (Carrier m, Enum uniq, Threads (StateT uniq) (Prims m))
               => Carrier (FreshEnumC uniq m)

-- | Run a 'Fresh' effect purely by specifying an 'Enum' to be used as the
-- type of unique objects.
--
-- __Beware:__ This is safe only if:
--
--   1. This is run after all interpreters which may revert local state
--      or produce multiple, inconsistent instances of local state.
--      This includes interpreters that may backtrack or produce multiple results
--      (such as 'Control.Error.Error.runError' or 'Control.Effect.NonDet.runNonDet').
--
--   2. You don't use any interpreter which may cause the final monad
--      to revert local state or produce multiple, inconsistent instances
--      of local state. This includes 'Control.Effect.Error.errorToIO' and
--      'Control.Effect.Conc.asyncToIO'.
--
-- Prefer 'freshToIO' or 'runFreshEnumIO' whenever possible.
--
-- @'Derivs' ('FreshEnumC' uniq m) = 'Fresh' uniq ': 'Derivs' m@
--
-- @'Control.Effect.Primitive.Prims'  ('FreshEnumC' uniq m) = 'Control.Effect.Primitive.Prims' m@
runFreshEnum :: forall uniq m a p
              . ( Enum uniq
                , Threaders '[StateThreads] m p
                , Carrier m
                )
             => FreshEnumC uniq m a
             -> m a
runFreshEnum =
     evalState (toEnum 0)
  .# reinterpretViaHandler
  .# unFreshEnumC
{-# INLINE runFreshEnum #-}
