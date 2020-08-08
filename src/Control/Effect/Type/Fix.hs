{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Fix where

import Control.Monad.Fix
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as LSt
import qualified Control.Monad.Trans.State.Strict as SSt
import qualified Control.Monad.Trans.Writer.Lazy as LWr
import qualified Control.Monad.Trans.Writer.Strict as SWr
import qualified Control.Monad.Trans.Writer.CPS as CPSWr

import Control.Effect.Internal.ViaAlg
import Control.Effect.Internal.Reflection
import Control.Effect.Internal.Utils
import Control.Effect.Internal.Union

-- | An effect corresponding to the 'MonadFix' type class.
--
-- 'Control.Effect.Effly.Effly''s 'MonadFix' instance is based
-- on this effect; by having access to 'Fix', you're able to
-- use recursive do notation inside of effect handlers.
--
-- __Fix is typically used as a primitive effect__.
-- If you define your own novel, non-trivial 'Control.Effect.Carrier',
-- then you need to make a @'ThreadsEff' 'Fix'@ instance for it (if possible).
newtype Fix m a where
  Fix :: (a -> m a) -> Fix m a

instance ( Reifies s (ReifiedEffAlgebra Fix m)
         , Monad m
         ) => MonadFix (ViaAlg s Fix m) where
  mfix f = case reflect @s of
    ReifiedEffAlgebra alg -> coerceAlg alg (Fix f)
  {-# INLINE mfix #-}


threadFixViaClass :: Monad m
                  => ( RepresentationalT t
                     , forall b. MonadFix b => MonadFix (t b)
                     )
                  => (forall x. Fix m x -> m x)
                  -> Fix (t m) a -> t m a
threadFixViaClass alg (Fix f) = reify (ReifiedEffAlgebra alg) $ \(_ :: pr s) ->
  unViaAlgT (mfix (viaAlgT @s @Fix #. f))
{-# INLINE threadFixViaClass #-}

#define THREADFIX(monadT)              \
instance ThreadsEff Fix (monadT) where \
  threadEff = threadFixViaClass;       \
  {-# INLINE threadEff #-}

#define THREADFIX_CTX(ctx, monadT)            \
instance ctx => ThreadsEff Fix (monadT) where \
  threadEff = threadFixViaClass;              \
  {-# INLINE threadEff #-}

-- TODO(KingoftheHomeless): Benchmark this vs hand-written instances.
THREADFIX(LSt.StateT s)
THREADFIX(SSt.StateT s)
THREADFIX_CTX(Monoid s, LWr.WriterT s)
THREADFIX_CTX(Monoid s, SWr.WriterT s)
THREADFIX(CPSWr.WriterT s)
THREADFIX(E.ExceptT e)
THREADFIX(R.ReaderT i)
