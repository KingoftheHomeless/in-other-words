{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Mask
 ( -- * Effects
   Mask(..)
 , MaskMode(..)

   -- * Threading utilities
 , threadMaskViaClass
 ) where

import Control.Effect.Internal.Union
import Control.Effect.Internal.Reflection
import Control.Effect.Internal.ViaAlg
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import qualified Control.Monad.Catch as C
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.State.Strict as SSt
import qualified Control.Monad.Trans.State.Lazy as LSt
import qualified Control.Monad.Trans.Writer.Lazy as LWr
import qualified Control.Monad.Trans.Writer.Strict as SWr
import qualified Control.Monad.Trans.Writer.CPS as CPSWr


data MaskMode
  = InterruptibleMask
  | UninterruptibleMask

-- | An effect for masking asynchronous exceptions.
--
-- __'Mask' is typically used as a primitive effect.__
-- If you define a 'Control.Effect.Carrier' that relies on a novel
-- non-trivial monad transformer @t@, then you need to make a
-- a @'ThreadsEff' t 'Mask'@ instance (if possible).
-- 'threadMaskViaClass' can help you with that.
data Mask m a where
  Mask :: MaskMode
       -> ((forall x. m x -> m x) -> m a)
       -> Mask m a

instance Monad m => MonadThrow (ViaAlg s Mask m) where
  throwM = error "threadMaskViaClass: Transformers threading Mask \
                 \are not allowed to use throwM."

instance Monad m => MonadCatch (ViaAlg s Mask m) where
  catch = error "threadMaskViaClass: Transformers threading Mask \
                 \are not allowed to use catch."

instance ( Reifies s (ReifiedEffAlgebra Mask m)
         , Monad m
         )
      => MonadMask (ViaAlg s Mask m) where
  mask main = case reflect @s of
    ReifiedEffAlgebra alg -> coerceAlg alg (Mask InterruptibleMask main)
  {-# INLINE mask #-}

  uninterruptibleMask main = case reflect @s of
    ReifiedEffAlgebra alg -> coerceAlg alg (Mask UninterruptibleMask main)
  {-# INLINE uninterruptibleMask #-}

  generalBracket = error "threadMaskViaClass: Transformers threading Mask \
                         \are not allowed to use generalBracket."

-- | A valid definition of 'threadEff' for a @'ThreadsEff' t 'Mask'@ instance,
-- given that @t@ lifts @'MonadMask'@.
--
-- __BEWARE__: 'threadMaskViaClass' is only safe if the implementation of
-- 'Control.Monad.Catch.mask' and 'Control.Monad.Catch.uninterruptibleMask'
-- for @t m@ only makes use of 'Conrol.Monad.Catch.mask'
-- and 'Control.Monad.Catch.uninterruptibleMask' for @m@, and no other methods of
-- 'MonadThrow', 'MonadCatch', and 'MonadMask'.
threadMaskViaClass :: forall t m a
                    . Monad m
                   => ( RepresentationalT t
                      , forall b. MonadMask b => MonadMask (t b)
                      )
                   => (forall x. Mask m x -> m x)
                   -> Mask (t m) a -> t m a
threadMaskViaClass alg (Mask mode main) =
  reify (ReifiedEffAlgebra alg) $ \(_ :: pr s) ->
    unViaAlgT $ case mode of
      InterruptibleMask -> C.mask $ \restore ->
        viaAlgT @s @Mask $ main (mapUnViaAlgT restore)
      UninterruptibleMask -> C.uninterruptibleMask $ \restore ->
        viaAlgT @s @Mask $ main (mapUnViaAlgT restore)
{-# INLINE threadMaskViaClass #-}

#define THREAD_MASK(monadT)             \
instance ThreadsEff (monadT) Mask where \
  threadEff = threadMaskViaClass;       \
  {-# INLINE threadEff #-}

#define THREAD_MASK_CTX(ctx, monadT)             \
instance (ctx) => ThreadsEff (monadT) Mask where \
  threadEff = threadMaskViaClass;                \
  {-# INLINE threadEff #-}

THREAD_MASK(ReaderT i)
THREAD_MASK(ExceptT e)
THREAD_MASK(LSt.StateT s)
THREAD_MASK(SSt.StateT s)
THREAD_MASK_CTX(Monoid s, LWr.WriterT s)
THREAD_MASK_CTX(Monoid s, SWr.WriterT s)

instance Monoid s => ThreadsEff (CPSWr.WriterT s) Mask where
  threadEff alg (Mask mode main) = CPSWr.writerT $ alg $ Mask mode $ \restore ->
    CPSWr.runWriterT (main (CPSWr.mapWriterT restore))
  {-# INLINE threadEff #-}
