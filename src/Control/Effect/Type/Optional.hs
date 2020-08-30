{-# LANGUAGE CPP, TupleSections #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Optional where

import Data.Functor.Const
import Control.Effect.Internal.Union
import Control.Effect.Type.Regional
import Control.Monad.Trans.Reader (ReaderT(..), mapReaderT)
import Control.Monad.Trans.Except (ExceptT(..), mapExceptT)
import qualified Control.Monad.Trans.State.Strict as SSt
import qualified Control.Monad.Trans.State.Lazy as LSt
import qualified Control.Monad.Trans.Writer.Lazy as LWr
import qualified Control.Monad.Trans.Writer.Strict as SWr
import qualified Control.Monad.Trans.Writer.CPS as CPSWr


-- | A /helper primitive effect/ for manipulating a region, with the option
-- to execute it in full or in part.
--
-- Helper primitive effects are effects that allow you to avoid interpreting one
-- of your own effects as a primitive if the power needed from direct access to
-- the underlying monad can instead be provided by the relevant helper primitive
-- effect. The reason why you'd want to do this is that helper primitive effects
-- already have 'ThreadsEff' instances defined for them, so you don't have to
-- define any for your own effect.
--
-- The helper primitive effects offered in this library are -- in order of
-- ascending power -- 'Control.Effect.Regional.Regional',
-- 'Control.Effect.Optional.Optional', 'Control.Effect.BaseControl.BaseControl'
-- and 'Control.Effect.Unlift.Unlift'.
--
-- The typical use-case of 'Regional' is to lift a natural transformation
-- of a base monad equipped with the power to recover from an exception.
-- 'Control.Effect.HoistOption' and accompaning interpreters is
-- provided as a specialization of 'Optional' for this purpose.
--
-- 'Optional' in its most general form lacks a pre-defined interpreter:
-- when not using 'Control.Effect.HoistOption', you're expected to define your
-- own interpreter for 'Optional' (treating it as a primitive effect).
-- Note that when used as a primitive effect, @s@ is expected to be a functor.
--
-- __'Optional' is typically used as a primitive effect.__
-- If you define a 'Control.Effect.Carrier' that relies on a novel
-- non-trivial monad transformer @t@, then you need to make a
-- a @Functor s => 'ThreadsEff' t ('Optional' s)@ instance (if possible).
-- 'Control.Effect.Optional.threadOptionalViaBaseControl'
-- can help you with that.
data Optional s m a where
  Optionally :: s a -> m a -> Optional s m a

-- | A valid definition of 'threadEff' for a @'ThreadsEff' ('Regional' s) t@ instance,
-- given that @t@ threads @'Optional' f@ for any functor @f@.
threadRegionalViaOptional :: ( ThreadsEff t (Optional (Const s))
                             , Monad m)
                          => (forall x. Regional s m x -> m x)
                          -> Regional s (t m) a -> t m a
threadRegionalViaOptional alg (Regionally s m) =
  threadEff
    (\(Optionally (Const s') m') -> alg (Regionally s' m'))
    (Optionally (Const s) m)
{-# INLINE threadRegionalViaOptional #-}

instance Functor s => ThreadsEff (ExceptT e) (Optional s) where
  threadEff alg (Optionally sa m) = mapExceptT (alg . Optionally (fmap Right sa)) m
  {-# INLINE threadEff #-}

instance ThreadsEff (ReaderT i) (Optional s) where
  threadEff alg (Optionally sa m) = mapReaderT (alg . Optionally sa) m
  {-# INLINE threadEff #-}

instance Functor s => ThreadsEff (SSt.StateT s') (Optional s) where
  threadEff alg (Optionally sa m) = SSt.StateT $ \s ->
    alg $ Optionally (fmap (, s) sa) (SSt.runStateT m s)
  {-# INLINE threadEff #-}

instance Functor s => ThreadsEff (LSt.StateT s') (Optional s) where
  threadEff alg (Optionally sa m) = LSt.StateT $ \s ->
    alg $ Optionally (fmap (, s) sa) (LSt.runStateT m s)
  {-# INLINE threadEff #-}

instance (Functor s, Monoid w) => ThreadsEff (LWr.WriterT w) (Optional s) where
  threadEff alg (Optionally sa m) =
    LWr.mapWriterT (alg . Optionally (fmap (, mempty) sa)) m
  {-# INLINE threadEff #-}

instance (Functor s, Monoid w) => ThreadsEff (SWr.WriterT w) (Optional s) where
  threadEff alg (Optionally sa m) =
    SWr.mapWriterT (alg . Optionally (fmap (, mempty) sa)) m
  {-# INLINE threadEff #-}

instance (Functor s, Monoid w)
      => ThreadsEff (CPSWr.WriterT w) (Optional s) where
  threadEff alg (Optionally sa m) =
    CPSWr.mapWriterT (alg . Optionally (fmap (, mempty) sa)) m
  {-# INLINE threadEff #-}
