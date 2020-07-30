{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Regional where
import Control.Effect.Internal.Union
import Control.Monad.Trans.Reader (ReaderT(..), mapReaderT)
import Control.Monad.Trans.Except (ExceptT(..), mapExceptT)
import qualified Control.Monad.Trans.State.Strict as SSt
import qualified Control.Monad.Trans.State.Lazy as LSt
import qualified Control.Monad.Trans.Writer.Lazy as LWr
import qualified Control.Monad.Trans.Writer.Strict as SWr
import qualified Control.Monad.Trans.Writer.CPS as CPSWr

-- | A /helper primitive effect/ for manipulating a region.
--
-- Helper primitive effects are effects that allow you to avoid interpreting one
-- of your own effects as a primitive if the power needed from direct access to
-- the underlying monad can instead be provided by the relevant helper primitive
-- effect. The reason why you'd want to do this is that helper primitive effects
-- already have 'ThreadsEff' instances defined for them; so you don't have to
-- define any for your own effect.
--
-- The helper primitive effects offered in this library are - in ascending
-- levels of power - 'Regional', 'Optional', 'BaseControl' and 'Unlift'.
--
-- The typical use-case of 'Regional' is to lift a natural transformation
-- of a base monad.
-- 'Control.Effect.Regional.Hoist' and accompaning interpreters is
-- provided as a specialization of 'Regional' for this purpose.
--
-- 'Regional' in its most general form lacks a pre-defined interpreter:
-- when not using 'Control.Effect.Regional.Hoist', you're expected to define
-- your own interpreter for 'Regional' (treating it as a primitive effect).
--
-- **'Regional' is typically used as a primitive effect.**
-- If you define a 'Control.Effect.Carrier' that relies on a novel
-- non-trivial monad transformer, then you need to make a
-- a @'ThreadsEff' ('Regional' s)@ instance for that monad transformer
-- (if possible). 'Control.Effect.Regional.threadRegionalViaOptional'
-- can help you with that.
data Regional s m a where
  Regional :: s -> m a -> Regional s m a

instance ThreadsEff (Regional s) (ExceptT e) where
  threadEff alg (Regional s m) = mapExceptT (alg . Regional s) m
  {-# INLINE threadEff #-}

instance ThreadsEff (Regional s) (ReaderT i) where
  threadEff alg (Regional s m) = mapReaderT (alg . Regional s) m
  {-# INLINE threadEff #-}

instance ThreadsEff (Regional s) (SSt.StateT i) where
  threadEff alg (Regional s m) = SSt.mapStateT (alg . Regional s) m
  {-# INLINE threadEff #-}

instance ThreadsEff (Regional s) (LSt.StateT i) where
  threadEff alg (Regional s m) = LSt.mapStateT (alg . Regional s) m
  {-# INLINE threadEff #-}

instance ThreadsEff (Regional s) (LWr.WriterT w) where
  threadEff alg (Regional s m) = LWr.mapWriterT (alg . Regional s) m
  {-# INLINE threadEff #-}

instance ThreadsEff (Regional s) (SWr.WriterT w) where
  threadEff alg (Regional s m) = SWr.mapWriterT (alg . Regional s) m
  {-# INLINE threadEff #-}

instance Monoid w => ThreadsEff (Regional s) (CPSWr.WriterT w) where
  threadEff alg (Regional s m) = CPSWr.mapWriterT (alg . Regional s) m
  {-# INLINE threadEff #-}
