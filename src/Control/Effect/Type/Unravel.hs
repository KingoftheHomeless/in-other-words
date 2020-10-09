{-# LANGUAGE TupleSections #-}
module Control.Effect.Type.Unravel where

import Control.Effect.Internal.Union
import Control.Monad.Trans
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Except (ExceptT(..))
-- import qualified Control.Monad.Trans.State.Strict as SSt
-- import qualified Control.Monad.Trans.State.Lazy as LSt
-- import qualified Control.Monad.Trans.Writer.Lazy as LWr
-- import qualified Control.Monad.Trans.Writer.Strict as SWr
-- import qualified Control.Monad.Trans.Writer.CPS as CPSWr

-- | A primitive effect which allows you to break a computation into layers.
-- This is the primitive effect underlying
-- 'Control.Effect.Intercept.Intercept' and
-- 'Control.Effect.Intercept.InterceptCont'.
--
-- Note: 'ThreadsEff' instances are not allowed to assume that @p@ is a functor.
--
-- __'Unravel' is typically used as a primitive effect.__
-- If you define a 'Control.Effect.Carrier' that relies on a novel
-- non-trivial monad transformer @t@, then you need to make
-- a @'ThreadsEff' t ('Unravel' p)@ instance (if possible).
--
-- The following threading constraints accept 'Unravel':
--
-- * 'Control.Effect.ReaderThreads'
-- * 'Control.Effect.Error.ErrorThreads'
-- * 'Control.Effect.NonDet.NonDetThreads'
-- * 'Control.Effect.Stepped.SteppedThreads'
-- * 'Control.Effect.Cont.ContThreads'
-- * 'Control.Effect.Select.SelectThreads'
data Unravel p :: Effect where
  Unravel :: p a -> (m a -> a) -> m a -> Unravel p m a

instance ThreadsEff (ReaderT i) (Unravel p) where
  threadEff alg (Unravel p cataM main) = ReaderT $ \i ->
    alg $ Unravel p (cataM . lift) (runReaderT main i)
  {-# INLINE threadEff #-}

instance ThreadsEff (ExceptT e) (Unravel p) where
  threadEff alg (Unravel p cataM (ExceptT main)) = lift $
    alg $ Unravel p (cataM . lift) (fmap (cataM . ExceptT . pure) main)
  {-# INLINE threadEff #-}

-- NOTE: These instances have very unintuitive semantics, so
-- we don't make them available.
{-
instance ThreadsEff (LSt.StateT s) (Unravel p) where
  threadEff alg (Unravel p cataM main) = LSt.StateT $ \s ->
    fmap (, s) $
      alg $ Unravel p
                    (cataM . lift)
                    (    (\t -> cataM (LSt.StateT $ \_ -> pure t))
                     <$> LSt.runStateT main s
                    )
  {-# INLINE threadEff #-}

instance ThreadsEff (SSt.StateT s) (Unravel p) where
  threadEff alg (Unravel p cataM main) = SSt.StateT $ \s ->
    fmap (, s) $
      alg $ Unravel p
                    (cataM . lift)
                    (    (\t -> cataM (SSt.StateT $ \_ -> pure t))
                     <$> SSt.runStateT main s
                    )
  {-# INLINE threadEff #-}

instance Monoid w => ThreadsEff (LWr.WriterT w) (Unravel p) where
  threadEff alg (Unravel p cataM main) = lift $
      alg $ Unravel p
                    (cataM . lift)
                    (    (\t -> cataM (LWr.WriterT $ pure t))
                     <$> LWr.runWriterT main
                    )
  {-# INLINE threadEff #-}

instance Monoid w => ThreadsEff (SWr.WriterT w) (Unravel p) where
  threadEff alg (Unravel p cataM main) = lift $
      alg $ Unravel p
                    (cataM . lift)
                    (    (\t -> cataM (SWr.WriterT $ pure t))
                     <$> SWr.runWriterT main
                    )
  {-# INLINE threadEff #-}

instance Monoid w => ThreadsEff (CPSWr.WriterT w) (Unravel p) where
  threadEff alg (Unravel p cataM main) = lift $
      alg $ Unravel p
                    (cataM . lift)
                    (    (\t -> cataM (CPSWr.writerT $ pure t))
                     <$> CPSWr.runWriterT main
                    )
  {-# INLINE threadEff #-}
-}
