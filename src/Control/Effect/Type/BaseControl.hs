{-# LANGUAGE AllowAmbiguousTypes, FlexibleInstances #-}
module Control.Effect.Type.BaseControl
  ( BaseControl(..)
  , baseControl
  , baseControl'
  , BaseControlPure(..)
  , baseControlPure
  , baseControlPure'
  , liftWithPure
  ) where

import Data.Coerce

import Control.Effect.Internal
import Control.Effect.Internal.Union
import Control.Effect.Internal.Utils
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict as SSt
import Control.Monad.Trans.State.Lazy as LSt
import Control.Monad.Trans.Writer.Lazy as LWr
import Control.Monad.Trans.Writer.Strict as SWr
import Control.Monad.Trans.Writer.CPS as CPSWr

newtype BaseControl b m a where
  BaseControl :: (  forall z
                  . (Coercible m z, MonadBaseControl b z)
                 => z a
                 )
              -> BaseControl b m a

baseControl
  :: Eff (BaseControl b) m
  => (  forall z
      . (MonadBaseControl b z, Coercible m z)
     => (forall x. m x -> z x)
     -> z a
     )
  -> m a
baseControl main = baseControl' (main coerce)

baseControl'
  :: Eff (BaseControl b) m
  => (  forall z
      . (MonadBaseControl b z, Coercible m z)
     => z a
     )
  -> m a
baseControl' main = send (BaseControl main)

class    a ~ StM m a => Pure m a
instance a ~ StM m a => Pure m a


class ( MonadBaseControl b m
      , forall x. Pure m x
      ) => MonadBaseControlPure b m

instance ( MonadBaseControl b m
         , forall x. Pure m x
         ) => MonadBaseControlPure b m


instance ThreadsEff (BaseControl b) (ExceptT e) where
  threadEff alg (BaseControl main) =
    ExceptT $ alg $ BaseControl (runExceptT main)

instance ThreadsEff (BaseControl b) (SSt.StateT e) where
  threadEff alg (BaseControl main) =
    SSt.StateT $ \s -> alg $ BaseControl (SSt.runStateT main s)

instance ThreadsEff (BaseControl b) (LSt.StateT e) where
  threadEff alg (BaseControl main) =
    LSt.StateT $ \s -> alg $ BaseControl (LSt.runStateT main s)

instance ThreadsEff (BaseControl b) (ReaderT i) where
  threadEff alg (BaseControl main) =
    ReaderT $ \i -> alg $ BaseControl (runReaderT main i)

instance Monoid w => ThreadsEff (BaseControl b) (LWr.WriterT w) where
  threadEff alg (BaseControl main) =
    LWr.WriterT $ alg $ BaseControl (LWr.runWriterT main)

instance Monoid w => ThreadsEff (BaseControl b) (SWr.WriterT w) where
  threadEff alg (BaseControl main) =
    SWr.WriterT $ alg $ BaseControl (SWr.runWriterT main)

-- monad-control still doesn't have a MonadBaseControl instance for CPS
-- WriterT, so we use a work-around to make this instance.
instance Monoid w => ThreadsEff (BaseControl b) (CPSWr.WriterT w) where
  threadEff alg (BaseControl main) =
    CPSWr.writerT $ alg $ BaseControl ((CPSWr.runWriterT .# unWriterCPS) main)


newtype WriterCPS s m a = WriterCPS { unWriterCPS :: CPSWr.WriterT s m a }
  deriving (Functor, Applicative, Monad) via CPSWr.WriterT s m
  deriving (MonadTrans) via CPSWr.WriterT s

instance MonadBase b m => MonadBase b (WriterCPS s m) where
  liftBase = lift . liftBase

instance (Monoid s, MonadBaseControl b m) => MonadBaseControl b (WriterCPS s m) where
  type StM (WriterCPS s m) a = StM m (a, s)

  liftBaseWith main = lift $ liftBaseWith $ \run_it ->
    main (run_it . CPSWr.runWriterT .# unWriterCPS)
  {-# INLINEABLE liftBaseWith #-}

  restoreM = WriterCPS #. CPSWr.writerT . restoreM
  {-# INLINEABLE restoreM #-}


newtype BaseControlPure b m a where
  BaseControlPure :: (  forall z
                      . (Coercible m z, MonadBaseControlPure b z)
                     => z a
                     )
                  -> BaseControlPure b m a

baseControlPure
  :: Eff (BaseControlPure b) m
  => (  forall z
      . (MonadBaseControlPure b z, Coercible m z)
     => (forall x. m x -> z x)
     -> z a
     )
  -> m a
baseControlPure main = baseControlPure' (main coerce)

baseControlPure'
  :: Eff (BaseControlPure b) m
  => (  forall z
      . (MonadBaseControlPure b z, Coercible z m)
     => z a
     )
  -> m a
baseControlPure' main = send (BaseControlPure main)

liftWithPure :: forall b m a
              . MonadBaseControlPure b m
             => ((forall x. m x -> b x) -> b a)
             -> m a
liftWithPure main = liftBaseWith $ \run_it ->
  main (run_it :: forall x. Pure m x => m x -> b x)

-- This is pretty terrible.
instance ThreadsEff (BaseControlPure b) (ReaderT i) where
  threadEff alg (BaseControlPure main :: BaseControlPure b (ReaderT i m) a) =
    ReaderT $ \i -> alg $ BaseControlPure
      (runReaderT (go @z main) i :: forall z. (Coercible z m, MonadBaseControlPure b z) => z a)

    where
      go :: forall z r. ((forall x. Pure z x => Pure (ReaderT i z) x) => r) -> r
      go a = a
      {-# INLINE go #-}
