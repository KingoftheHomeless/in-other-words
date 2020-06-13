module Control.Effect.Type.Reader where

import Control.Monad.Trans
import Control.Effect.Internal.Union

import Control.Monad.Trans.Except (ExceptT(..))
import qualified Control.Monad.Trans.Except as E

import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.Reader as R

import qualified Control.Monad.Trans.State.Strict as SSt
import qualified Control.Monad.Trans.State.Lazy as LSt
import qualified Control.Monad.Trans.Writer.Lazy as LWr
import qualified Control.Monad.Trans.Writer.Strict as SWr
import qualified Control.Monad.Trans.Writer.CPS as CPSWr
import Control.Monad.Trans.Cont (ContT(..))
import qualified Control.Monad.Trans.Cont as C

data Reader i m a where
  Ask   :: Reader i m i
  Local :: (i -> i) -> m a -> Reader i m a

instance ThreadsEff (Reader i) (ExceptT e) where
  threadEff alg = \case
    Ask -> lift (alg Ask)
    Local f m -> E.mapExceptT (alg . Local f) m

instance ThreadsEff (Reader i) (SSt.StateT s) where
  threadEff alg = \case
    Ask -> lift (alg Ask)
    Local f m -> SSt.mapStateT (alg . Local f) m

instance ThreadsEff (Reader i) (LSt.StateT s) where
  threadEff alg = \case
    Ask -> lift (alg Ask)
    Local f m -> LSt.mapStateT (alg . Local f) m

instance Monoid w => ThreadsEff (Reader i) (LWr.WriterT w) where
  threadEff alg = \case
    Ask -> lift (alg Ask)
    Local f m -> LWr.mapWriterT (alg . Local f) m

instance Monoid w => ThreadsEff (Reader i) (SWr.WriterT w) where
  threadEff alg = \case
    Ask -> lift (alg Ask)
    Local f m -> SWr.mapWriterT (alg . Local f) m

instance Monoid w => ThreadsEff (Reader i) (CPSWr.WriterT w) where
  threadEff alg = \case
    Ask -> lift (alg Ask)
    Local f m -> CPSWr.mapWriterT (alg . Local f) m

instance ThreadsEff (Reader i) (C.ContT s) where
  threadEff alg = \case
    Ask -> lift (alg Ask)
    Local f m -> C.ContT $ \c -> do
      s <- alg Ask
      alg $ Local f $ runContT m (alg . Local (const s) . c)

instance ThreadsEff (Reader i) (ReaderT i') where
  threadEff alg = \case
    Ask -> lift (alg Ask)
    Local f m -> R.mapReaderT (alg . Local f) m
