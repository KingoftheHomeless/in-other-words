module Control.Effect.Carrier.Reader where

import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.Reader as R

import Control.Effect.Internal
import Control.Effect.Internal.Union
import Control.Effect.Type.Reader

newtype ReaderC (m :: * -> *) a = ReaderC { unReaderC :: Bool -> m a }
  deriving (Functor, Applicative, Monad) via ReaderT Bool m

deriving via (ReaderT Bool m) instance (
    Carrier m
  , Threads (Prims m) (ReaderT Bool)
  ) => Carrier (ReaderC m)

