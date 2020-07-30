{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Reader where

import Data.Coerce

import Control.Effect
import Control.Effect.Type.Reader
import Control.Effect.Carrier

import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.Reader as R

newtype ReaderC i m a = ReaderC {
    unReaderC :: ReaderT i m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl)

instance ( Threads (ReaderT i) (Prims m)
         , Carrier m
         )
      => Carrier (ReaderC i m) where
  type Derivs (ReaderC i m) = Reader i ': Derivs m
  type Prims  (ReaderC i m) = Reader i ': Prims m
  algPrims = powerAlg (coerce (thread @(ReaderT i) (algPrims @m))) $ \case
    Ask -> ReaderC (R.ask)
    Local f (ReaderC m) -> ReaderC (R.local f m)
  {-# INLINE algPrims #-}

  reformulate = addPrim (liftReform reformulate)
  {-# INLINE reformulate #-}
