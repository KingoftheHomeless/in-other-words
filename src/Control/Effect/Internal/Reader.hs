{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Reader where

import Data.Coerce

import Control.Effect
import Control.Effect.Carrier

import Control.Effect.Type.ReaderPrim

import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.Reader as R

-- | An effect for arbitrary input
data Ask i :: Effect where
  Ask :: Ask i m i

-- | An effect for locally modifying an environment
-- used to gain access to information.
data Local i :: Effect where
  Local :: (i -> i) -> m a -> Local i m a

-- | A pseudo-effect for connected @'Ask' i@ and @'Local' i@ effects.
--
-- @'Reader'@ should only ever be used inside of 'Eff' and 'Effs'
-- constraints. It is not a real effect! See 'Bundle'.
type Reader i = Bundle [Local i, Ask i]

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
  type Derivs (ReaderC i m) = Local i ': Ask i ': Derivs m
  type Prims  (ReaderC i m) = ReaderPrim i ': Prims m
  algPrims = powerAlg (coerce (thread @(ReaderT i) (algPrims @m))) $ \case
    ReaderPrimAsk -> ReaderC R.ask
    ReaderPrimLocal f (ReaderC m) -> ReaderC (R.local f m)
  {-# INLINEABLE algPrims #-}

  reformulate n alg =
    powerAlg (
    powerAlg (
      reformulate (n . lift) (weakenAlg alg)
    ) $ \case
      Ask -> n (ReaderC R.ask)
    ) $ \case
      Local f m -> (alg . inj) $ ReaderPrimLocal f m
  {-# INLINEABLE reformulate #-}
