{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Carrier.Internal.Stepped where

import Data.Coerce
import Control.Monad.Trans
import Control.Monad.Trans.Free.Church.Alternate
import Control.Effect.Internal
import Control.Effect.Internal.Utils
import Control.Effect.Internal.Membership
import Control.Effect.Internal.Derive
import Control.Effect.Internal.Union

data FOEff e x where
  FOEff :: e q x -> FOEff e x

-- | A carrier for any first-order effect @e@ that allows for
-- dividing a computation into several steps, where
-- each step is seperated by the use of the effect.
--
-- This can be used to implement coroutines.
newtype SteppedC (e :: Effect) m a = SteppedC {
    unSteppedC :: FreeT (FOEff e) m a
  }
  deriving ( Functor, Applicative, Monad
           , MonadFail, MonadIO, MonadBase b
           , MonadThrow, MonadCatch
           )
  deriving MonadTrans

sendStepped :: e q a -> SteppedC e m a
sendStepped = SteppedC #. liftF . FOEff
{-# INLINE sendStepped #-}

instance ( Threads (FreeT (FOEff e)) (Prims m)
         , Carrier m
         )
      => Carrier (SteppedC e m) where
  type Derivs (SteppedC e m) = e ': Derivs m
  type Prims  (SteppedC e m) = Prims m

  algPrims = coerceAlg (thread @(FreeT (FOEff e)) (algPrims @m))
  {-# INLINE algPrims #-}

  reformulate n alg = powerAlg' (reformulate (n . lift) alg) (n . sendStepped)
  {-# INLINE reformulate #-}

deriving instance Functor m => Functor (Steps e m)

data Steps e m a where
  Done :: a -> Steps e m a
  More :: e q x -> (x -> m (Steps e m a)) -> Steps e m a

-- | Run the first-order effect @e@ by breaking the computation using it
-- into steps, where each step is seperated by the use of actions of @e@.
steps :: forall e m a p
       . ( Carrier m
         , Threaders '[FreeThreads] m p
         )
      => SteppedC e m a -> m (Steps e m a)
steps =
    foldFreeT
      Done
      (\c (FOEff e) -> return (More e c))
  .# unSteppedC
{-# INLINE steps #-}

liftSteps :: (MonadTrans t, Monad m) => Steps e m a -> Steps e (t m) a
liftSteps (Done a) = Done a
liftSteps (More e c) = More e (lift . fmap liftSteps . c)

-- | Execute all the steps of a computation.
unsteps :: forall e m a
         . ( forall z x. Coercible (e z x) (e m x)
           , Member e (Derivs m)
           , Carrier m
           )
         => Steps e m a -> m a
unsteps (Done a)   = return a
unsteps (More e c) = send @e (coerce e) >>= c >>= unsteps

type SteppedThreads = FreeThreads