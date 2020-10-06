{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Carrier.Internal.Stepped where

import Data.Coerce
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Free.Church.Alternate
import Control.Effect.Internal
import Control.Effect.Internal.Utils
import Control.Effect.Internal.Membership
import Control.Effect.Internal.Derive
import Control.Effect.Internal.Union

data FOEff e x where
  FOEff :: e q x -> FOEff e x

-- | A constraint that @e@ is first-order.
--
-- This is automatically deduced by the compiler.
class    (forall m n x. Coercible (e m x) (e n x))
      => FirstOrder (e :: Effect)
instance (forall m n x. Coercible (e m x) (e n x))
      => FirstOrder e

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

  algPrims = coerce (thread @(FreeT (FOEff e)) (algPrims @m))
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg' (reformulate (n . lift) alg) (n . sendStepped)
  {-# INLINEABLE reformulate #-}


data Steps (e :: Effect) m a where
  Done :: a -> Steps e m a
  More :: e q x -> (x -> m (Steps e m a)) -> Steps e m a

deriving instance Functor m => Functor (Steps e m)

instance Functor m => Applicative (Steps e m) where
  pure = Done
  {-# INLINE pure #-}

  liftA2 f (Done a) fb = fmap (f a) fb
  liftA2 f (More e c) fb = More e (fmap (\fa -> liftA2 f fa fb) . c)

instance Functor m => Monad (Steps e m) where
  Done a >>= f = f a
  More e c >>= f = More e (fmap (>>= f) . c)

-- | Run the first-order effect @e@ by breaking the computation using it
-- into steps, where each step is seperated by the use of actions of @e@.
steps :: forall e m a p
       . ( Carrier m
         , Threaders '[SteppedThreads] m p
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
         . ( FirstOrder e
           , Member e (Derivs m)
           , Carrier m
           )
         => Steps e m a -> m a
unsteps (Done a)   = return a
unsteps (More e c) = send @e (coerce e) >>= c >>= unsteps

-- | 'SteppedThreads' accepts the following primitive effects:
--
-- * 'Control.Effect.Regional.Regional' @s@
-- * 'Control.Effect.Optional.Optional' @s@ (when @s@ is a functor)
-- * 'Control.Effect.Type.Unravel.Unravel' @p@
-- * 'Control.Effect.Type.ListenPrim.ListenPrim' @s@ (when @s@ is a 'Monoid')
-- * 'Control.Effect.Type.ReaderPrim.ReaderPrim' @i@
type SteppedThreads = FreeThreads
