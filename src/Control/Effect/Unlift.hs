{-# LANGUAGE DerivingVia #-}
module Control.Effect.Unlift
 ( -- * Effects
   Unlift(..)

   -- * Actions
 , unlift

   -- * Interpretations
 , MonadBaseControlPure
 , unliftToFinal

 , runUnlift

   -- * Threading utilities
 , threadUnliftViaClass

    -- * Combinators for 'Algebra's
    -- Intended to be used for custom 'Carrier' instances when
    -- defining 'algPrims'.
  , powerAlgUnlift
  , powerAlgUnliftFinal

    -- * Carriers
 , UnliftToFinalC
 , UnliftC
 ) where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Internal.Unlift

import Control.Effect.Type.Unlift

unlift :: Eff (Unlift b) m => ((forall x. m x -> b x) -> b a) -> m a
unlift main = send (Unlift main)
{-# INLINE unlift #-}

-- | Run a @'Unlift' m@ effect, where the unlifted monad @m@ is the
-- current monad.
--
-- @'Derivs' ('UnliftC' m) = 'Unlift' m ': 'Derivs' m@
--
-- @'Prims'  ('UnliftC' m) = 'Unlift' m ': 'Prims' m@
runUnlift :: Carrier m
          => UnliftC m a
          -> m a
runUnlift = unUnliftC
{-# INLINE runUnlift #-}

data UnliftToFinalH

instance ( MonadBaseControlPure b m
         , Carrier m
         )
      => PrimHandler UnliftToFinalH (Unlift b) m where
  effPrimHandler (Unlift main) = unliftBase main
  {-# INLINEABLE effPrimHandler #-}

type UnliftToFinalC b = InterpretPrimC UnliftToFinalH (Unlift b)

-- | Run a @'Unlift' b@ effect, where the unlifted monad @b@ is the
-- final base monad of @m@
--
-- @'Derivs' ('UnliftToFinalC' b m) = 'Unlift' b ': 'Derivs' m@
--
-- @'Prims'  ('UnliftToFinalC' b m) = 'Unlift' b ': 'Prims' m@
unliftToFinal :: ( MonadBaseControlPure b m
                 , Carrier m
                 )
              => UnliftToFinalC b m a
              -> m a
unliftToFinal = interpretPrimViaHandler
{-# INLINE unliftToFinal #-}

-- | Strengthen an @'Algebra' p m@ by adding a @'Unlift' m@ handler
powerAlgUnlift :: forall m p a
                . Algebra' p m a
               -> Algebra' (Unlift m ': p) m a
powerAlgUnlift alg = powerAlg alg $ \case
  Unlift main -> main id
{-# INLINE powerAlgUnlift #-}

-- | Strengthen an @'Algebra' p m@ by adding a @'Unlift' b@ handler, where
-- @b@ is the final base monad.
powerAlgUnliftFinal :: forall b m p a
                     . MonadBaseControlPure b m
                    => Algebra' p m a
                    -> Algebra' (Unlift b ': p) m a
powerAlgUnliftFinal alg = powerAlg alg $ \case
  Unlift main -> unliftBase main
{-# INLINE powerAlgUnliftFinal #-}
