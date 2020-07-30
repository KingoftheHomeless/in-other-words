{-# LANGUAGE DerivingVia #-}
module Control.Effect.Unlift
 ( -- * Effect
   Unlift(..)

   -- * Actions
 , unlift

   -- * Carriers and interpreters
 , MonadBaseControlPure
 , UnliftToFinalC
 , unliftToFinal

 , UnliftC
 , runUnlift

   -- * Threading utilities
 , threadUnliftViaClass
 ) where

import Control.Effect
import Control.Effect.Primitive
import Control.Effect.Internal.Unlift

import Control.Effect.Type.Unlift

unlift :: Eff (Unlift b) m => ((forall x. m x -> b x) -> b a) -> m a
unlift main = send (Unlift main)
{-# INLINE unlift #-}

-- | Run a @'Unlift' m@ effect, where the unlifted monad @m@ is the
-- current monad.
--
-- @'Derivs' ('UnliftC' m) = 'Unlift' m ': 'Derivs' m@
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
  {-# INLINE effPrimHandler #-}

type UnliftToFinalC b = InterpretPrimC UnliftToFinalH (Unlift b)

-- | Run a @'Unlift' b@ effect, where the unlifted monad @b@ is the
-- final base monad of @m@
--
-- @'Derivs' ('UnliftToFinalC' b m) = 'Unlift' b ': 'Derivs' m@
-- @'Prims'  ('UnliftToFinalC' b m) = 'Unlift' b ': 'Prims' m@
unliftToFinal :: ( MonadBaseControlPure b m
                 , Carrier m
                 )
              => UnliftToFinalC b m a
              -> m a
unliftToFinal = interpretPrimViaHandler
{-# INLINE unliftToFinal #-}
