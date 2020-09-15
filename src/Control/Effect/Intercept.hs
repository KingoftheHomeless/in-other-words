module Control.Effect.Intercept
  ( -- * Effects
    Intercept(..)
  , InterceptCont(..)
  , InterceptionMode(..)

    -- * Actions
  , intercept
  , interceptCont
  , interceptCont1

    -- * Interpretations
  , runInterceptCont

    -- * Interpretations for other effects
  , runStateStepped
  , runTellStepped
  , runTellListStepped
  , runListenStepped

    -- * Threading constraints
  , SteppedThreads

    -- * Carriers
  , InterceptContC
  , SteppedC
  , ListenSteppedC
  ) where

import Control.Effect
import Control.Effect.Stepped
import Control.Effect.Internal.Intercept

-- | Intercept all uses of an effect within a region.
intercept :: Eff (Intercept e) m => (forall x. e m x -> m x) -> m a -> m a
intercept h m = send (Intercept h m)
{-# INLINE intercept #-}

-- | Intercept all uses of an effect within a region -- and at each use-site,
-- capture the continuation of the argument computation, and also allow for
-- early abortion (by not invoking the continuation).
interceptCont :: Eff (InterceptCont e) m
              => (forall x. (x -> m a) -> e m x -> m a)
              -> m a -> m a
interceptCont h m = send (InterceptCont InterceptAll h m)
{-# INLINE interceptCont #-}

-- | Intercept only the _first_ use of an effect within a region --
-- and at that use-site, capture the continuation of the argument computation,
-- and also allow for early abortion (by not invoking the continuation).
interceptCont1 :: Eff (InterceptCont e) m
               => (forall x. (x -> m a) -> e m x -> m a)
               -> m a -> m a
interceptCont1 h m = send (InterceptCont InterceptOne h m)
{-# INLINE interceptCont1 #-}
