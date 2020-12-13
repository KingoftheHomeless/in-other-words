{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Carrier.Internal.Intro where

import Data.Coerce

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Catch
import Control.Monad.Fix

import Control.Effect.Internal
import Control.Effect.Internal.Derive
import Control.Effect.Internal.Union
import Control.Effect.Internal.Utils
import Control.Effect.Internal.KnownList

newtype IntroC (top :: [Effect])
               (new :: [Effect])
               (m :: * -> *)
               a
    = IntroC { runIntroC :: m a }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

type RestDerivs top new m = StripPrefix new (StripPrefix top (Derivs m))

instance ( Carrier m
         , KnownList top
         , KnownList new
         , IntroConsistent top new m
         ) => Carrier (IntroC top new m) where
  type Derivs (IntroC top new m) = Append top (RestDerivs top new m)
  type Prims  (IntroC top new m) = Prims m

  algPrims = coerce (algPrims @m)
  {-# INLINEABLE algPrims #-}

  reformulate n alg =
    weakenAlgMid
      @(RestDerivs top new m)
      (singList @top)
      (singList @new)
      (reformulate (n .# IntroC) alg)
  {-# INLINEABLE reformulate #-}

  algDerivs =
    weakenAlgMid
      @(RestDerivs top new m)
      (singList @top)
      (singList @new)
      (coerce (algDerivs @m))
  {-# INLINEABLE algDerivs #-}


type IntroTopC = IntroC '[]
type IntroUnderC e = IntroC '[e]

-- | Synonym for 'IntroC' to match 'introUnderMany'
type IntroUnderManyC = IntroC

-- | A constraint that the effect stack of @m@ -- @'Control.Effect.Derivs' m@ --
-- begins with the effect @e@.
--
-- Note that unlike 'Control.Effect.Eff', this does not give
-- 'Control.Effect.Bundle' special treatment.
type HeadEff e m = (IntroConsistent '[] '[e] m, Carrier m)

-- | A constraint that the effect stack of @m@ -- @'Control.Effect.Derivs' m@ --
-- begins with @new@.
--
-- Note that unlike 'Control.Effect.Effs', this does not give
-- 'Control.Effect.Bundle' special treatment.
type HeadEffs new m = (IntroConsistent '[] new m, Carrier m)

-- | A constraint that the effect stack of @m@ -- @'Control.Effect.Derivs' m@ --
-- begins with @Append top new@.
type IntroConsistent top new m
  = (Append top (Append new (StripPrefix new (StripPrefix top (Derivs m)))) ~ Derivs m)

-- | Introduce multiple effects under a number of top effects of the effect
-- stack -- or rather, reveal those effects which were previously hidden.
--
-- @'Derivs' ('IntroUnderManyC' top new m) = Append top ('Control.Effect.Carrier.StripPrefix' (Append top new) ('Derivs' m))@
--
-- @'Prims'  ('IntroUnderManyC' top new m) = 'Prims' m@
introUnderMany :: forall top new m a
                . ( KnownList top
                  , KnownList new
                  , IntroConsistent top new m
                  )
               => IntroUnderManyC top new m a
               -> m a
introUnderMany = runIntroC
{-# INLINE introUnderMany #-}

-- | Introduce multiple effects under the top effect of the effect stack
-- -- or rather, reveal those effects which were previously hidden.
--
-- @'Derivs' ('IntroUnderC' e new m) = e ': 'Control.Effect.Carrier.StripPrefix' (e ': new) ('Derivs' m)@
--
-- @'Prims'  ('IntroUnderC' e new m) = 'Prims' m@
introUnder :: forall new e m a
            . ( KnownList new
              , IntroConsistent '[e] new m
              )
           => IntroUnderC e new m a
           -> m a
introUnder = runIntroC
{-# INLINE introUnder #-}

-- | Introduce an effect under the top effect of the effect stack
-- -- or rather, reveal that effect which was previously hidden.
--
-- @'Derivs' ('IntroUnderC' e '[new] m) = e ': 'Control.Effect.Carrier.StripPrefix' [e, new] ('Derivs' m)@
--
-- @'Prims'  ('IntroUnderC' e '[new] m) = 'Prims' m@
introUnder1 :: forall new e m a
             . IntroConsistent '[e] '[new] m
            => IntroUnderC e '[new] m a
            -> m a
introUnder1 = runIntroC
{-# INLINE introUnder1 #-}

-- | Introduce multiple effects on the top of the effect stack
-- -- or rather, reveal effects previously hidden.
--
-- @'Derivs' ('IntroTopC' new m) = 'Control.Effect.Carrier.StripPrefix' new ('Derivs' m)@
--
-- @'Prims'  ('IntroTopC' new m) = 'Prims' m@
intro :: forall new m a
       . ( KnownList new
         , IntroConsistent '[] new m
         )
      => IntroTopC new m a
      -> m a
intro = runIntroC
{-# INLINE intro #-}

-- | Introduce an effect at the top of the stack -- or rather, reveal an effect
-- previously hidden.
--
-- @'Derivs' ('IntroTopC' '[e] m) = 'Control.Effect.Carrier.StripPrefix' '[e] ('Derivs' m)@
--
-- @'Prims'  ('IntroTopC' '[e] m) = 'Prims' m@
intro1 :: forall e m a
        . IntroConsistent '[] '[e] m
       => IntroTopC '[e] m a
       -> m a
intro1 = runIntroC
{-# INLINE intro1 #-}
