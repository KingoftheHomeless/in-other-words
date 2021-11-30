module Control.Effect
  ( -- * Core class
    Carrier(Derivs)
  , Effect
  , RepresentationalEff

    -- * Effect membership
  , Eff
  , Effs
  , Bundle
  , Member

    -- * Sending actions of effects
  , send

    -- * Running final monad
  , run

  , runM

    -- * Integrating external monads
  , Embed(..)
  , embed

    -- * Effect interpretation
  , interpretSimple
  , SimpleInterpreterFor

  , interpretViaHandler
  , Handler(..)

  , interpret
  , InterpreterFor

  , EffHandler

    -- * Effect reinterpretation
  , reinterpretSimple
  , reinterpretViaHandler
  , reinterpret

    -- * Threading constraints
  , Threaders
  , ReaderThreads

    -- * Effect Introduction
  , intro1
  , intro
  , introUnder1
  , introUnder
  , introUnderMany
  , HeadEff
  , HeadEffs

    -- * Combining effect carriers
  , CompositionC
  , runComposition

    -- * Other utilities
  , Effly(..)
  , subsume

   -- * Template Haskell
  , makeEff
  , makeEff_

    -- * Reexports from other modules
  , MonadBase(..)
  , MonadTrans(..)

    -- * Carriers and other misc. types
  , RunC
  , RunMC
  , InterpretSimpleC
  , InterpretC
  , InterpretReifiedC
  , ReifiesHandler
  , ViaReifiedH
  , ReinterpretSimpleC
  , ReinterpretC
  , ReinterpretReifiedC
  , IntroConsistent
  , IntroC
  , IntroTopC
  , IntroUnderC
  , IntroUnderManyC
  , KnownList
  , SubsumeC
  ) where

import Control.Effect.Internal
import Control.Effect.Internal.Effly
import Control.Effect.Internal.KnownList
import Control.Effect.Internal.Membership
import Control.Effect.Internal.Union
import Control.Effect.Embed
import Control.Effect.Carrier.Internal.Compose
import Control.Effect.Carrier.Internal.Intro
import Control.Effect.Carrier.Internal.Interpret
import Control.Monad.Base
import Control.Monad.Trans
import Control.Effect.Internal.TH.Effect (makeEff, makeEff_)

-- | A useful type synonym for the type of 'interpret' provided a handler
--
-- @m@ is left polymorphic so that you may place @'Eff'/s@ constraints on it.
type InterpreterFor e m =
     forall x
   . InterpretReifiedC e m x
  -> m x

-- | A useful type synonym for the type of 'interpretSimple' provided a handler
--
-- @m@ is left polymorphic so that you may place @'Eff'/s@ constraints on it.
type SimpleInterpreterFor e m =
     forall x p
   . Threaders '[ReaderThreads] m p
  => InterpretSimpleC e m x
  -> m x
