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
  , RunC
  , run

  , RunMC(RunMC)
  , runM

    -- * Integrating external monads
  , Embed(..)
  , embed

    -- * Effect interpretation
  , EffHandler

    -- ** 'interpretSimple'
  , interpretSimple
  , SimpleInterpreterFor
  , InterpretSimpleC

    -- ** 'interpretViaHandler'
  , interpretViaHandler
  , Handler(..)
  , MonadBase(..)
  , InterpretC

    -- ** 'interpret'
  , interpret
  , InterpreterFor
  , ReifiesHandler
  , ViaReifiedH
  , InterpretReifiedC

    -- * Effect reinterpretation
    -- ** 'reinterpretSimple'
  , reinterpretSimple
  , ReinterpretSimpleC
    -- ** 'reinterpretViaHandler'
  , reinterpretViaHandler
  , ReinterpretC
    -- ** 'reinterpret'
  , reinterpret
  , ReinterpretReifiedC

    -- * Lifting
  , MonadTrans(..)

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
  , IntroConsistent
  , KnownList
  , IntroC
  , IntroTopC
  , IntroUnderC

    -- * Combining effect carriers
  , CompositionC
  , runComposition

    -- * Other utilities
  , SubsumeC
  , subsume
  , Effly(..)
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

-- | A useful type synonym for the type of 'interpret' provided a handler
type InterpreterFor e m =
     forall x
   . InterpretReifiedC e m x
  -> m x

-- | A useful type synonym for the type of 'interpretSimple' provided a handler
type SimpleInterpreterFor e m =
     forall x p
   . Threaders '[ReaderThreads] m p
  => InterpretSimpleC e m x
  -> m x
