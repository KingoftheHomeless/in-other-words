module Control.Effect
  ( -- * Core class
    Carrier(Derivs)
  , Effect
  , RepresentationalEff

    -- * Effect membership
  , Member
  , Eff
  , Effs
  , Bundle

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

    -- * Effect Interpretation
  , InterpretSimpleC
  , interpretSimple
  , SimpleInterpreterFor

  , Handler(..)
  , EffHandler
  , MonadBase(..)
  , InterpretC
  , interpretViaHandler

  , InterpretReifiedC
  , ReifiesHandler
  , ViaReifiedH
  , interpret
  , InterpreterFor

    -- ** Effect reinterpretation
  , ReinterpretC
  , reinterpretViaHandler

  , ReinterpretReifiedC
  , reinterpret

  , ReinterpretSimpleC
  , reinterpretSimple

    -- ** Trivial Interpretation
  , SubsumeC
  , subsume

    -- * Lifting
  , MonadTrans(..)

    -- * Threading constraints
  , Threaders
  , ReaderThreads

    -- * Effect introduction
  , HeadEff
  , HeadEffs
  , IntroConsistent
  , KnownList
  , IntroC
  , IntroTopC
  , IntroUnderC
  , intro1
  , intro
  , introUnder1
  , introUnder
  , introUnderMany

    -- * Combining effect carriers
  , CompositionC
  , runComposition

    -- * Other utilities
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
