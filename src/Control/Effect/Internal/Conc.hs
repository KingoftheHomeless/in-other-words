{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Conc where

import Control.Effect
import Control.Effect.Unlift
import Control.Effect.Newtype
import Control.Effect.Internal.Newtype
import Control.Effect.Internal.Utils

-- For coercion purposes
import Control.Effect.Internal.Derive
import Control.Effect.Carrier.Internal.Interpret
import Control.Effect.Carrier.Internal.Compose

-- | An effect for concurrent execution.
newtype Conc m a = Conc (Unlift IO m a)
  deriving EffNewtype via Conc `WrapperOf` Unlift IO

unliftConc :: Eff Conc m => ((forall x. m x -> IO x) -> IO a) -> m a
unliftConc main = wrapWith Conc $ unlift (\lower -> main (lower .# lift))
{-# INLINE unliftConc #-}

newtype ConcToIOC m a = ConcToIOC {
    unConcToIOC ::
        UnwrapTopC Conc
      ( UnliftToFinalC IO
      ( m
      )) a
  } deriving ( Functor, Applicative, Monad
             , Alternative, MonadPlus
             , MonadFix, MonadFail, MonadIO
             , MonadThrow, MonadCatch, MonadMask
             , MonadBase b, MonadBaseControl b
             )
    deriving MonadTrans
    via CompositionBaseT
     '[ UnwrapTopC Conc
      , UnliftToFinalC IO
      ]

deriving instance (Carrier m, MonadBaseControlPure IO m)
               => Carrier (ConcToIOC m)
