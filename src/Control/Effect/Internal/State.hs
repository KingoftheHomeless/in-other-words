{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.State where

import Data.Coerce

import Control.Effect
import Control.Effect.Carrier

import qualified Control.Monad.Trans.State.Strict as SSt
import qualified Control.Monad.Trans.State.Lazy as LSt

-- | An effect for __non-atomic__ stateful operations.
--
-- If you need atomicity, use 'Control.Effect.AtomicState.AtomicState'
-- instead.
data State s m a where
  Get :: State s m s
  Put :: s -> State s m ()

newtype StateC s m a = StateC { unStateC :: SSt.StateT s m a }
  deriving ( Functor, Applicative, Monad
           , MonadFix, Alternative, MonadPlus
           , MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl)

instance ( Carrier m
         , Threads (SSt.StateT s) (Prims m)
         )
      => Carrier (StateC s m) where
  type Derivs (StateC s m) = State s ': Derivs m
  type Prims  (StateC s m) = Prims m

  algPrims = coerce (thread @(SSt.StateT s) (algPrims @m))
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    Put s -> n $ StateC $ SSt.put s
    Get   -> n (StateC SSt.get)
  {-# INLINEABLE reformulate #-}

newtype StateLazyC s m a = StateLazyC { unStateLazyC :: LSt.StateT s m a }
  deriving ( Functor, Applicative, Monad
           , MonadFix, Alternative, MonadPlus
           , MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl)

instance ( Carrier m
         , Threads (LSt.StateT s) (Prims m)
         )
      => Carrier (StateLazyC s m) where
  type Derivs (StateLazyC s m) = State s ': Derivs m
  type Prims  (StateLazyC s m) = Prims m

  algPrims = coerce (thread @(LSt.StateT s) (algPrims @m))
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    Put s -> n $ StateLazyC $ LSt.put s
    Get   -> n (StateLazyC LSt.get)
  {-# INLINEABLE reformulate #-}

-- | 'StateLazyThreads' accepts the following primitive effects:
--
-- * 'Control.Effect.Regional.Regional' @s@
-- * 'Control.Effect.Optional.Optional' @s@ (when @s@ is a functor)
-- * 'Control.Effect.BaseControl.BaseControl' @b@
-- * 'Control.Effect.Type.ListenPrim.ListenPrim' @s@ (when @s@ is a 'Monoid')
-- * 'Control.Effect.Type.WriterPrim.WriterPrim' @s@ (when @s@ is a 'Monoid')
-- * 'Control.Effect.Type.ReaderPrim.ReaderPrim' @i@
-- * 'Control.Effect.Mask.Mask'
-- * 'Control.Effect.Bracket.Bracket'
-- * 'Control.Effect.Fix.Fix'
-- * 'Control.Effect.NonDet.Split'
class    ( forall s. Threads (LSt.StateT s) p
         ) => StateLazyThreads p
instance ( forall s. Threads (LSt.StateT s) p
         ) => StateLazyThreads p

-- | 'StateThreads' accepts the following primitive effects:
--
-- * 'Control.Effect.Regional.Regional' @s@
-- * 'Control.Effect.Optional.Optional' @s@ (when @s@ is a functor)
-- * 'Control.Effect.BaseControl.BaseControl' @b@
-- * 'Control.Effect.Type.ListenPrim.ListenPrim' @s@ (when @s@ is a 'Monoid')
-- * 'Control.Effect.Type.WriterPrim.WriterPrim' @s@ (when @s@ is a 'Monoid')
-- * 'Control.Effect.Type.ReaderPrim.ReaderPrim' @i@
-- * 'Control.Effect.Mask.Mask'
-- * 'Control.Effect.Bracket.Bracket'
-- * 'Control.Effect.Fix.Fix'
-- * 'Control.Effect.NonDet.Split'
class    ( forall s. Threads (SSt.StateT s) p
         ) => StateThreads p
instance ( forall s. Threads (SSt.StateT s) p
         ) => StateThreads p
