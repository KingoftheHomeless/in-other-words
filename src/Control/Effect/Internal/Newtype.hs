{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Newtype
  (
    WrapC(WrapC)
  , wrap
  , wrapWith

  , UnwrapC(UnwrapC)
  , unwrap
  , unwrapWith
  ) where

import Data.Coerce

import Control.Monad.Trans.Identity
import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Union
import Control.Effect.Internal.Utils

newtype WrapC (e :: Effect)
              (e' :: Effect)
              m
              (a :: *) = WrapC { unWrapC :: m a }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance ( Member e' (Derivs m)
         , Coercible e e'
         , Carrier m
         )
      => Carrier (WrapC e e' m) where
  type Derivs (WrapC e e' m) = e ': Derivs m
  type Prims  (WrapC e e' m) = Prims m

  algPrims = coerce (algPrims @m)
  {-# INLINE algPrims #-}

  reformulate n alg = powerAlg' (reformulate (n .# WrapC) alg) $
    \(e :: e z x) -> reformulate n alg (Union membership (coerce e :: e' z x))
  {-# INLINE reformulate #-}

  algDerivs = powerAlg' (coerce (algDerivs @m)) $
    \(e :: e z x) -> algDerivs (Union membership (coerce e :: e' z x))
  {-# INLINE algDerivs #-}

newtype UnwrapC (e :: Effect)
                (e' :: Effect)
                m
                (a :: *) = UnwrapC { unUnwrapC :: m a }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance ( IntroConsistent '[] '[e] m
         , Coercible e' e
         , Carrier m
         )
      => Carrier (UnwrapC e e' m) where
  type Derivs (UnwrapC e e' m) = e' ': StripPrefix '[e] (Derivs m)
  type Prims  (UnwrapC e e' m) = Prims m

  algPrims = coerce (algPrims @m)
  {-# INLINE algPrims #-}

  reformulate n alg = powerAlg' (weakenAlg (reformulate (n .# UnwrapC) alg)) $
    \(e :: e' z x) ->
      reformulate (n .# UnwrapC) alg (Union Here (coerce e :: e z x))
  {-# INLINE reformulate #-}

  algDerivs = powerAlg' (weakenAlg (coerce (algDerivs @m))) $
    \(e :: e' z x) -> coerceAlg (algDerivs @m) (Union Here (coerce e :: e z x))
  {-# INLINE algDerivs #-}


wrap :: ( Member e (Derivs m)
        , Carrier m
        , Coercible unwrappedE e
        )
     => WrapC unwrappedE e m a
     -> m a
wrap = unWrapC
{-# INLINE wrap #-}

-- | Wrap uses of an effect, injecting them into a newtype of that effect.
-- The first argument is ignored.
--
-- This is useful for creating actions of effect newtypes.
-- For example:
--
-- @
-- newtype Counter m a = Counter ('Control.Effect.State.State' Int m)
--
-- probe :: Eff Counter m => m Int
-- probe = 'wrapWith' Counter $ 'Control.Effect.State.state'' @Int (\s -> (s + 1, s))
-- @
--
wrapWith :: ( Member e (Derivs m)
            , Carrier m
            , Coercible unwrappedE e
            )
         => (unwrappedE z x -> e z x)
         -> WrapC unwrappedE e m a
         -> m a
wrapWith _ = wrap
{-# INLINE wrapWith #-}

unwrap :: forall e unwrappedE m a
        . ( IntroConsistent '[] '[unwrappedE] m
          , Carrier m
          , Coercible unwrappedE e
          )
       => UnwrapC unwrappedE e m a
       -> m a
unwrap = unUnwrapC
{-# INLINE unwrap #-}

unwrapWith :: forall e unwrappedE m a z x
            . ( IntroConsistent '[] '[unwrappedE] m
              , Carrier m
              , Coercible unwrappedE e
              )
           => (e z x -> unwrappedE z x)
           -> UnwrapC unwrappedE e m a
           -> m a
unwrapWith _ = unwrap
{-# INLINE unwrapWith #-}
