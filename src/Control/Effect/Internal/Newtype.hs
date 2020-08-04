{-# LANGUAGE DefaultSignatures, DerivingVia #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Newtype
  (
    WrapC(WrapC)
  , wrap
  , wrapWith

  , UnwrapC(UnwrapC)
  , unwrap

  , UnwrapTopC(UnwrapTopC)
  , unwrapTop

  , EffNewtype(..)

  , WrapperOf(..)
  ) where

import Data.Coerce

import Control.Monad.Trans.Identity
import Control.Effect
import Control.Effect.Carrier
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

newtype UnwrapC (e :: Effect)
                m
                (a :: *) = UnwrapC { unUnwrapC :: m a }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance ( Carrier m
         , Member (UnwrappedEff e) (Derivs m)
         , EffNewtype e
         )
      => Carrier (UnwrapC e m) where
  type Derivs (UnwrapC e m) = e ': Derivs m
  type Prims  (UnwrapC e m) = Prims m

  algPrims = coerce (algPrims @m)
  {-# INLINE algPrims #-}

  reformulate n alg = powerAlg' (reformulate (n .# UnwrapC) alg) $
    \e -> reformulate (n .# UnwrapC) alg (Union membership (unwrapped e))
  {-# INLINE reformulate #-}

  algDerivs = powerAlg' (coerce (algDerivs @m)) $
    \e -> coerceAlg (algDerivs @m) (Union membership (unwrapped e))
  {-# INLINE algDerivs #-}

newtype UnwrapTopC (e :: Effect)
                m
                (a :: *) = UnwrapTopC { unUnwrapTopC :: m a }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance ( IntroConsistent '[] '[UnwrappedEff e] m
         , EffNewtype e
         , Carrier m
         )
      => Carrier (UnwrapTopC e m) where
  type Derivs (UnwrapTopC e m) = e ': StripPrefix '[UnwrappedEff e] (Derivs m)
  type Prims  (UnwrapTopC e m) = Prims m

  algPrims = coerce (algPrims @m)
  {-# INLINE algPrims #-}

  reformulate n alg = powerAlg' (weakenAlg (reformulate (n .# UnwrapTopC) alg)) $
    \e -> reformulate (n .# UnwrapTopC) alg (Union Here (unwrapped e))
  {-# INLINE reformulate #-}

  algDerivs = powerAlg' (weakenAlg (coerce (algDerivs @m))) $
    \e -> coerceAlg (algDerivs @m) (Union Here (unwrapped e))
  {-# INLINE algDerivs #-}



-- | Unwrap uses of an effect
unwrap :: forall e m a
        . ( Carrier m
          , Member (UnwrappedEff e) (Derivs m)
          , EffNewtype e
          )
       => UnwrapC e m a
       -> m a
unwrap = unUnwrapC
{-# INLINE unwrap #-}

-- | Unwrap uses of an effect, placing its unwrapped version on top
-- of the effect stack.
unwrapTop :: forall e m a
           . ( IntroConsistent '[] '[UnwrappedEff e] m
             , Carrier m
             , EffNewtype e
             )
          => UnwrapTopC e m a
          -> m a
unwrapTop = unUnwrapTopC
{-# INLINE unwrapTop #-}

class EffNewtype (e :: Effect) where
  type UnwrappedEff e :: Effect
  unwrapped :: e z x -> UnwrappedEff e z x
  default unwrapped :: Coercible e (UnwrappedEff e) => e z x -> UnwrappedEff e z x
  unwrapped = coerce

newtype WrapperOf (e :: Effect) (e' :: Effect) m a = WrapperOf (e m a)

instance Coercible e e' => EffNewtype (WrapperOf e e') where
  type UnwrappedEff (WrapperOf e e') = e'
