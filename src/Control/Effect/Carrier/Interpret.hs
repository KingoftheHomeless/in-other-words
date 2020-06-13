{-# LANGUAGE AllowAmbiguousTypes, MagicHash, BangPatterns, DerivingVia, UndecidableInstances #-}
module Control.Effect.Carrier.Interpret where

import Data.Coerce

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix

import Control.Monad.Trans.Identity

import Control.Effect.Internal
import Control.Effect.Internal.Union
import Control.Effect.Internal.Effly
import Control.Effect.Internal.Reflection
import Control.Effect.Internal.Utils
import Control.Monad.Base
import Control.Monad.Trans.Control
import GHC.Exts (Proxy#, proxy#)

data HandlerCState m z
  = HandlerCState (forall x. m x -> z x) (Algebra (Prims m) z)

newtype HandlerC (s :: *) (m :: * -> *) z (a :: *) = HandlerC { unHandlerC :: z a }
  deriving (Functor, Applicative, Monad) via z

instance ( Reifies s (HandlerCState m z)
         , Monad z
         , Carrier m
         ) => Carrier (HandlerC s m z) where
  type Derivs (HandlerC s m z) = Derivs m
  type Prims (HandlerC s m z) = Prims m

  algP =
    let
      HandlerCState _ alg = reflect @s
    in
      \u -> coerce (alg (coerce u))

  -- Eta-expand to help inlining
  reformulate n' alg =
    let
      HandlerCState n _ = reflect @s
    in
      \u -> reformulate @m (n' . HandlerC #. n) alg u

  algD =
    let
      HandlerCState n alg = reflect @s
    in
      \u -> reformulate @m (HandlerC #. n) (\u' -> coerce (alg (coerce u'))) u

instance ( Reifies s (HandlerCState m z)
         , Monad m
         , Monad z
         ) => MonadBase m (HandlerC s m z) where
  liftBase m = case reflect @s of
    HandlerCState n _ -> HandlerC (n m)


newtype InterpretPrimC' (s :: *) e m a = InterpretPrimC {
      unInterpretPrimC :: m a
    }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail
           , MonadBase b, MonadBaseControl b
           )
       via m
  deriving (MonadTrans, MonadTransControl) via IdentityT

newtype InterpretC' (s :: *) e m a = InterpretC {
      unInterpretC :: m a
    }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail
           , MonadBase b, MonadBaseControl b
           )
       via m
  deriving (MonadTrans, MonadTransControl) via IdentityT


newtype Interpretation e m = Interpretation (
      forall z x
    . Monad z
   => (forall y. m y -> z y)
   -> Algebra (Prims m) z
   -> e z x -> z x
  )

instance ( Carrier m
         , Reifies s (Interpretation e m)
         , RepresentationalEff e
         ) => Carrier (InterpretC' s e m) where
  type Derivs (InterpretC' s e m) = e ': Derivs m
  type Prims (InterpretC' s e m) = Prims m

  algP = coerce (algP @m)

  reformulate n alg = powerAlg (reformulate (n .# InterpretC) alg) $ \e ->
    case reflect @s of
      Interpretation int -> int (n .# InterpretC) alg (coerce e)

  algD = powerAlg (coerce (algD @m)) $ \e ->
    case reflect @s of
      Interpretation int -> int InterpretC algP (coerce e)


newtype PrimInterpretation e m = PrimInterpretation {
    getPrimInterpretation :: forall x. e m x -> m x
  }

coerceHandler :: (RepresentationalEff e, Coercible m n)
              => (e m a -> m a) -> e n a -> n a
coerceHandler = coerce

instance ( Carrier m
         , Reifies s (PrimInterpretation e m)
         , RepresentationalEff e
         ) => Carrier (InterpretPrimC' s e m) where
  type Derivs (InterpretPrimC' s e m) = e ': Derivs m
  type Prims (InterpretPrimC' s e m) = e ': Prims m

  algP =
    powerAlg
      (coerce (algP @m))
      (coerceHandler (getPrimInterpretation (reflect @s)))

  reformulate = liftReformulation (addPrimitive reformulate)

type InterpretC e m a =
     forall s
   . Reifies s (Interpretation e m)
  => InterpretC' s e m a

type InterpretPrimC e m a =
     forall s
   . Reifies s (PrimInterpretation e m)
  => InterpretPrimC' s e m a

-- | Interpret an effect in terms of other effects.
--
-- The handler is required to work with /any/ carrier monad @z@ that
-- lifts @m@, and has the same derived and primitive effects as @m@ does.
--
-- The only constraints that are propagated to @z@ are membership
-- constraints:
-- @MonadIO m@ doesn't imply @MonadIO z@, but @Eff (Embed IO) m@ /does/
-- imply @Eff (Embed IO) z@.
--
-- In addition, since @z@ lifts @m@, you can lift values of @m@
-- to @z@ through 'liftBase'. This is useful for bringing values of
-- @m@ from outside of the handler into the handler.
--
-- The @z@ provided to the handler has 'Effly' wrapped around it,
-- so the handler may make use of 'Effly' 's various instances.
-- For example, you have access to 'MonadFix' inside the handler
-- if you have  @'Eff' 'Fixpoint' m@.
--
-- Any effect interpreted with 'interpret' needs to be
-- /representational in the monad parameter/,
-- which is what the @RepresentationalEff@ constraint corresponds to.
-- Like any other coercibility constraint, this is automatically proven by
-- the compiler.
--
-- This is not a very serious requirement: even effects that don't satisfy this can
-- always be rewritten into equivalently powerful versions that are representational.
-- If you ever encounter that an effect you've written doesn't satisfy
-- @RepresentationalEff@, please consult [this guide.]
interpret
  :: forall e m a
   . (RepresentationalEff e, Carrier m)
  => InterpretC e m a
  -> (  forall z x
      . ( Carrier z
        , Prims z ~ Prims m
        , Derivs z ~ Derivs m
        , MonadBase m z
        )
     => e (Effly z) x -> Effly z x
     )
  -> m a
interpret m h =
  let
    int :: Interpretation e m
    int = Interpretation $ \ !n !alg e ->
      let
        !handlerState = HandlerCState n alg
      in
        reify handlerState $ \(_ :: p s) ->
          unHandlerC @s @m $ runEffly $ h (coerce e)
  in
    reify int (\(_ :: p s) -> unInterpretC @s m)
{-# INLINE interpret #-}

-- | Interpret an effect as a new primitive effect.
--
-- Unlike 'interpret', the handler gains direct access to @m@,
-- which gives it significantly more power. In particular, this allows you
-- to interpret higher order effects in terms of the base monad
-- by making use of 'MonadBaseControl'.
--
-- However, if 'interpretPrim' is used, the interpreted effect is treated as a /primitive/
-- effect, meaning that it must be lifted on a carrier-by-carrier basis via
-- the @'ThreadsEff'@ class, which could incur a massive amount of boilerplate.
--
-- Use 'interpret' instead if you don't need direct access to @m@. In particular,
-- try to find effects that could do the same job you intended to do by having access to @m@.
-- For example, instead of using 'interpretPrim' so that you may reach 'IO' in order to
-- use 'X.mask', you could use the 'Mask' effect instead; or if you want to catch exceptions,
-- you could use 'ErrorIO' instead.
--
-- That said, there are particular use-cases of 'interpretPrim' where incurred
-- boilerplate is minimal. See the [Guide] for more information.
interpretPrim
  :: forall e m a
   . (RepresentationalEff e, Carrier m)
  => InterpretPrimC e m a
  -> (forall x. e m x -> m x)
  -> m a
interpretPrim m h =
  let
    int :: PrimInterpretation e m
    int = PrimInterpretation h
  in
    reify int (\(_ :: p s) -> unInterpretPrimC @s m)
{-# INLINE interpretPrim #-}


reformulateNew
  :: forall n e m
   . ( Coercible n m
     , RepresentationalEff e
     , Carrier n
     )
  => (  forall z x
      . ( Carrier z
        , Prims z ~ Prims n
        , Derivs z ~ Derivs n
        , MonadBase n z
        )
     => e (Effly z) x -> Effly z x
     )
  -> Reformulation (e ': Derivs n) (Prims n) m
reformulateNew h = \n alg ->
  let
    !handlerState = HandlerCState (n .# coerce) alg
  in
    reify handlerState $ \(_ :: p s) ->
      powerAlg (reformulate @n (n .# coerce) alg) $ \e ->
        unHandlerC @s @n $ runEffly $ h (coerce e)
{-# INLINE reformulateNew #-}
