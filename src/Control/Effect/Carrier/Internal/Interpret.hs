{-# LANGUAGE AllowAmbiguousTypes, DerivingVia #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Carrier.Internal.Interpret where

import Data.Coerce

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Fix

import Control.Monad.Trans.Identity

import Control.Effect.Internal
import Control.Effect.Internal.Derive
import Control.Effect.Internal.Itself
import Control.Effect.Internal.KnownList
import Control.Effect.Internal.Union
import Control.Effect.Internal.Effly
import Control.Effect.Internal.Reflection
import Control.Effect.Internal.Utils
import Control.Monad.Base
import Control.Effect.Carrier.Internal.Intro

data HandlerCState p m z
  = HandlerCState (forall x. m x -> z x) (Algebra p z)

newtype ReifiedReformulation r p m = ReifiedReformulation {
    getReifiedReformulation :: Reformulation r p m
  }

newtype
    HandlerC
      (sHandler :: *)
      (sReform :: *)
      (r :: [Effect])
      (p :: [Effect])
      (m :: * -> *) z (a :: *)
  = HandlerC { unHandlerC :: z a }
  deriving (Functor, Applicative, Monad) via z

data CarrierReform m

instance (Carrier m, r ~ Derivs m, p ~ Prims m)
      => Reifies (CarrierReform m)
                 (ReifiedReformulation r p m) where
  reflect = ReifiedReformulation reformulate
  {-# INLINE reflect #-}


instance ( Reifies sHandler (HandlerCState p m z)
         , Reifies sReform (ReifiedReformulation r p m)
         , Monad z
         )
      => Carrier (HandlerC sHandler sReform r p m z) where
  type Derivs (HandlerC sHandler sReform r p m z) = r
  type Prims  (HandlerC sHandler sReform r p m z) = p

  algPrims =
    let
      HandlerCState _ alg = reflect @sHandler
    in
      coerce #. alg .# coerce
  {-# INLINE algPrims #-}

  reformulate n' alg =
    let
      HandlerCState n _ = reflect @sHandler
    in
      getReifiedReformulation (reflect @sReform) (n' . HandlerC #. n) alg
  {-# INLINE reformulate #-}

  algDerivs =
    let
      HandlerCState n alg = reflect @sHandler
    in
      getReifiedReformulation
        (reflect @sReform)
        (HandlerC #. n)
        (coerce #. alg .# coerce)
  {-# INLINE algDerivs #-}


instance ( Reifies sHandler (HandlerCState p m z)
         , Monad z
         , Monad m
         )
      => MonadBase m (HandlerC sHandler sReform r p m z) where
  liftBase m =
    let
      HandlerCState n _ = reflect @sHandler
    in
      HandlerC (n m)
  {-# INLINE liftBase #-}

newtype InterpretPrimC (s :: *) (e :: Effect) (m :: * -> *) a =
  InterpretPrimC {
      unInterpretPrimC :: m a
    }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

-- | The class of effect handlers for derived effects.
-- Instances of this class can be used together 'interpretViaHandler'
-- in order to interpret effects.
--
-- @h@ is the tag for the handler, @e@ is the effect to interpret,
-- and @m@ is the 'Carrier' on which the handler operates.
--
-- To define your own interpreter using this method, create a new
-- datatype without any constructors to serve as the tag
-- for the handler, and then define a 'Handler' instance for it.
-- Then, you can use your handler to interpret effects with
-- 'interpretViaHandler'.
--
-- Alternatively, you can use 'interpret' or 'interpretSimple',
-- which lets you avoid the need to define instances of 'Handler',
-- but come at other costs.
--
-- Any 'Handler' is required to work with /any/ carrier monad @z@ that
-- lifts @m@, and has the same derived and primitive effects as @m@ does.
-- The only constraints that are propagated to @z@ are membership
-- constraints:
-- @MonadIO m@ doesn't imply @MonadIO z@, but @Eff (Embed IO) m@ /does/
-- imply @Eff (Embed IO) z@.
--
-- In addition, since @z@ lifts @m@, you can lift values of @m@
-- to @z@ through 'liftBase'. This is most useful when using
-- 'interpret' or 'interpretSimple', as it allows you to
-- bring monadic values of @m@ from outside of the handler
-- (like arguments to the interpreter) into the handler.
--
-- The @z@ provided to the handler has 'Effly' wrapped around it,
-- so the handler may make use of the various instances of 'Effly'.
-- For example, you have access to 'MonadFix' inside the handler
-- if you have  @'Eff' 'Fix' m@.
--
-- Any effect to be handled needs to be
-- /representational in the monad parameter/. See 'RepresentationalEff'
-- for more information.
class ( RepresentationalEff e
      , Carrier m
      )
   => Handler (h :: *) e m where
  effHandler :: EffHandler e m


-- | The type of effect handlers for a derived effect @e@ with current monad @m@.
--
-- Don't let the type overwhelm you; in most cases, you can treat this as
-- @e m x -> m x@.
type EffHandler e m =
     forall z x
   . ( Carrier z
     , Prims z ~ Prims m
     , Derivs z ~ Derivs m
     , MonadBase m z
     )
  => e (Effly z) x -> Effly z x

class ( RepresentationalEff e
      , Carrier m
      ) => PrimHandler (h :: *) e m where
  effPrimHandler :: e m a -> m a

instance ( Carrier m
         , Handler h e m
         ) => Carrier (InterpretC h e m) where
  type Derivs (InterpretC h e m) = e ': Derivs m
  type Prims (InterpretC h e m) = Prims m

  algPrims = coerce (algPrims @m)
  {-# INLINE algPrims #-}

  reformulate !n !alg = powerAlg (reformulate (n .# InterpretC) alg) $
    let
      !handlerState = HandlerCState (n .# InterpretC) alg
    in
      reify handlerState $ \(_ :: p s) ->
        \e -> unHandlerC @s @(CarrierReform m) @_ @_ @m $ runEffly $
          effHandler @h @e @m (coerce e)
  {-# INLINE reformulate #-}

  algDerivs = powerAlg (coerce (algDerivs @m)) $ \e ->
    InterpretC $ unItself $ runEffly $ effHandler @h @e (coerce e)
  {-# INLINE algDerivs #-}

newtype InterpretC (h :: *) (e :: Effect) (m :: * -> *) a = InterpretC {
      unInterpretC :: m a
    }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT


newtype ReifiedHandler e m = ReifiedHandler {
  getReifiedHandler :: EffHandler e m
  }



newtype ReifiedPrimHandler (e :: Effect) m = ReifiedPrimHandler {
    getReifiedPrimHandler :: forall z x. Coercible z m => e z x -> m x
  }

coerceHandler :: (RepresentationalEff e, Coercible m n)
              => (e m a -> m a) -> e n a -> n a
coerceHandler = coerce

instance PrimHandler h e m => Carrier (InterpretPrimC h e m) where
  type Derivs (InterpretPrimC h e m) = e ': Derivs m
  type Prims (InterpretPrimC h e m) = e ': Prims m

  algPrims =
    powerAlg
      (coerce (algPrims @m))
      (coerceHandler (effPrimHandler @h @e @m))
  {-# INLINE algPrims #-}

  reformulate = addPrim (coerceReform (reformulate @m))
  {-# INLINE reformulate #-}

  algDerivs =
    powerAlg
      (coerce (algDerivs @m))
      (coerceHandler (effPrimHandler @h @e @m))
  {-# INLINE algDerivs #-}

data ViaReifiedH (s :: *)

instance ( RepresentationalEff e
         , Carrier m
         , Reifies s (ReifiedHandler e m)
         ) => Handler (ViaReifiedH s) e m where
  effHandler = getReifiedHandler (reflect @s)
  {-# INLINE effHandler #-}

instance ( RepresentationalEff e
         , Carrier m
         , Reifies s (ReifiedPrimHandler e m)
         ) => PrimHandler (ViaReifiedH s) e m where
  effPrimHandler = getReifiedPrimHandler (reflect @s)
  {-# INLINE effPrimHandler #-}

type InterpretReifiedC e m a =
     forall s
   . ReifiesHandler s e m
  => InterpretC (ViaReifiedH s) e m a

type InterpretPrimReifiedC e m a =
     forall s
   . ReifiesPrimHandler s e m
  => InterpretPrimC (ViaReifiedH s) e m a

newtype InterpretSimpleC (e :: Effect) (m :: * -> *) a = InterpretSimpleC {
      _unInterpretSimpleC :: ReaderT (ReifiedHandler e m) m a
    }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
       via ReaderT (ReifiedHandler e m) m

instance MonadTrans (InterpretSimpleC e) where
  lift m = InterpretSimpleC (lift m)
  {-# INLINE lift #-}

instance ( Threads (ReaderT (ReifiedHandler e m)) (Prims m)
         , RepresentationalEff e
         , Carrier m
         )
      => Carrier (InterpretSimpleC e m) where
  type Derivs (InterpretSimpleC e m) = e ': Derivs m
  type Prims  (InterpretSimpleC e m) = Prims m

  algPrims = coerceAlg (thread @(ReaderT (ReifiedHandler e m)) (algPrims @m))
  {-# INLINE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \e -> do
    ReifiedHandler handler <- n (InterpretSimpleC ask)
    let !handlerState = HandlerCState (n . lift) alg
    reify handlerState $ \(_ :: p s) ->
      unHandlerC @s @(CarrierReform m) @_ @_ @m $ runEffly $
        handler (coerce e)
  {-# INLINE reformulate #-}

newtype InterpretPrimSimpleC (e :: Effect) (m :: * -> *) a =
    InterpretPrimSimpleC {
      _unInterpretPrimSimpleC :: ReaderT (ReifiedPrimHandler e m) m a
    }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
       via ReaderT (ReifiedPrimHandler e m) m

instance MonadTrans (InterpretPrimSimpleC e) where
  lift m = InterpretPrimSimpleC (lift m)
  {-# INLINE lift #-}

instance ( Threads (ReaderT (ReifiedPrimHandler e m)) (Prims m)
         , ThreadsEff e (ReaderT (ReifiedPrimHandler e m))
         , RepresentationalEff e
         , Carrier m
         )
      => Carrier (InterpretPrimSimpleC e m) where
  type Derivs (InterpretPrimSimpleC e m) = e ': Derivs m
  type Prims  (InterpretPrimSimpleC e m) = e ': Prims m

  algPrims =
    powerAlg
      (coerce (thread @(ReaderT (ReifiedPrimHandler e m)) (algPrims @m)))
      $ \e -> InterpretPrimSimpleC $ ReaderT $ \rh@(ReifiedPrimHandler h) ->
        runReaderT (threadEff @_ @(ReaderT (ReifiedPrimHandler e m)) h (coerce e)) rh
  {-# INLINE algPrims #-}

  reformulate = addPrim (liftReform reformulate)
  {-# INLINE reformulate #-}

-- | Interpret an effect in terms of other effects, without needing to
-- define an explicit 'Handler' instance. This is an alternative to
-- 'interpretViaHandler', and is more performant than 'interpretSimple'.
--
-- See 'Handler' for more information about the handler you pass to
-- this function.
--
-- Note that 'InterpretReifiedC' is a type synonym for a higher-ranked type.
-- This unfortunately makes 'interpret' difficult to work with; you typically
-- can't use it partially applied, otherwise you get scary type errors.
-- For example, you can't compose 'interpret' with other interpreters
-- by using '.'; you must use '$' instead.
--
-- Consider using 'interpretSimple' instead if performance is secondary.
interpret
  :: forall e m a
   . (RepresentationalEff e, Carrier m)
  => EffHandler e m
  -> InterpretReifiedC e m a
  -> m a
interpret h m = reify (ReifiedHandler h) $ \(_ :: p s) ->
  unInterpretC @(ViaReifiedH s) m
{-# INLINE interpret #-}

-- | A significantly slower variant of 'interpret' that doesn't rely
-- on a higher-ranked type, making it simpler to use partially applied.
--
-- See 'Handler' for more information about the handler you pass to
-- this function.
interpretSimple
  :: forall e m a p
   . ( RepresentationalEff e
     , Threaders '[ReaderThreads] m p
     , Carrier m
     )
  => EffHandler e m
  -> InterpretSimpleC e m a
  -> m a
interpretSimple h m = coerce m (ReifiedHandler @e @m h)
{-# INLINE interpretSimple #-}

-- | Interpret an effect in terms of other effects by using
-- an explicit 'Handler' instance.
--
-- See 'Handler' for more information.
--
-- Unlike 'interpret', this does not have a higher-rank type,
-- making it easier to use partially applied.
interpretViaHandler :: forall h e m a
                     . Handler h e m
                    => InterpretC h e m a
                    -> m a
interpretViaHandler = unInterpretC
{-# INLINE interpretViaHandler #-}

-- | Interpret an effect as a new primitive effect.
--
-- Unlike 'interpret', the handler gains direct access to @m@,
-- which gives it significantly more power. In particular, this allows you
-- to interpret higher order effects in terms of the base monad
-- by making use of 'MonadBaseControl'.
--
-- However, if 'interpretPrim' is used, the interpreted effect is treated as a
-- /primitive/
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
  => InterpretPrimReifiedC e m a
  -> (forall x. e m x -> m x)
  -> m a
interpretPrim m h =
  let
    int :: ReifiedPrimHandler e m
    int = ReifiedPrimHandler (h .# coerce)
  in
    reify int $
      \(_ :: p s) -> interpretPrimViaHandler @(ViaReifiedH s) m
{-# INLINE interpretPrim #-}

interpretPrimViaHandler
  :: forall h e m a
   . PrimHandler h e m
  => InterpretPrimC h e m a
  -> m a
interpretPrimViaHandler = unInterpretPrimC
{-# INLINE interpretPrimViaHandler #-}

-- | A significantly slower variant of 'interpretPrim' that doesn't rely
-- on a higher-ranked type, making it simpler to use partially applied.
--
-- See 'Handler' for more information about the handler you pass to
-- this function.
interpretPrimSimple
  :: forall e m a p
   . ( RepresentationalEff e
     , Threaders '[ReaderThreads] m p
     , (forall i. ThreadsEff e (ReaderT i))
     , Carrier m
     )
  => (forall x. e m x -> m x)
  -> InterpretPrimSimpleC e m a
  -> m a
interpretPrimSimple h m = coerce m (ReifiedPrimHandler @e @m (h .# coerce))
{-# INLINE interpretPrimSimple #-}

-- | Add a derived effect to a reformulation
-- by providing a handler for that effect.
addDeriv :: ( RepresentationalEff e
            , Monad m
            )
         => Reformulation r p m
         -> (  forall z x
             . ( Carrier z
               , Prims z ~ p
               , Derivs z ~ r
               , MonadBase m z
               )
            => e (Effly z) x -> Effly z x
            )
         -> Reformulation (e ': r) p m
addDeriv !reform !h = \ !n !alg ->
  let
    !handlerState = HandlerCState n alg
  in
    reify handlerState $ \(_ :: pr sHandler) ->
    reify (ReifiedReformulation reform) $ \(_ :: pr sReform) ->
      powerAlg (reform n alg) $ \e ->
        unHandlerC @sHandler @sReform $ runEffly $ h (coerce e)
{-# INLINE addDeriv #-}



newtype ReinterpretC h e new m a = ReinterpretC {
    unReinterpretC :: IntroUnderC e new (InterpretC h e m) a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

deriving
  via
    IntroUnderC e new (InterpretC h e m)
  instance
       ( Handler h e m
       , Carrier m
       , KnownList new
       , IntroConsistent '[] new m
       )
    => Carrier (ReinterpretC h e new m)

type ReifiesHandler s e m = Reifies s (ReifiedHandler e m)
type ReifiesPrimHandler s e m = Reifies s (ReifiedPrimHandler e m)

type ReinterpretReifiedC e new m a =
     forall s
   . ReifiesHandler s e m
  => ReinterpretC (ViaReifiedH s) e new m a

reinterpret :: forall new e m a
             . ( RepresentationalEff e
               , KnownList new
               , HeadEffs new m
               )
            => EffHandler e m
            -> ReinterpretReifiedC e new m a
            -> m a
reinterpret h main = interpret h $ introUnder (unReinterpretC main)
{-# INLINE reinterpret #-}

reinterpretViaHandler :: forall h new e m a
                       . ( Handler h e m
                         , KnownList new
                         , IntroConsistent '[] new m
                         )
                      => ReinterpretC h e new m a
                      -> m a
reinterpretViaHandler = coerce
{-# INLINE reinterpretViaHandler #-}

newtype ReinterpretSimpleC e new m a = ReinterpretSimpleC {
    unReinterpretSimpleC :: IntroUnderC e new (InterpretSimpleC e m) a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving MonadTrans via InterpretSimpleC e

deriving via IntroUnderC e new (InterpretSimpleC e m)
    instance ( Threads (ReaderT (ReifiedHandler e m)) (Prims m)
             , RepresentationalEff e
             , KnownList new
             , IntroConsistent '[] new m
             , Carrier m
             )
          => Carrier (ReinterpretSimpleC e new m)

reinterpretSimple :: forall new e m a p
                   . ( RepresentationalEff e
                     , KnownList new
                     , HeadEffs new m
                     , Threaders '[ReaderThreads] m p
                     )
                  => EffHandler e m
                  -> ReinterpretSimpleC e new m a
                  -> m a
reinterpretSimple h =
     interpretSimple h
  .# introUnder
  .# unReinterpretSimpleC
{-# INLINE reinterpretSimple #-}
