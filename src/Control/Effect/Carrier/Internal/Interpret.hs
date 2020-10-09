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
class ( RepresentationalEff e
      , Carrier m
      )
   => Handler (h :: *) e m where
  effHandler :: EffHandler e m


-- | The type of effect handlers for a derived effect @e@ with current
-- carrier @m@.
--
-- Don't let the type overwhelm you; in most cases, you can treat this as
-- @e m x -> m x@.
--
-- Any 'EffHandler' is required to work with /any/ carrier monad @z@ that
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
-- if you have  @'Eff' 'Control.Effect.Fix.Fix' m@.
--
-- Any effect to be handled needs to be
-- /representational in the monad parameter/. See 'RepresentationalEff'
-- for more information.
type EffHandler e m =
     forall z x
   . ( Carrier z
     , Derivs z ~ Derivs m
     , Prims z ~ Prims m
     , MonadBase m z
     )
  => e (Effly z) x -> Effly z x

-- | The type of effect handlers for a primitive effect @e@ with current
-- carrier @m@.
--
-- Unlike 'EffHandler's, 'EffPrimHandler's have direct access to @m@,
-- giving them significantly more powerful.
--
-- That said, __you should interpret your own effects as primitives only as a__
-- __last resort.__ Every primitive effect comes at the cost of enormous amounts
-- of boilerplate: namely, the need for a 'ThreadsEff' instance for every
-- monad transformer that can thread that effect.
--
-- Some effects in this library are intended to be used as primitive effects,
-- such as 'Control.Effect.Regional.Regional'. Try to use such effects
-- to gain the power you need to interpret your effects instead of
-- defining your own primitive effects, since the primitive effects offered
-- in this library already have 'ThreadsEff' instances defined for them.
type EffPrimHandler e m = forall x. e m x -> m x

-- | The class of effect handlers for primitive effects.
-- Instances of this class can be used together 'interpretPrimViaHandler'
-- in order to interpret primitive effects.
--
-- @h@ is the tag for the handler, @e@ is the effect to interpret,
-- and @m@ is the 'Carrier' on which the handler operates.
--
-- To define your own interpreter using this method, create a new
-- datatype without any constructors to serve as the tag
-- for the handler, and then define a 'PrimHandler' instance for it.
-- Then, you can use your handler to interpret effects with
-- 'interpretPrimViaHandler'.
--
-- Alternatively, you can use 'interpretPrim' or 'interpretPrimSimple',
-- which lets you avoid the need to define instances of 'PrimHandler',
-- but come at other costs.
--
-- __Only interpret your own effects as primitives as a last resort.__
-- See 'EffPrimHandler'.
class ( RepresentationalEff e
      , Carrier m
      ) => PrimHandler (h :: *) e m where
  effPrimHandler :: EffPrimHandler e m

instance ( Carrier m
         , Handler h e m
         ) => Carrier (InterpretC h e m) where
  type Derivs (InterpretC h e m) = e ': Derivs m
  type Prims (InterpretC h e m) = Prims m

  algPrims = coerce (algPrims @m)
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n .# InterpretC) alg) $
    let
      !handlerState = HandlerCState (n .# InterpretC) alg
    in
      reify handlerState $ \(_ :: p s) ->
        \e -> unHandlerC @s @(CarrierReform m) @_ @_ @m $ runEffly $
          effHandler @h @e @m (coerce e)
  {-# INLINEABLE reformulate #-}

  algDerivs = powerAlg (coerce (algDerivs @m)) $ \e ->
    InterpretC $ unItself $ runEffly $ effHandler @h @e (coerce e)
  {-# INLINEABLE algDerivs #-}

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
{-# INLINE coerceHandler #-}

instance PrimHandler h e m => Carrier (InterpretPrimC h e m) where
  type Derivs (InterpretPrimC h e m) = e ': Derivs m
  type Prims (InterpretPrimC h e m) = e ': Prims m

  algPrims =
    powerAlg
      (coerce (algPrims @m))
      (coerceHandler (effPrimHandler @h @e @m))
  {-# INLINEABLE algPrims #-}

  reformulate = addPrim (coerceReform (reformulate @m))
  {-# INLINEABLE reformulate #-}

  algDerivs =
    powerAlg
      (coerce (algDerivs @m))
      (coerceHandler (effPrimHandler @h @e @m))
  {-# INLINEABLE algDerivs #-}

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
      unInterpretSimpleC :: ReaderT (ReifiedHandler e m) m a
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
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \e -> do
    ReifiedHandler handler <- n (InterpretSimpleC ask)
    let !handlerState = HandlerCState (n . lift) alg
    reify handlerState $ \(_ :: p s) ->
      unHandlerC @s @(CarrierReform m) @_ @_ @m $ runEffly $
        handler (coerce e)
  {-# INLINEABLE reformulate #-}

newtype InterpretPrimSimpleC (e :: Effect) (m :: * -> *) a =
    InterpretPrimSimpleC {
      unInterpretPrimSimpleC :: ReaderT (ReifiedPrimHandler e m) m a
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
         , ThreadsEff (ReaderT (ReifiedPrimHandler e m)) e
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
        runReaderT (threadEff @(ReaderT (ReifiedPrimHandler e m)) h (coerce e)) rh
  {-# INLINEABLE algPrims #-}

  reformulate = addPrim (liftReform reformulate)
  {-# INLINEABLE reformulate #-}

-- | Interpret an effect in terms of other effects, without needing to
-- define an explicit 'Handler' instance. This is an alternative to
-- 'interpretViaHandler', and is more performant than 'interpretSimple'.
--
-- See 'EffHandler' for more information about the handler you pass to
-- this function.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'interpret' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__ You must use
-- paranthesis or '$'.
--
-- Consider using 'interpretSimple' instead if performance is secondary.
--
-- Example usage:
--
-- @
-- data Teletype :: Effect where
--   ReadTTY  :: Teletype m String
--   WriteTTY :: String -> Teletype m ()
--
-- readTTY :: 'Eff' Teletype m => m String
-- readTTY = send ReadTTY
--
-- writeTTY :: 'Eff' Teletype m => String -> m ()
-- writeTTY = send . WriteTTY
--
-- echo :: 'Eff' Teletype m => m ()
-- echo = readTTY >>= sendTTY
--
-- teletypeToIO :: 'Eff' ('Control.Effect.Embed' IO) m => 'Control.Effect.InterpreterFor' Teletype m
-- teletypeToIO = 'interpret' $ \case
--   ReadTTY -> 'Control.Effect.embed' getLine
--   WriteTTY str -> 'Control.Effect.embed' $ putStrLn str
--
-- main :: IO ()
-- main = 'Control.Effect.runM' $ teletypeToIO $ echo
-- @
--
interpret :: forall e m a
           . (RepresentationalEff e, Carrier m)
          => EffHandler e m
          -> InterpretReifiedC e m a
          -> m a
interpret h m = reify (ReifiedHandler h) $ \(_ :: p s) ->
  unInterpretC @(ViaReifiedH s) m
{-# INLINE interpret #-}

-- | Interpret an effect in terms of other effects, without needing to
-- define an explicit 'Handler' instance. This is an alternative to
-- 'interpretViaHandler'.
--
-- See 'EffHandler' for more information about the handler you pass to
-- this function.
--
-- This is a significantly slower variant of 'interpret' that doesn't have
-- a higher-ranked type, making it much easier to use partially applied.
--
-- Note: this emits the threading constraint 'ReaderThreads' (see 'Threaders').
-- This makes 'interpretSimple' significantly less attractive to use
-- in application code, as it means propagating that constraint
-- through your application.
--
-- Example usage:
--
-- @
-- data Teletype :: Effect where
--   ReadTTY  :: Teletype m String
--   WriteTTY :: String -> Teletype m ()
--
-- readTTY :: 'Eff' Teletype m => m String
-- readTTY = send ReadTTY
--
-- writeTTY :: 'Eff' Teletype m => String -> m ()
-- writeTTY = send . WriteTTY
--
-- echo :: 'Eff' Teletype m => m ()
-- echo = readTTY >>= sendTTY
--
-- teletypeToIO :: 'Eff' ('Control.Effect.Embed' IO) m => 'Control.Effect.SimpleInterpreterFor' Teletype m
-- teletypeToIO = 'interpretSimple' $ \case
--   ReadTTY -> 'Control.Effect.embed' getLine
--   WriteTTY str -> 'Control.Effect.embed' $ putStrLn str
--
-- main :: IO ()
-- main = 'Control.Effect.runM' $ teletypeToIO $ echo
-- @
--
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
-- making it easier to use partially applied, and unlike
-- 'interpretSimple' doesn't sacrifice performance.
--
-- Example usage:
--
-- @
-- data Teletype :: Effect where
--   ReadTTY  :: Teletype m String
--   WriteTTY :: String -> Teletype m ()
--
-- readTTY :: 'Eff' Teletype m => m String
-- readTTY = send ReadTTY
--
-- writeTTY :: 'Eff' Teletype m => String -> m ()
-- writeTTY = send . WriteTTY
--
-- echo :: 'Eff' Teletype m => m ()
-- echo = readTTY >>= sendTTY
--
-- data TeletypeToIOH
--
-- instance 'Eff' ('Control.Effect.Embed' IO) m
--       => 'Handler' TeletypeToIOH Teletype m where
--   effHandler = \case
--     ReadTTY -> 'Control.Effect.embed' getLine
--     WriteTTY str -> 'Control.Effect.embed' $ putStrLn str
--
-- type TeletypeToIOC = 'InterpretC' TeletypeToIOH Teletype
--
-- teletypeToIO :: 'Eff' ('Control.Effect.Embed' IO) m => TeletypeToIOC m a -> m a
-- teletypeToIO = 'interpretViaHandler'
--
-- main :: IO ()
-- main = 'Control.Effect.runM' $ teletypeToIO $ echo
-- @
--
interpretViaHandler :: forall h e m a
                     . Handler h e m
                    => InterpretC h e m a
                    -> m a
interpretViaHandler = unInterpretC
{-# INLINE interpretViaHandler #-}

-- | Interpret an effect as a new primitive effect.
--
-- __*Only interpret your own effects as primitives as a last resort.__
-- See 'EffPrimHandler'.
--
-- This has a higher-rank type, as it makes use of 'InterpretPrimReifiedC'.
-- __This makes 'interpretPrim' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__ You must use
-- paranthesis or '$'.
--
-- Consider using 'interpretPrimSimple' instead if performance is secondary.
interpretPrim :: forall e m a
               . (RepresentationalEff e, Carrier m)
              => EffPrimHandler e m
              -> InterpretPrimReifiedC e m a
              -> m a
interpretPrim h m =
  let
    int :: ReifiedPrimHandler e m
    int = ReifiedPrimHandler (h .# coerce)
  in
    reify int $
      \(_ :: p s) -> interpretPrimViaHandler @(ViaReifiedH s) m
{-# INLINE interpretPrim #-}

-- | Interpret an effect as a new primitive effect by using
-- an explicit 'PrimHandler' instance.
--
-- See 'PrimHandler' for more information.
--
-- __Only interpret your own effects as primitives as a last resort.__
-- See 'EffPrimHandler'.
--
-- Unlike 'interpretPrim', this does not have a higher-rank type,
-- making it easier to use partially applied, and unlike
-- 'interpretPrimSimple' doesn't sacrifice performance.
interpretPrimViaHandler
  :: forall h e m a
   . PrimHandler h e m
  => InterpretPrimC h e m a
  -> m a
interpretPrimViaHandler = unInterpretPrimC
{-# INLINE interpretPrimViaHandler #-}

-- | A significantly slower variant of 'interpretPrim' that doesn't have
-- a higher-ranked type, making it much easier to use partially applied.
--
-- __*Only interpret your own effects as primitives as a last resort.__
-- See 'EffPrimHandler'.
--
-- Note the @ReaderThreads '[e]@ constraint, meaning
-- you need to define a @ThreadsEff e (ReaderT i)@ instance in order
-- to use 'interpretPrimSimple'.
interpretPrimSimple
  :: forall e m a p
   . ( RepresentationalEff e
     , Threaders '[ReaderThreads] m p
     , ReaderThreads '[e]
     , Carrier m
     )
  => EffPrimHandler e m
  -> InterpretPrimSimpleC e m a
  -> m a
interpretPrimSimple h m = coerce m (ReifiedPrimHandler @e @m (h .# coerce))
{-# INLINE interpretPrimSimple #-}

-- | Add a derived effect to a 'Reformulation'
-- by providing a handler for that effect.
--
-- The handler is an 'EffHandler', but with derived and primitive effects
-- determined by the transformed 'Reformulation'.
addDeriv :: ( RepresentationalEff e
            , Monad m
            )
         => (  forall z x
             . ( Carrier z
               , Derivs z ~ r
               , Prims z ~ p
               , MonadBase m z
               )
            => e (Effly z) x -> Effly z x
            )
         -> Reformulation r p m
         -> Reformulation (e ': r) p m
addDeriv !h !reform = \ !n !alg ->
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

-- | Reinterpret an effect in terms of newly introduced effects.
--
-- This combines 'interpret' and 'introUnder' in order to introduce the effects
-- @new@ under @e@, which you then may make use of inside the handler for @e@.
--
-- This has a higher-rank type, as it makes use of 'ReinterpretReifiedC'.
-- __This makes 'reinterpret' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__ You must use
-- paranthesis or '$'.
--
-- Consider using 'reinterpretSimple' instead if performance is secondary.
reinterpret :: forall e new m a
             . ( RepresentationalEff e
               , KnownList new
               , HeadEffs new m
               )
            => EffHandler e m
            -> ReinterpretReifiedC e new m a
            -> m a
reinterpret h main = interpret h $ introUnder (unReinterpretC main)
{-# INLINE reinterpret #-}

-- | Reinterpret an effect in terms of newly introduced effects by using
-- an explicit 'Handler' instance.
--
-- See 'Handler' for more information.
--
-- This combines 'interpretViaHandler' and 'introUnder' in order to introduce
-- the effects @new@ under @e@, which you then may make use of inside the handler
-- for @e@.
--
-- Unlike 'reinterpret', this does not have a higher-rank type,
-- making it easier to use partially applied, and unlike
-- 'reinterpretSimple' doesn't sacrifice performance.
reinterpretViaHandler :: forall h e new m a
                       . ( Handler h e m
                         , KnownList new
                         , HeadEffs new m
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
             , HeadEffs new m
             , Carrier m
             )
          => Carrier (ReinterpretSimpleC e new m)

-- | Reinterpret an effect in terms of newly introduced effects.
--
-- This combines 'interpretSimple' and 'introUnder' in order to introduce
-- the effects @new@ under @e@, which you then may make use of inside the
-- handler for @e@.
--
-- This is a significantly slower variant of 'reinterpret' that doesn't have
-- a higher-ranked type, making it much easier to use partially applied.
reinterpretSimple :: forall e new m a p
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
