{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Intercept where

import Data.Coerce

import Control.Monad
import Control.Effect
import Control.Effect.Unlift
import Control.Effect.Carrier
import Control.Effect.State
import Control.Effect.Writer
import Control.Effect.Carrier.Internal.Stepped
import Control.Monad.Trans.Free.Church.Alternate
import Control.Monad.Trans.Reader

import Control.Effect.Type.Unravel
import Control.Effect.Type.ListenPrim

import Control.Effect.Internal.Utils

-- For coercion purposes
import Control.Effect.Carrier.Internal.Compose


-- | An effect for intercepting actions of a first-order effect.
--
-- Even for this library, proper usage of this effect is very complicated.
-- When properly used, this can be a very useful helper effect,
-- allowing you write interpretations for a class of higher-order effects
-- that wouldn't otherwise be possible.
--
-- For more information, see the
-- [wiki](https://github.com/KingoftheHomeless/in-other-words/wiki/Advanced-Topics#effect-interception).
data Intercept (e :: Effect) :: Effect where
  Intercept :: Coercible z m
            => (forall x. e z x -> m x)
            -> m a
            -> Intercept e m a

-- | A variant of 'InterceptCont' that is significantly more powerful, allowing
-- you to capture the continuation of the program at each use-site of an
-- effect, as well as aborting execution of the parameter computation
-- early.
data InterceptCont (e :: Effect) :: Effect where
  InterceptCont :: Coercible z m
                => InterceptionMode
                -> (forall x. (x -> m a) -> e z x -> m a)
                -> m a
                -> InterceptCont e m a

data InterceptionMode
  = InterceptOne
  | InterceptAll

data InterceptB e a where
  InterceptB :: (forall q x. (x -> a) -> e q x -> a)
             -> InterceptB e a

interceptB :: forall e m q a
            . ( FirstOrder e
              , Eff (Unravel (InterceptB e)) m
              )
           => (forall x. (x -> m a) -> e q x -> m a)
           -> m a -> m a
interceptB h m = join $ send $
  Unravel @(InterceptB e)
    (InterceptB (\c -> h c .# coerce))
    join
    (fmap pure m)
{-# INLINE interceptB #-}


newtype InterceptContC e m a = InterceptContC {
    unInterceptContC ::
        IntroC '[InterceptCont e, Intercept e]
               '[Unravel (InterceptB e)]
      ( InterpretC InterceptH (InterceptCont e)
      ( InterpretC InterceptH (Intercept e)
      ( InterpretPrimC InterceptH (Unravel (InterceptB e))
      ( SteppedC e
      ( m
      ))))) a
  } deriving ( Functor, Applicative, Monad
             , MonadFail, MonadIO
             , MonadThrow, MonadCatch
             , MonadBase b
             )
    deriving MonadTrans
    via CompositionBaseT
     '[ IntroC '[InterceptCont e, Intercept e]
                '[Unravel (InterceptB e)]
      , InterpretC InterceptH (InterceptCont e)
      , InterpretC InterceptH (Intercept e)
      , InterpretPrimC InterceptH (Unravel (InterceptB e))
      , SteppedC e
      ]

deriving instance ( FirstOrder e, Carrier m, Member e (Derivs m)
                  , Threaders '[SteppedThreads] m p
                  )
               => Carrier (InterceptContC e m)

data InterceptH

instance ( FirstOrder e
         , Eff (Unravel (InterceptB e)) m
         )
      => Handler InterceptH (Intercept e) m where
  effHandler (Intercept h m) =
    interceptB
      (\c e -> h e >>= c)
      m
  {-# INLINEABLE effHandler #-}

instance ( FirstOrder e
         , Member e (Derivs m)
         , Eff (Unravel (InterceptB e)) m
         )
      => Handler InterceptH (InterceptCont e) m where
  effHandler (InterceptCont mode h main) = case mode of
    InterceptAll -> interceptB h main
    InterceptOne ->
          send (Unravel
                  @(InterceptB e)
                  (InterceptB $ \c e b ->
                      if b then
                        h (`c` False) (coerce e)
                      else
                        send @e (coerce e) >>= (`c` b)
                  )
                  (\m b -> m >>= \f -> f b)
                  (fmap (const . pure) main)
               )
      >>= \f -> f True
  {-# INLINEABLE effHandler #-}

instance ( FirstOrder e
         , Carrier m
         , Threaders '[SteppedThreads] m p
         )
      => PrimHandler InterceptH
                     (Unravel (InterceptB e))
                     (SteppedC e m) where
  effPrimHandler (Unravel (InterceptB cataEff) cataM main) =
    return $ cataM
           $ lift
           $ unFreeT (unSteppedC main)
                     (\(FOEff e) c -> return $ cataEff (cataM . lift . c) e)
                     return
  {-# INLINEABLE effPrimHandler #-}


-- | Run @'Intercept' e@, @'InterceptCont' e@ and @e@ effects, provided
-- that @e@ is first-order and also part of the remaining effect stack.
--
-- There are three very important things to note here:
--
-- * __@e@ must be first-order.__
-- * __Any action of @e@ made by a handler run after 'runInterceptCont'__
-- __won't get be intercepted__. What this means is __that you typically want__
-- __to run the handler for @e@ immediately after 'runInterceptCont'__.
-- * __This imposes the very restrictive primitive effect__
-- __'Control.Effect.Type.Unravel.Unravel'__. Most notably, neither
-- 'StateThreads' nor 'WriterThreads' accepts it.
-- Because of that, this module offers various alternatives
-- of several common 'State' and 'Tell' interpreters with threading
-- constraints that do accept 'Unravel'.
--
-- @'Derivs' ('InterceptContC' e m) = 'InterceptCont' e ': 'Intercept' e ': e ': Derivs m@
--
-- @'Prims'  ('InterceptContC' e m) = 'Unravel' (InterceptB e) ': 'Prims' m@
runInterceptCont :: forall e m a p
                  . ( FirstOrder e
                    , Carrier m
                    , Member e (Derivs m)
                    , Threaders '[SteppedThreads] m p
                    )
                 => InterceptContC e m a
                 -> m a
runInterceptCont m =
       (\m' -> unFreeT m'
                       (\(FOEff e) c -> send @e (coerce e) >>= c)
                       return
       )
     $ unSteppedC
     $ interpretPrimViaHandler
     $ interpretViaHandler
     $ interpretViaHandler
     $ introUnderMany
     $ unInterceptContC
     $ m
{-# INLINE runInterceptCont #-}

-- | A variant of 'runState' with a 'SteppedThreads' threading constraint
-- instead of a 'StateThreads' threading constraint.
runStateStepped :: forall s m a p
                 . (Carrier m, Threaders '[SteppedThreads] m p)
                => s
                -> SteppedC (State s) m a
                -> m (s, a)
runStateStepped s0 main = do
  foldFreeT' (\m s -> m >>= \f -> f s)
             (\a s -> return (s, a))
             (\c (FOEff e) s -> case e of
                 Get -> c s s
                 Put s' -> c () s'
             )
             (unSteppedC main)
             s0
{-# INLINE runStateStepped #-}

-- | A variant of 'runTell' with a 'SteppedThreads' threading constraint
-- instead of a 'StateThreads' threading constraint.
runTellListStepped :: forall o m a p
                    . ( Carrier m
                      , Threaders '[SteppedThreads] m p
                      )
                   => SteppedC (Tell o) m a
                   -> m ([o], a)
runTellListStepped main =
  foldFreeT' (\m acc -> m >>= \f -> f acc)
             (\a acc -> return (reverse acc, a))
             (\c (FOEff (Tell o)) acc -> c () $! (o:acc))
             (unSteppedC main)
             []
{-# INLINE runTellListStepped #-}

-- | A variant of 'runTell' with a 'SteppedThreads' threading constraint
-- instead of a 'StateThreads' threading constraint.
runTellStepped :: forall w m a p
                . ( Monoid w
                  , Carrier m
                  , Threaders '[SteppedThreads] m p
                  )
               => SteppedC (Tell w) m a
               -> m (w, a)
runTellStepped main =
  foldFreeT' (\m acc -> m >>= \f -> f acc)
             (\a acc -> return (acc, a))
             (\c (FOEff (Tell o)) acc -> c () $! acc <> o)
             (unSteppedC main)
             mempty
{-# INLINE runTellStepped #-}

data ListenSteppedH

instance Eff (ListenPrim w) m
      => Handler ListenSteppedH (Listen w) m where
  effHandler (Listen m) = send $ ListenPrimListen m
  {-# INLINEABLE effHandler #-}

instance (Monoid w, Carrier m, Threaders '[SteppedThreads] m p)
      => PrimHandler ListenSteppedH (ListenPrim w) (SteppedC (Tell w) m) where
  effPrimHandler = \case
    ListenPrimTell w -> tell w
    ListenPrimListen main -> SteppedC $ FreeT $ \handler c -> do
      foldFreeT' (\mx acc -> mx >>= \f -> f acc)
                 (\a acc -> c (acc, a))
                 (\cn e@(FOEff (Tell o)) acc -> do
                     handler e $ \t -> cn t $! acc <> o
                 )
                 (unSteppedC main)
                 mempty
  {-# INLINEABLE effPrimHandler #-}

newtype ListenSteppedC w m a = ListenSteppedC {
    unListenSteppedC ::
        ReinterpretC ListenSteppedH (Listen w) '[ListenPrim w]
      ( InterpretPrimC ListenSteppedH (ListenPrim w)
      ( SteppedC (Tell w)
      ( m
      ))) a
  } deriving ( Functor, Applicative, Monad
             , MonadFail, MonadIO
             , MonadThrow, MonadCatch
             , MonadBase b
             )
    deriving MonadTrans
    via CompositionBaseT
     '[ ReinterpretC ListenSteppedH (Listen w) '[ListenPrim w]
      , InterpretPrimC ListenSteppedH (ListenPrim w)
      , SteppedC (Tell w)
      ]

deriving instance (Monoid w, Carrier m, Threaders '[SteppedThreads] m p)
               => Carrier (ListenSteppedC w m)

-- | A variant of 'runListen' with a 'SteppedThreads' threading constraint
-- instead of a 'StateThreads' threading constraint.
--
-- @'Derivs' ('ListenSteppedC' w m) = 'Listen' w ': 'Tell' w ': Derivs m@
--
-- @'Prims' ('ListenSteppedC' w m) = 'ListenPrim' w ': Derivs m@
runListenStepped :: forall w m a p
                . ( Monoid w
                  , Carrier m
                  , Threaders '[SteppedThreads] m p
                  )
               => ListenSteppedC w m a
               -> m (w, a)
runListenStepped m =
    runTellStepped
  $ interpretPrimViaHandler
  $ reinterpretViaHandler
  $ unListenSteppedC
  $ m
{-# INLINE runListenStepped #-}



newtype ReifiedFOHandler e m = ReifiedFOHandler (forall q x. e q x -> m x)

newtype InterceptRC (e :: Effect) m a = InterceptRC {
    unInterceptRC :: ReaderT (ReifiedFOHandler e m) m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )

instance MonadTrans (InterceptRC e) where
  lift = InterceptRC #. lift
  {-# INLINE lift #-}

instance ( FirstOrder e
         , Carrier m
         , Threads (ReaderT (ReifiedFOHandler e m)) (Prims m)
         )
      => Carrier (InterceptRC e m) where
  type Derivs (InterceptRC e m) = Intercept e ': e ': Derivs m
  type Prims  (InterceptRC e m) = Unlift (ReaderT (ReifiedFOHandler e m) m)
                                  ': Prims m

  algPrims =
    powerAlg (
      coerce (thread @(ReaderT (ReifiedFOHandler e m)) (algPrims @m))
    ) $ \case
      Unlift main -> InterceptRC (main unInterceptRC)
  {-# INLINEABLE algPrims #-}

  reformulate n alg =
    powerAlg (
    powerAlg' (
      reformulate (n . lift) (weakenAlg alg)
    ) $ \e -> do
        ReifiedFOHandler h <- n $ InterceptRC ask
        n $ lift $ h e
    ) $ \case
      Intercept h m ->
        (alg . inj) $ Unlift @(ReaderT (ReifiedFOHandler e m) m) $ \lower ->
          local
            (\h' -> ReifiedFOHandler $ \e ->
              runReaderT (lower (h (coerce e))) h'
            )
            (lower m)
  {-# INLINEABLE reformulate #-}


-- | Run @'Intercept' e@ and @e@ effects, provided
-- @e@ is first-order and part of the effect stack.
--
-- 'runInterceptR' differs from 'runInterceptCont' in four different ways:
--
-- * It doesn't handle 'InterceptCont'.
-- * It has the significantly less restrictive threading constraint
-- 'ReaderThreads' instead of 'SteppedThreads'
-- * It imposes the significantly /more/ restrictive primitive effect 'Unlift'
-- instead of 'Unravel'.
-- * It is significantly faster.
--
-- There are some interpreters -- such as 'Control.Effect.Bracket.bracketToIO' and 'Control.Effect.Conc.concToIO' --
-- that 'runInterceptCont' can't be used together with in any capacity
-- due to its 'SteppedThreads' threading constraint. In
-- these cases, 'runInterceptR' can be used instead.
--
-- @'Derivs' ('InterceptRC' e m) = 'Intercept' e ': e ': 'Derivs m'@
--
-- @'Prims'  ('InterceptRC' e m) = 'Unlift' (ReaderT (ReifiedFOHandler e m)) ': 'Derivs m'@
runInterceptR :: forall e m a p
               . ( FirstOrder e
                 , Member e (Derivs m)
                 , Carrier m
                 , Threaders '[ReaderThreads] m p
                 )
              => InterceptRC e m a
              -> m a
runInterceptR m =
  runReaderT (unInterceptRC m)
             (ReifiedFOHandler $ \e -> send @e (coerce e))
{-# INLINE runInterceptR #-}
