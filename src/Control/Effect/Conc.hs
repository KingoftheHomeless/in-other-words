{-# LANGUAGE DerivingVia #-}
module Control.Effect.Conc
  ( -- * Effect
    Conc
  , A.Async

    -- * Interpretations
  , ConcToIOC
  , concToIO

  , ConcToUnliftIOC
  , concToUnliftIO

    -- * Key actions
  , async
  , withAsync
  , wait
  , poll

  , concurrently
  , race

  , waitEither
  , waitBoth
  , link
  , link2

  , waitAny
  , mapConcurrently
  , forConcurrently

    -- * Concurrently applicative
  , Concurrently(..)

    -- * Other actions
  , asyncBound
  , asyncOn
  , asyncWithUnmask
  , asyncOnWithUnmask
  , withAsyncBound
  , withAsyncWithUnmask
  , withAsyncOnWithUnmask
  , waitCatch
  , cancel
  , uninterruptibleCancel
  , cancelWith
  , waitAnyCatch
  , waitAnyCancel
  , waitAnyCatchCancel
  , waitEitherCatch
  , waitEitherCancel
  , waitEitherCatchCancel
  , waitEither_
  , linkOnly
  , link2Only
  , race_
  , concurrently_
  , mapConcurrently_
  , forConcurrently_
  , replicateConcurrently
  , replicateConcurrently_

    -- Re-exports from Control.Concurrent.Async
  , A.asyncThreadId
  , A.AsyncCancelled(..)
  , A.ExceptionInLinkedThread(..)
  , A.waitAnySTM
  , A.waitAnyCatchSTM
  , A.waitEitherSTM
  , A.waitEitherCatchSTM
  , A.waitEitherSTM_
  , A.waitBothSTM
  , A.compareAsyncs
  ) where

import Control.Applicative
import Control.Monad

import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as A

import Control.Effect
import Control.Effect.Unlift

import Control.Exception (SomeException, Exception)
import Control.Effect.Internal.Newtype

-- For coercion purposes
import Control.Effect.Internal.Utils
import Control.Monad.Trans.Identity
import Control.Effect.Carrier.Internal.Intro
import Control.Effect.Carrier.Internal.Compose
import Control.Effect.Carrier.Internal.Interpret

-- | An effect for concurrent operations.
newtype Conc m a = Conc (Unlift IO m a)

unliftConc :: Eff Conc m => ((forall x. m x -> IO x) -> IO a) -> m a
unliftConc main = wrapWith Conc $ unlift (\lower -> main (lower .# lift))
{-# INLINE unliftConc #-}

data ConcH

instance Eff (Unlift IO) m
      => Handler ConcH Conc m where
  effHandler (Conc e) = send e
  {-# INLINE effHandler #-}

type ConcToUnliftIOC = InterpretC ConcH Conc

type ConcToIOC = CompositionC
 '[ ReinterpretC ConcH Conc '[Unlift IO]
  , UnliftToFinalC IO
  ]

-- | Run a 'Conc' effect if all effects in the effect stack are
-- eventually reduced to operations on 'IO'.
concToIO :: ( Carrier m
            , MonadBaseControlPure IO m
            )
          => ConcToIOC m a
          -> m a
concToIO =
     unliftToFinal
  .# reinterpretViaHandler
  .# runComposition
{-# INLINE concToIO #-}

-- | Transform a 'Conc' effect into @'Unlift' IO@.
concToUnliftIO :: Eff (Unlift IO) m
               => ConcToUnliftIOC m a
               -> m a
concToUnliftIO = interpretViaHandler
{-# INLINE concToUnliftIO #-}

async :: Eff Conc m => m a -> m (Async a)
async m = unliftConc $ \lower -> A.async (lower m)
{-# INLINE async #-}

asyncBound :: Eff Conc m => m a -> m (Async a)
asyncBound m = unliftConc $ \lower -> A.asyncBound (lower m)
{-# INLINE asyncBound #-}

asyncOn :: Eff Conc m => Int -> m a -> m (Async a)
asyncOn i m = unliftConc $ \lower -> A.asyncOn i (lower m)
{-# INLINE asyncOn #-}

asyncWithUnmask :: Eff Conc m => ((forall x. m x -> m x) -> m a) -> m (Async a)
asyncWithUnmask main = unliftConc $ \lower -> A.asyncWithUnmask $ \restore ->
  lower $ main $ \m -> unliftConc $ \lower' -> restore (lower' m)
{-# INLINE asyncWithUnmask #-}

asyncOnWithUnmask :: Eff Conc m => Int -> ((forall x. m x -> m x) -> m a) -> m (Async a)
asyncOnWithUnmask i main = unliftConc $ \lower -> A.asyncOnWithUnmask i $ \restore ->
  lower $ main $ \m -> unliftConc $ \lower' -> restore (lower' m)
{-# INLINE asyncOnWithUnmask #-}

withAsync :: Eff Conc m => m a -> (Async a -> m b) -> m b
withAsync m c = unliftConc $ \lower -> A.withAsync (lower m) (lower . c)
{-# INLINE withAsync #-}

withAsyncBound :: Eff Conc m => m a -> (Async a -> m b) -> m b
withAsyncBound m c = unliftConc $ \lower -> A.withAsyncBound (lower m) (lower . c)
{-# INLINE withAsyncBound #-}

withAsyncWithUnmask :: Eff Conc m => ((forall x. m x -> m x) -> m a) -> (Async a -> m b) -> m b
withAsyncWithUnmask main c = unliftConc $ \lower ->
  A.withAsyncWithUnmask
    (\restore -> lower $ main $ \m -> unliftConc $ \lower' -> restore (lower' m))
    (lower . c)
{-# INLINE withAsyncWithUnmask #-}


withAsyncOnWithUnmask :: Eff Conc m => Int -> ((forall x. m x -> m x) -> m a) -> (Async a -> m b) -> m b
withAsyncOnWithUnmask i main c = unliftConc $ \lower ->
  A.withAsyncOnWithUnmask i
    (\restore -> lower $ main $ \m -> unliftConc $ \lower' -> restore (lower' m))
    (lower . c)
{-# INLINE withAsyncOnWithUnmask #-}

wait :: Eff Conc m => Async a -> m a
wait a = unliftConc $ \_ -> A.wait a
{-# INLINE wait #-}

poll :: Eff Conc m => Async a -> m (Maybe (Either SomeException a))
poll a = unliftConc $ \_ -> A.poll a
{-# INLINE poll #-}

waitCatch :: Eff Conc m => Async a -> m (Either SomeException a)
waitCatch a = unliftConc $ \_ -> A.waitCatch a
{-# INLINE waitCatch #-}

cancel :: Eff Conc m => Async a -> m ()
cancel a = unliftConc $ \_ -> A.cancel a
{-# INLINE cancel #-}

uninterruptibleCancel :: Eff Conc m => Async a -> m ()
uninterruptibleCancel a = unliftConc $ \_ -> A.uninterruptibleCancel a
{-# INLINE uninterruptibleCancel #-}

cancelWith :: Eff Conc m => (Exception e, Eff Conc m) => Async a -> e -> m ()
cancelWith a e = unliftConc $ \_ -> A.cancelWith a e
{-# INLINE cancelWith #-}

waitAny :: Eff Conc m => [Async a] -> m (Async a, a)
waitAny as = unliftConc $ \_ -> A.waitAny as
{-# INLINE waitAny #-}

waitAnyCatch :: Eff Conc m => [Async a] -> m (Async a, Either SomeException a)
waitAnyCatch as = unliftConc $ \_ -> A.waitAnyCatch as
{-# INLINE waitAnyCatch #-}

waitAnyCancel :: Eff Conc m => [Async a] -> m (Async a, a)
waitAnyCancel as = unliftConc $ \_ -> A.waitAnyCancel as
{-# INLINE waitAnyCancel #-}

waitAnyCatchCancel :: Eff Conc m => [Async a] -> m (Async a, Either SomeException a)
waitAnyCatchCancel as = unliftConc $ \_ -> A.waitAnyCatchCancel as
{-# INLINE waitAnyCatchCancel #-}

waitEither :: Eff Conc m => Async a -> Async b -> m (Either a b)
waitEither aa ab = unliftConc $ \_ -> A.waitEither aa ab
{-# INLINE waitEither #-}


waitEitherCatch :: Eff Conc m => Async a -> Async b -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatch aa ab = unliftConc $ \_ -> A.waitEitherCatch aa ab
{-# INLINE waitEitherCatch #-}

waitEitherCancel :: Eff Conc m => Async a -> Async b -> m (Either a b)
waitEitherCancel aa ab = unliftConc $ \_ -> A.waitEitherCancel aa ab
{-# INLINE waitEitherCancel #-}

waitEitherCatchCancel :: Eff Conc m => Async a -> Async b -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchCancel aa ab = unliftConc $ \_ -> A.waitEitherCatchCancel aa ab
{-# INLINE waitEitherCatchCancel #-}

waitEither_ :: Eff Conc m => Async a -> Async b -> m ()
waitEither_ aa ab = unliftConc $ \_ -> A.waitEither_ aa ab
{-# INLINE waitEither_ #-}

waitBoth :: Eff Conc m => Async a -> Async b -> m (a, b)
waitBoth aa ab = unliftConc $ \_ -> A.waitBoth aa ab
{-# INLINE waitBoth #-}

link :: Eff Conc m => Async a -> m ()
link a = unliftConc $ \_ -> A.link a
{-# INLINE link #-}

linkOnly :: Eff Conc m => (SomeException -> Bool) -> Async a -> m ()
linkOnly h a = unliftConc $ \_ -> A.linkOnly h a
{-# INLINE linkOnly #-}

link2 :: Eff Conc m => Async a -> Async b -> m ()
link2 a b = unliftConc $ \_ -> A.link2 a b
{-# INLINE link2 #-}

link2Only :: Eff Conc m => (SomeException -> Bool) -> Async a -> Async b -> m ()
link2Only h a b = unliftConc $ \_ -> A.link2Only h a b
{-# INLINE link2Only #-}

race :: Eff Conc m => m a -> m b -> m (Either a b)
race ma mb = unliftConc $ \lower -> A.race (lower ma) (lower mb)
{-# INLINE race #-}

race_ :: Eff Conc m => m a -> m b -> m ()
race_ ma mb = unliftConc $ \lower -> A.race_ (lower ma) (lower mb)
{-# INLINE race_ #-}

concurrently :: Eff Conc m => m a -> m b -> m (a, b)
concurrently ma mb = unliftConc $ \lower -> A.concurrently (lower ma) (lower mb)
{-# INLINE concurrently #-}

concurrently_ :: Eff Conc m => m a -> m b -> m ()
concurrently_ ma mb = unliftConc $ \lower -> A.concurrently_ (lower ma) (lower mb)
{-# INLINE concurrently_ #-}

mapConcurrently :: (Traversable t, Eff Conc m) => (a -> m b) -> t a -> m (t b)
mapConcurrently = (runConcurrently .) #. traverse .# (Concurrently .)
{-# INLINE mapConcurrently #-}

forConcurrently :: (Traversable t, Eff Conc m) => t a -> (a -> m b) -> m (t b)
forConcurrently = flip mapConcurrently
{-# INLINE forConcurrently #-}

mapConcurrently_ :: (Foldable t, Eff Conc m) => (a -> m b) -> t a -> m ()
mapConcurrently_ f = runConcurrently #. foldMap (Concurrently #. void . f)
{-# INLINE mapConcurrently_ #-}

forConcurrently_ :: (Foldable t, Eff Conc m) => t a -> (a -> m b) -> m ()
forConcurrently_ = flip mapConcurrently_
{-# INLINE forConcurrently_ #-}

replicateConcurrently :: Eff Conc m => Int -> m a -> m [a]
replicateConcurrently cnt = runConcurrently #.  replicateM cnt .# Concurrently
{-# INLINE replicateConcurrently #-}

replicateConcurrently_ :: Eff Conc m => Int -> m a -> m ()
replicateConcurrently_ cnt = runConcurrently #. replicateM_ cnt .# Concurrently
{-# INLINE replicateConcurrently_ #-}

newtype Concurrently m a = Concurrently { runConcurrently :: m a }
  deriving Functor

instance Eff Conc m => Applicative (Concurrently m) where
  pure = Concurrently #. return
  {-# INLINE pure #-}

  Concurrently fs <*> Concurrently as = Concurrently $ unliftConc $ \lower ->
    A.runConcurrently (A.Concurrently (lower fs) <*> A.Concurrently (lower as))
  {-# INLINE (<*>) #-}

instance Eff Conc m => Alternative (Concurrently m) where
  empty = Concurrently $ unliftConc $ \_ -> A.runConcurrently empty
  {-# INLINE empty #-}

  Concurrently as <|> Concurrently bs = Concurrently $ unliftConc $ \lower ->
    A.runConcurrently (A.Concurrently(lower as) <|> A.Concurrently (lower bs))
  {-# INLINE (<|>) #-}

instance (Eff Conc m, Semigroup a) => Semigroup (Concurrently m a) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}

instance (Eff Conc m, Monoid a) => Monoid (Concurrently m a) where
  mempty = pure mempty
  {-# INLINE mempty #-}
