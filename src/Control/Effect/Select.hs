module Control.Effect.Select
  ( -- * Effect
    Select(..)

    -- * Actions
  , select

    -- * Interpretations
  , SelectC
  , runSelect

  , SelectFastC
  , runSelectFast

    -- * Threading constraints
  , FreeThreads
  , ContThreads
  ) where

import Data.Coerce

import Control.Monad.Trans
import Control.Monad.Base
import qualified Control.Monad.Fail as Fail

import Control.Effect
import Control.Effect.Cont
import Control.Effect.Type.Select

import Control.Effect.Carrier

import Control.Effect.Internal.Utils

import qualified Control.Monad.Trans.Cont as C
import Control.Monad.Trans.Free.Church.Alternate

data SelectBase s t u a where
  GetCont :: SelectBase s t u (Either (a -> t) a)
  Attempt :: t -> SelectBase s t u (s, u)
  Commit  :: u -> SelectBase s t u a

-- | Perform a search: capture the continuation
-- of the program, so that you may test values of @a@ and observe
-- what corresponding @s@ each value would result in
-- at the end of the program (which may be seen as the evaluation of @a@).
-- When you find a satisfactory @a@, you may return associated @r@.
--
-- The way higher-order actions interact with the continuation depends
-- on the interpreter. In general, you cannot expect to interact with the
-- computation in any meaningful way: for example, you should not assume that
-- you will be able to catch an exception thrown at some point in the future of
-- the computation by using `catch` on the computation.
select :: Eff (Select s) m
       => (forall r. (a -> m (s, r)) -> m r) -> m a
select main = send (Select main)
{-# INLINE select #-}

getCont :: SelectC s t u m (Either (a -> t) a)
getCont = SelectC $ liftF $ GetCont
{-# INLINE getCont #-}

attempt :: t -> SelectC s t u m (s, u)
attempt = SelectC #. liftF . Attempt
{-# INLINE attempt #-}

commit :: u -> SelectC s t u m a
commit = SelectC #. liftF . Commit
{-# INLINE commit #-}

newtype SelectC s t u m a = SelectC {
    unSelectC :: FreeT (SelectBase s t u) m a
  }
  deriving ( Functor, Applicative, Monad
           , MonadBase b, Fail.MonadFail, MonadIO
           , MonadThrow, MonadCatch
           )
  deriving MonadTrans

instance ( Carrier m
         , Threads (FreeT (SelectBase s t u)) (Prims m)
         )
      => Carrier (SelectC s t u m) where
  type Derivs (SelectC s t u m) = Select s ': Derivs m
  type Prims  (SelectC s t u m) = Prims m

  algPrims = coerce (thread @(FreeT (SelectBase s t u)) (algPrims @m))
  {-# INLINE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    Select main -> n getCont >>= \case
      Left c  -> main (n . attempt . c) >>= n . commit
      Right a -> return a
  {-# INLINE reformulate #-}

-- | Run a @'Select' s@ effect by providing an evaluator
-- for the final result of type @a@.
runSelect :: forall s m a p
           . ( Carrier m
             , Threaders '[FreeThreads] m p
             )
          => (a -> m s)
          -> SelectC s (m (s, a)) a m a
          -> m a
runSelect eval =
    foldFreeT
      id
      (\c -> \case
        GetCont   -> c $ Left $ \x -> do
          a <- c (Right x)
          (\s -> (s, a)) <$> eval a
        Attempt m -> m >>= c
        Commit a  -> return a
      )
  .# unSelectC
{-# INLINE runSelect #-}


newtype SelectFastC s r m a = SelectFastC {
    unSelectFastC :: C.ContT (s, r) m a
  }
  deriving ( Functor, Applicative, Monad
           , MonadBase b, MonadIO, Fail.MonadFail
           )
  deriving MonadTrans

instance ( Carrier m
         , Threads (C.ContT (s, r)) (Prims m)
         )
      => Carrier (SelectFastC s r m) where
  type Derivs (SelectFastC s r m) = Select s ': Derivs m
  type Prims  (SelectFastC s r m) = Prims m

  algPrims = coerce (thread @(C.ContT (s, r)) (algPrims @m))
  {-# INLINE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    Select main ->
      n (SelectFastC $ C.ContT $ \c -> c (Left (c . Right))) >>= \case
        Left c  -> do
          end <- main (n . lift . fmap (\(s,r) -> (s, pure (s, r))) . c)
          n $ SelectFastC $ C.ContT $ \_ -> end
        Right a -> return a
  {-# INLINE reformulate #-}

-- | Run a @'Select' s@ effect by providing an evaluator
-- for the final result of type @a@.
--
-- Compared to 'runSelect', this is quite a bit faster, but is significantly
-- more restrictive in what interpreters are used after it, since there are
-- very few primitive effects that the carrier for 'runSelectFast' is able to thread.
-- In fact, of all the primitive effects featured in this library, only
-- one satisfies 'ContThreads': namely, 'Control.Effect.Reader.Reader'.
runSelectFast :: forall s m a p
             . ( Carrier m
               , Threaders '[ContThreads] m p
               )
            => (a -> m s)
            -> SelectFastC s a m a
            -> m a
runSelectFast eval m = fmap snd $ C.runContT (unSelectFastC m) $ \a ->
  (\s -> (s, a)) <$> eval a
{-# INLINE runSelectFast #-}
