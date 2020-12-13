{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Select where

import Control.Effect
import Control.Effect.Cont

-- For coercion purposes
import Control.Effect.Carrier
import Control.Effect.Carrier.Internal.Compose
import Control.Effect.Internal.Cont
import Control.Monad.Trans.Free.Church.Alternate

-- | An effect for backtracking search.
newtype Select s m a where
  Select :: (forall r. (a -> m (s, r)) -> m r) -> Select s m a

-- | Perform a search: capture the continuation
-- of the program, so that you may test values of @a@ and observe
-- what corresponding @s@ each value would result in
-- at the end of the program (which may be seen as the evaluation of @a@).
-- When you find a satisfactory @a@, you may return the associated @r@.
--
-- The way higher-order actions interact with the continuation depends
-- on the interpretation of 'Select'. In general, you cannot expect to interact
-- with the continuation in any meaningful way: for example, you should not
-- assume that you will be able to catch an exception thrown at some point in
-- the future of the computation by using 'Control.Effect.Error.catch' on the
-- continuation.
select :: Eff (Select s) m
       => (forall r. (a -> m (s, r)) -> m r) -> m a
select main = send (Select main)
{-# INLINE select #-}

data SelectH r

instance Eff (Shift (s, r)) m
      => Handler (SelectH r) (Select s) m where
  effHandler = \case
    Select main -> shift @(s, r) $ \c ->
      main $ \a -> (\(s,r) -> (s, (s, r))) <$> c a
  {-# INLINEABLE effHandler #-}

newtype SelectC s r m a = SelectC {
    unSelectC ::
        ReinterpretC (SelectH r) (Select s) '[Shift (s, r)]
      ( ShiftC (s, r)
      ( m
      )) a
  } deriving ( Functor, Applicative, Monad
             , MonadFail, MonadIO
             , MonadThrow, MonadCatch
             , MonadBase b
             )
    deriving MonadTrans
    via CompositionBaseT
     '[ ReinterpretC (SelectH r) (Select s) '[Shift (s, r)]
      , ShiftC (s, r)
      ]

deriving instance (Carrier m, Threads (FreeT (ContBase (m (s, r)) (s, r))) (Prims m))
               => Carrier (SelectC s r m)

-- | Run a @'Select' s@ effect by providing an evaluator
-- for the final result of type @a@.
--
--  @'Derivs' ('SelectC' s r m) = 'Select' s ': 'Derivs' m@
--
--  @'Control.Effect.Primitive.Prims'  ('SelectC' s r m) = 'Control.Effect.Primitive.Prims' m@
runSelect :: forall s a m p
           . (Carrier m, Threaders '[ContThreads] m p)
          => (a -> m s)
          -> SelectC s a m a
          -> m a
runSelect c m =
    fmap snd
  $ runShift
  $ (>>= \a -> (\s -> (s, a)) <$> lift (c a))
  $ reinterpretViaHandler
  $ unSelectC
  $ m
{-# INLINE runSelect #-}
