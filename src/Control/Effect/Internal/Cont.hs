{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Cont where

import Data.Coerce

import Control.Monad.Trans
import Control.Monad.Base
import qualified Control.Monad.Fail as Fail

import Control.Effect
import Control.Effect.Carrier

import Control.Effect.Internal.Utils

import qualified Control.Monad.Trans.Cont as C
import Control.Monad.Trans.Free.Church.Alternate

-- | An effect for abortive continuations.
data Cont m a where
  CallCC :: ((forall b. a -> m b) -> m a) -> Cont m a

data ContBase s a where
  Jump    :: s -> ContBase s a
  GetCont :: ContBase s (Either (a -> s) a)


getCont :: ContC s m (Either (a -> s) a)
getCont = ContC $ liftF $ GetCont
{-# INLINE getCont #-}

exit :: s -> ContC s m a
exit = ContC #. liftF . Jump
{-# INLINE exit #-}

newtype ContC s m a = ContC { unContC :: FreeT (ContBase s) m a }
  deriving ( Functor, Applicative, Monad
           , MonadBase b, Fail.MonadFail, MonadIO
           , MonadThrow, MonadCatch
           )
  deriving MonadTrans

instance ( Carrier m
         , Threads (FreeT (ContBase s)) (Prims m)
         )
      => Carrier (ContC s m) where
  type Derivs (ContC s m) = Cont ': Derivs m
  type Prims  (ContC s m) = Prims m

  algPrims = coerce (thread @(FreeT (ContBase s)) (algPrims @m))
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    CallCC main -> n getCont >>= \case
      Left c  -> main (n . exit . c)
      Right a -> return a
  {-# INLINEABLE reformulate #-}


newtype ContFastC s m a = ContFastC { unContFastC :: C.ContT s m a }
  deriving (Functor, Applicative, Monad)
  deriving MonadTrans

-- I don't understand why these have to be standalone, when
-- SelectFastC's instances don't have to be.
deriving instance MonadBase b m
               => MonadBase b (ContFastC s m)
deriving instance MonadIO m
               => MonadIO (ContFastC s m)
deriving instance Fail.MonadFail m
               => Fail.MonadFail (ContFastC s m)

instance ( Carrier m
         , Threads (C.ContT s) (Prims m)
         )
      => Carrier (ContFastC s m) where
  type Derivs (ContFastC s m) = Cont ': Derivs m
  type Prims  (ContFastC s m) = Prims m

  algPrims = coerce (thread @(C.ContT s) (algPrims @m))
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    CallCC main ->
      n (ContFastC $ C.ContT $ \c -> c (Left (c . Right))) >>= \case
        Left c  -> main (\a -> n $ ContFastC $ C.ContT $ \_ -> c a)
        Right a -> return a
  {-# INLINEABLE reformulate #-}

-- | 'ContThreads' accepts the following primitive effects:
--
-- * 'Control.Effect.Regional.Regional' @s@
-- * 'Control.Effect.Optional.Optional' @s@ (when @s@ is a functor)
-- * 'Control.Effect.Type.Unravel.Unravel' @p@
-- * 'Control.Effect.Type.ListenPrim.ListenPrim' @s@ (when @s@ is a 'Monoid')
-- * 'Control.Effect.Type.ReaderPrim.ReaderPrim' @i@
type ContThreads = FreeThreads

-- | 'ContFastThreads' accepts the following primitive effects:
--
-- * 'Control.Effect.Type.ReaderPrim.ReaderPrim' @i@
class    ( forall s. Threads (C.ContT s) p
         ) => ContFastThreads p
instance ( forall s. Threads (C.ContT s) p
         ) => ContFastThreads p
