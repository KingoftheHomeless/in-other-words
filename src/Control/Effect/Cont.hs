module Control.Effect.Cont where

import Data.Functor.Identity
import Data.Coerce
import Data.Void

import Control.Monad.Trans
import Control.Applicative
import Control.Monad
import qualified Control.Monad.Fail as Fail

import Control.Effect
import Control.Effect.Error
import Control.Monad.Base
import Control.Monad.Fix

import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.Cont as C

data Cont :: Effect where
  CallCC :: ((forall b. a -> m b) -> m a) -> Cont m a

callCC :: Eff Cont m
       => ((forall b. a -> m b) -> m a) -> m a
callCC main = send (CallCC main)

newtype ContCTrans s m = ContCTrans (forall x. m x -> (x -> s) -> s)

newtype ContC (s :: *) m a = ContC {
    unContC :: (forall x. m x -> (x -> s) -> s)
            -> (a -> s) -> s
  }
  deriving (Functor, Applicative, Monad) via R.ReaderT (ContCTrans s m) (C.Cont s)

instance MonadTrans (ContC s) where
  lift m = ContC $ \bind c -> m `bind` c

instance ( Carrier m
         , Threads (Prims m) (ContC s)
         ) => Carrier (ContC s m) where
  type Prims (ContC s m) = Prims m
  type Derivs (ContC s m) = Cont ': Derivs m

  algP = thread (algP @m)

  reformulate n alg = powerAlg (reformulate @m (n . lift) alg) $ \case
    CallCC main ->
      n (ContC $ \_ c -> c $ Left (c . Right)) >>= \case
        Left c  -> main (\a -> n $ ContC $ \_ _ -> c a)
        Right a -> return a

runCont :: Monad m => ContC (m a) m a -> m a
runCont m = unContC m (>>=) pure

instance Alternative m => Alternative (ContC s m) where
  empty = ContC $ \bind _ -> empty `bind` id
  ma <|> mb = ContC $ \bind c ->
    unContC ma
      (\mx c' -> (`bind` id) $
        fmap c' mx <|> pure (unContC mb bind c)
      )
      c

instance ThreadsEff (Error e) (ContC s) where
  threadEff alg = \case
    Throw e -> lift $ alg $ Throw e
    Catch m h -> ContC $ \bind c ->
      unContC m
        (\mx c' -> (`bind` id) $
          alg $ fmap c' mx `Catch` \e -> pure (unContC (h e) bind c)
        )
        c

{-

data Split m a where
  Split :: m a -> Split m (Maybe (a, m a))

data Split m a where
  Split :: m a -> (Maybe (a, m a) -> b) -> Split m b

-}
