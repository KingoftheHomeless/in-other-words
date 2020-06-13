{-# LANGUAGE FunctionalDependencies, BangPatterns, DerivingVia, UndecidableInstances #-}
module Control.Effect.Internal where

import Data.Coerce

import Control.Monad.Trans
import Control.Effect.Internal.Membership
import Control.Effect.Internal.Union

import Control.Effect.Type.Reader

import Control.Monad.Trans.Except (ExceptT(..))
import qualified Control.Monad.Trans.Except as E

import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.Reader as R

import qualified Control.Monad.Trans.State.Strict as SSt
import qualified Control.Monad.Trans.State.Lazy as LSt
import qualified Control.Monad.Trans.Writer.Lazy as LWr
import qualified Control.Monad.Trans.Writer.Strict as SWr
import qualified Control.Monad.Trans.Writer.CPS as CPSWr

class Monad m => Carrier m where
  type Derivs m :: [Effect]
  type Prims  m :: [Effect]

  -- The type for 'algP' is @'Algebra' ('Prims' m) m@.
  -- That type synonym can't be used directly as it would make
  -- this class impossible to newtype derive due to a GHC bug.
  algP :: Union (Prims m) m a -> m a

  -- The type for 'reformulate' is @'Reformulation' ('Derivs' m) ('Prims' m) m@.
  -- That type synonym can't be used directly as it would make
  -- this class impossible to newtype derive due to a GHC bug.
  reformulate :: Monad z
              => (forall x. m x -> z x)
              -> Algebra (Prims m) z
              -> Algebra (Derivs m) z

  -- A type synonym for 'algD''s type is @'Algebra' ('Derivs' m) m@.
  algD :: Union (Derivs m) m a -> m a
  algD = reformulate id algP
  {-# INLINE algD #-}

type Eff e m = (Member e (Derivs m), Carrier m)
type Effs es m = (Members es (Derivs m), Carrier m)

send :: Eff e m => e m a -> m a
send = algD . inj

threadReformulate
  :: (MonadTrans t, Carrier m)
  => Reformulation (e ': Derivs m) (e ': Prims m) (t m)
threadReformulate = liftReformulation (addPrimitive reformulate)

algPNew
  :: forall n e m
   . ( Coercible n m
     , RepresentationalEff e
     , Carrier n
     )
  => (forall x. e n x -> n x)
  -> Algebra (e ': Prims n) m
algPNew h = coerce (powerAlg (algP @n) h)
{-# INLINE algPNew #-}

instance ( Carrier m
         , Threads (Prims m) (ReaderT i)
         ) => Carrier (ReaderT i m) where
  type Derivs (ReaderT i m) = Reader i ': Derivs m
  type Prims (ReaderT i m) = Reader i ': Prims m

  reformulate = threadReformulate

  algP = powerAlg (thread (algP @m)) $ \case
    Ask -> R.ask
    Local f m -> R.local f m
