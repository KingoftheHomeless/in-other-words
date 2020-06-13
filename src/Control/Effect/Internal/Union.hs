{-# LANGUAGE EmptyCase, AllowAmbiguousTypes #-}
module Control.Effect.Internal.Union where

import Data.Coerce

import Control.Monad.Trans
import Control.Monad.Trans.Except (ExceptT(..))
import qualified Control.Monad.Trans.Except as E
import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Strict as SSt
import qualified Control.Monad.Trans.State.Lazy as LSt
import qualified Control.Monad.Trans.Writer.Lazy as LWr
import qualified Control.Monad.Trans.Writer.Strict as SWr
import qualified Control.Monad.Trans.Writer.CPS as CPSWr
import Control.Monad.Trans.Cont (ContT(..))
import qualified Control.Monad.Trans.Cont as C

import Control.Effect.Internal.Membership
import Control.Monad.Trans

type Effect = (* -> *) -> * -> *

data Union (r :: [Effect]) m a where
  Union :: Coercible z m => ElemOf e r -> e z a -> Union r m a

type Algebra r m = forall x. Union r m x -> m x

class (forall m n x. Coercible m n => Coercible (e m x) (e n x))
  => RepresentationalEff (e :: Effect)
instance (forall m n x. Coercible m n => Coercible (e m x) (e n x))
  => RepresentationalEff e

decomp :: RepresentationalEff e
       => Union (e ': r) m a
       -> Either (Union r m a) (e m a)
decomp (Union Here e) = Right (coerce e)
decomp (Union (There pr) e) = Left (Union pr e)

weaken :: Union r m a -> Union (e ': r) m a
weaken (Union pr e) = Union (There pr) e

absurdU :: Union '[] m a -> b
absurdU (Union pr _) = case pr of {}

weakenAlg :: Algebra (e ': r) m -> Algebra r m
weakenAlg alg u = alg (weaken u)

powerAlg :: forall e r m
          . RepresentationalEff e
         => Algebra r m
         -> (forall x. e m x -> m x)
         -> Algebra (e ': r) m
powerAlg alg h u = case decomp u of
  Right e -> h e
  Left g  -> alg g

addPrimitive
  :: Reformulation r p m
  -> Reformulation (e ': r) (e ': p) m
-- Explicitly pattern match on Union to avoid redundant coerces,
-- and to avoid RepresentationalEff constraint.
addPrimitive _ _ alg (Union Here e) =
  alg (Union Here e)
addPrimitive reform n alg (Union (There pr) e) =
  reform n (weakenAlg alg) (Union pr e)

liftReformulation
  :: (MonadTrans t, Monad m)
  => Reformulation r p m
  -> Reformulation r p (t m)
liftReformulation reform n = reform (n . lift)



type Reformulation r p m
  =  forall z
   . Monad z
  => (forall x. m x -> z x)
  -> Algebra p z
  -> Algebra r z

class RepresentationalEff e => ThreadsEff e t where
  threadEff :: Monad m
            => (forall x. e m x -> m x)
            -> e (t m) a
            -> t m a

class Threads p t where
  thread :: Monad m
         => Algebra p m
         -> Algebra p (t m)

instance Threads '[] t where
  thread _ = absurdU

instance (ThreadsEff e t, Threads p t) => Threads (e ': p) t where
  thread alg = powerAlg (thread (weakenAlg alg)) (threadEff (alg . Union Here))

inj :: Member e r => e m a -> Union r m a
inj = Union membership

class    (forall i. Threads p (ReaderT i)) => ReaderThreads p
instance (forall i. Threads p (ReaderT i)) => ReaderThreads p

class    ( forall s. Threads p (LSt.StateT s)
         ) => LazyStateThreads p
instance ( forall s. Threads p (LSt.StateT s)
         ) => LazyStateThreads p

class    ( forall s. Threads p (SSt.StateT s)
         ) => StateThreads p
instance ( forall s. Threads p (SSt.StateT s)
         ) => StateThreads p

class    ( forall s. Threads p (CPSWr.WriterT s)
         ) => WriterThreads p
instance ( forall s. Threads p (CPSWr.WriterT s)
         ) => WriterThreads p

class    ( forall s. Threads p (LWr.WriterT s)
         ) => LazyWriterThreads p
instance ( forall s. Threads p (LWr.WriterT s)
         ) => LazyWriterThreads p

class    ( forall s. Threads p (C.ContT s)
         ) => ContThreads p
instance ( forall s. Threads p (C.ContT s)
         ) => ContThreads p
