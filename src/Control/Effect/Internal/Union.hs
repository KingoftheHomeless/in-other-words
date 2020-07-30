{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Union where

import Data.Coerce
import Data.Kind (Constraint)

import Control.Monad.Trans
import Control.Monad.Trans.Reader (ReaderT)

import Control.Effect.Internal.Membership
import Control.Effect.Internal.Utils

type Effect = (* -> *) -> * -> *

-- | An effect for collecting multiple effects into one effect.
--
-- Behind the scenes, 'Union' is the most important effect
-- in the entire library, as the 'Control.Effect.Carrier' class is built
-- around handling 'Union's of effects.
--
-- However, even outside of defining novel 'Control.Effect.Carrier' instances,
-- 'Union' can be useful as an effect in its own right.
-- 'Union' is useful for effect newtypes -- effects defined through creating a
-- newtype over an existing effect.
-- By making a newtype of 'Union', it's possible to wrap multiple effects in one
-- newtype.
--
-- Not to be confused with 'Control.Effect.Bundle'.
-- Unlike 'Control.Effect.Bundle', 'Union' is a proper effect that is given no
-- special treatment by 'Control.Effect.Eff' or 'Control.Effect.Effs'.
data Union (r :: [Effect]) m a where
  Union :: Coercible z m => ElemOf e r -> e z a -> Union r m a

type Algebra r m = forall x. Union r m x -> m x
type Algebra' r m a = Union r m a -> m a

-- | 'RepresentationalEff' is the constraint every effect is expected
-- to satisfy: namely, that any effect @e m a@ is representational in @m@,
-- which -- in practice -- means that no constraints are ever placed upon
-- @m@ within the definion of @e@.
--
-- You don't need to make instances of 'RepresentationalEff'; the compiler
-- will automatically infer if your effect satisfies it.
--
-- 'RepresentationalEff' is not a very serious requirement, and
-- even effects that don't satisfy it can typically be rewritten into
-- equally powerful variants that do.
--
-- If you ever encounter that an effect you've written doesn't satisfy
-- 'RepresentationalEff', please consult [this guide.]
class    ( forall m n x. Coercible m n => Coercible (e m x) (e n x) )
      => RepresentationalEff (e :: Effect)
instance ( forall m n x. Coercible m n => Coercible (e m x) (e n x) )
      => RepresentationalEff (e :: Effect)


decomp :: RepresentationalEff e
       => Union (e ': r) m a
       -> Either (Union r m a) (e m a)
decomp (Union Here e) = Right (coerce e)
decomp (Union (There pr) e) = Left (Union pr e)
{-# INLINE decomp #-}

extract :: RepresentationalEff e
        => Union '[e] m a
        -> e m a
extract (Union Here e) = coerce e
extract (Union (There pr) _) = absurdMember pr
{-# INLINE extract #-}

weaken :: Union r m a -> Union (e ': r) m a
weaken (Union pr e) = Union (There pr) e
{-# INLINE weaken #-}

absurdU :: Union '[] m a -> b
absurdU (Union pr _) = case pr of {}
{-# INLINE absurdU #-}

weakenAlg :: Algebra' (e ': r) m a -> Algebra' r m a
weakenAlg alg u = alg (weaken u)
{-# INLINE weakenAlg #-}

-- | Strengthen an algebra by providing a handler for a new effect @e@.
powerAlg :: forall e r m a
          . RepresentationalEff e
         => Algebra' r m a
         -> (e m a -> m a)
         -> Algebra' (e ': r) m a
powerAlg alg h = powerAlg' alg (h .# coerce)
{-# INLINE powerAlg #-}

powerAlg' :: forall e r m a
           . Algebra' r m a
          -> (forall z. Coercible z m => e z a -> m a)
          -> Algebra' (e ': r) m a
powerAlg' _ h (Union Here e) = h e
powerAlg' alg _ (Union (There pr) e) = alg (Union pr e)
{-# INLINE powerAlg' #-}


-- | Add a primitive effect and corresponding derived effect to a reformulation.
addPrim :: Monad z
        => Reformulation' r p m z a
        -> Reformulation' (e ': r) (e ': p) m z a
addPrim reform n alg = powerAlg' (reform n (weakenAlg alg)) (alg . Union Here)
{-# INLINE addPrim #-}

liftReform
  :: (MonadTrans t, Monad m)
  => Reformulation' r p m z a
  -> Reformulation' r p (t m) z a
liftReform reform n = reform (n . lift)
{-# INLINE liftReform #-}

coerceReform :: Coercible m n
             => Reformulation' r p m z a
             -> Reformulation' r p n z a
coerceReform reform n alg = coerce (reform (n .# coerce) alg)
{-# INLINE coerceReform #-}

weakenReform :: Reformulation' (e ': r) p m z a
             -> Reformulation' r p m z a
weakenReform reform n alg = weakenAlg (reform n alg)
{-# INLINE weakenReform #-}

type Reformulation' r p m z a
  =  (forall x. m x -> z x)
  -> Algebra p z
  -> Algebra' r z a

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

class Threads t p where
  thread :: Monad m
         => Algebra p m
         -> Algebra p (t m)

instance Threads t '[] where
  thread _ = absurdU
  {-# INLINE thread #-}

instance (ThreadsEff e t, Threads t p) => Threads t (e ': p) where
  thread alg = powerAlg (thread (weakenAlg alg)) (threadEff (alg . Union Here))
  {-# INLINE thread #-}

inj :: Member e r => e m a -> Union r m a
inj = Union membership
{-# INLINE inj #-}

class    (forall i. Threads (ReaderT i) p) => ReaderThreads p
instance (forall i. Threads (ReaderT i) p) => ReaderThreads p

coerceEff :: forall n m e a
           . (Coercible n m, RepresentationalEff e)
          => e m a
          -> e n a
coerceEff = coerce
{-# INLINE coerceEff #-}

coerceAlg :: forall n m e a
           . (Coercible n m, RepresentationalEff e)
          => (e m a -> m a)
          -> e n a -> n a
coerceAlg = coerce
{-# INLINE coerceAlg #-}

-- | A pseudo-effect given special treatment by 'Control.Effect.Eff' and 'Control.Effect.Effs'.
--
-- An 'Control.Effect.Eff'/s constraint on @'Bundle' '[eff1, eff2, ... , effn]@ will expand it into
-- membership constraints for @eff1@ through @effn@. For example.
--
-- @
-- 'Control.Effect.Error.Error' e = 'Bundle' '['Control.Effect.Error.Throw' e, 'Control.Effect.Error.Catch' e]
-- @
--
-- so
--
-- @
-- 'Control.Effect.Eff' ('Error' e) m = ('Control.Effect.Carrier' m, 'Control.Effect.Member' ('Control.Effect.Error.Throw' e) ('Control.Effect.Derivs' m), 'Control.Effect.Member' ('Control.Effect.Error.Throw' e) ('Control.Effect.Derivs' m))
-- @
--
-- 'Bundle' should /never/ be used in any other contexts but within 'Control.Effect.Eff' and 'Control.Effect.Effs'.
--
-- Not to be confused with 'Control.Effect.Union.Union', which is a proper effect that combines multiple
-- effects into one.
data Bundle :: [Effect] -> Effect

type family EffMember (e :: Effect) (r :: [Effect]) :: Constraint where
  EffMember (Bundle xs) r = EffMembers xs r
  EffMember e r = Member e r

type family EffMembers (xs :: [Effect]) (r :: [Effect]) :: Constraint where
  EffMembers '[] r = ()
  EffMembers (Bundle as ': xs) r = (EffMembers as r, EffMembers xs r)
  EffMembers (e ': xs) r = (Member e r, EffMembers xs r)
