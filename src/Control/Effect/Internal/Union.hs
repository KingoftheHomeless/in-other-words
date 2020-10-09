{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Union where

import Data.Coerce
import Data.Kind (Constraint)

import Control.Monad.Trans
import Control.Monad.Trans.Reader (ReaderT)

import Control.Effect.Internal.Membership
import Control.Effect.Internal.Utils

-- | The kind of effects.
--
-- Helpful for defining new effects:
--
-- @
-- data InOut i o :: Effect where
--   Input  :: InOut i o m i
--   Output :: o -> InOut i o m ()
-- @
--
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

-- | An @'Algebra' r m@ desribes a collection of effect handlers for @m@ over
-- all effects in the list @r@.
type Algebra r m = forall x. Union r m x -> m x

-- | A first-rank type which can often be used instead of 'Algebra'
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
-- 'RepresentationalEff', please consult
-- [the wiki](https://github.com/KingoftheHomeless/in-other-words/wiki/Advanced-topics#making-effects-representationaleff).
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

-- | Extract the only effect of an 'Union'.
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


-- | Weaken an 'Algebra' by removing the topmost effect.
weakenAlg :: Algebra' (e ': r) m a -> Algebra' r m a
weakenAlg alg u = alg (weaken u)
{-# INLINE weakenAlg #-}

-- | Strengthen an 'Algebra' by providing a handler for a new effect @e@.
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
{-# INLINEABLE powerAlg' #-}


-- | Add a primitive effect and corresponding derived effect to a 'Reformulation'.
addPrim :: forall e r p m z a
         . Monad z
        => Reformulation' r p m z a
        -> Reformulation' (e ': r) (e ': p) m z a
addPrim reform n alg = powerAlg' (reform n (weakenAlg alg)) (alg . Union Here)
{-# INLINE addPrim #-}

-- | Lift an @m@-based 'Reformulation' to a @t m@-based 'Reformulation',
-- where @t@ is any 'MonadTrans'
liftReform
  :: (MonadTrans t, Monad m)
  => Reformulation' r p m z a
  -> Reformulation' r p (t m) z a
liftReform reform = \n -> reform (n . lift)
{-# INLINE liftReform #-}

coerceReform :: Coercible m n
             => Reformulation' r p m z a
             -> Reformulation' r p n z a
coerceReform reform n alg = coerce (reform (n .# coerce) alg)
{-# INLINE coerceReform #-}

-- | Weaken a 'Reformulation' by removing the topmost
-- derived effect.
weakenReform :: Reformulation' (e ': r) p m z a
             -> Reformulation' r p m z a
weakenReform reform n alg = weakenAlg (reform n alg)
{-# INLINE weakenReform #-}

-- | A /less/ higher-rank variant of 'Reformulation', which is sometimes
-- important.
type Reformulation' r p m z a
  =  (forall x. m x -> z x)
  -> Algebra p z
  -> Algebra' r z a

-- | The type of 'Control.Effect.Carrier.reformulate'.
--
-- A @'Reformulation' r p m@ describes how the derived effects @r@ are
-- formulated in terms of the primitive effects @p@ and first-order operations
-- of @m@.
-- This is done by providing an @'Algebra' r z@ for any monad @z@ that lifts
-- @m@ and implements an 'Algebra' over @p@.
type Reformulation r p m
  =  forall z
   . Monad z
  => (forall x. m x -> z x)
  -> Algebra p z
  -> Algebra r z

-- | An instance of 'ThreadsEff' represents the ability for a monad transformer
-- @t@ to thread a primitive effect @e@ -- i.e. lift handlers of that effect.
--
-- Instances of 'ThreadsEff' are accumulated into entire stacks of primitive
-- effects by 'Threads'.
--
-- You only need to make 'ThreadsEff' instances for monad transformers that
-- aren't simply newtypes over existing monad transformers. You also don't need
-- to make them for 'Control.Monad.Trans.Identity.IdentityT'.
class RepresentationalEff e => ThreadsEff t e where
  threadEff :: Monad m
            => (forall x. e m x -> m x)
            -> e (t m) a
            -> t m a

-- | @'Threads' t p@ is satisfied if @ThreadsEff t e@ instances are defined for
-- each effect @e@ in @p@. By using the @'Threads' t p@ constraint, you're
-- able to lift 'Algebra's over p from any monad @m@ to @t m@. This is useful
-- when defining custom 'Control.Effect.Carrier.Carrier' instances.
--
-- Note that you /should not/ place a @'Threads' t p@ constraint if @t@ is
-- simply a newtype over an existsing monad transformer @u@ that already has
-- 'ThreadsEff' instances defined for it. Instead, you should place a
-- @'Threads' u p@ constraint, and use its 'thread' by coercing the resulting
-- algebra from @'Algrebra' p (t m)@ to @'Algebra' p (u m)@'.
-- That way, you avoid having to define redundant 'ThreadsEff' instances for
-- every newtype of a monad transformer.
--
-- 'Threads' forms the basis of /threading constraints/
-- (see 'Control.Effect.Threaders'), and every threading constraint offered
-- in the library makes use of 'Threads' in one way or another.
class Threads t p where
  thread :: Monad m
         => Algebra p m
         -> Algebra p (t m)

instance Threads t '[] where
  thread _ = absurdU
  {-# INLINE thread #-}

instance (ThreadsEff t e, Threads t p) => Threads t (e ': p) where
  thread alg = powerAlg (thread (weakenAlg alg)) (threadEff (alg . Union Here))
  {-# INLINEABLE thread #-}

-- | Inject an effect into a 'Union' containing that effect.
inj :: Member e r => e m a -> Union r m a
inj = Union membership
{-# INLINE inj #-}

-- | The most common threading constraint of the library, as it is emitted by
-- @-Simple@ interpreters (interpreters that internally make use of
-- 'Control.Effect.interpretSimple' or 'Control.Effect.reinterpretSimple').
--
-- 'ReaderThreads' accepts all the primitive effects
-- (intended to be used as such) offered in this library.
--
-- Most notably, 'ReaderThreads' accepts @'Control.Effect.Unlift.Unlift' b@.
class    (forall i. Threads (ReaderT i) p) => ReaderThreads p
instance (forall i. Threads (ReaderT i) p) => ReaderThreads p

coerceEff :: forall n m e a
           . (Coercible n m, RepresentationalEff e)
          => e m a
          -> e n a
coerceEff = coerce
{-# INLINE coerceEff #-}

coerceAlg :: forall n m e a b
           . (Coercible n m, RepresentationalEff e)
          => (e m a -> m b)
          -> e n a -> n b
coerceAlg = coerce
{-# INLINE coerceAlg #-}

-- | A pseudo-effect given special treatment by 'Control.Effect.Eff'
-- and 'Control.Effect.Effs'.
--
-- An @'Control.Effect.Eff'/s@ constraint on
-- @'Bundle' '[eff1, eff2, ... , effn]@
-- will expand it into membership constraints for @eff1@ through @effn@.
-- For example:
--
-- @
-- 'Control.Effect.Error.Error' e = 'Bundle' '['Control.Effect.Error.Throw' e, 'Control.Effect.Error.Catch' e]
-- @
--
-- so
--
-- @
-- 'Control.Effect.Eff' ('Control.Effect.Error.Error' e) m = ('Control.Effect.Carrier' m, 'Control.Effect.Member' ('Control.Effect.Error.Throw' e) ('Control.Effect.Derivs' m), 'Control.Effect.Member' ('Control.Effect.Error.Catch' e) ('Control.Effect.Derivs' m))
-- @
--
-- 'Bundle' should /never/ be used in any other contexts but within
-- 'Control.Effect.Eff' and 'Control.Effect.Effs', as it isn't an actual effect.
--
-- Not to be confused with 'Control.Effect.Union.Union', which is a proper
-- effect that combines multiple effects into one.
data Bundle :: [Effect] -> Effect

type family Append l r where
  Append '[] r = r
  Append (x ': l) r = x ': (Append l r)

type family FlattenBundles (e :: [Effect]) :: [Effect] where
  FlattenBundles '[] = '[]
  FlattenBundles (Bundle bs ': es) = Append (FlattenBundles bs) (FlattenBundles es)
  FlattenBundles (e ': es) = e ': FlattenBundles es

type family Members (es :: [Effect]) (r :: [Effect]) :: Constraint where
  Members '[] r = ()
  Members (e ': es) r = (Member e r, Members es r)

type EffMembers (xs :: [Effect]) (r :: [Effect]) = Members (FlattenBundles xs) r
