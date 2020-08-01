{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal where

import Data.Coerce
import Data.Kind (Constraint)

import Data.Functor.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Identity
import Control.Effect.Internal.Membership
import Control.Effect.Internal.Union
import Control.Effect.Internal.Utils
import Control.Effect.Internal.Derive
import Control.Effect.Internal.Itself

-- | The class of effect carriers, and is the underlying mechanism with which
-- effects are implemented.
--
-- Each carrier is able to implement a number of /derived/ effects,
-- and /primitive/ effects. Typically, only the derived effects are relevant
-- for the end user.
--
-- The "Control.Effect#interpretation tools" are typically powerful enough to
-- let you avoid making instances of this class directly. If you need to make
-- your own instance of 'Carrier', import "Control.Effect.Carrier" and consult the
-- [Guide].
class Monad m => Carrier m where
  -- | The derived effects that @m@ carries. Each derived effect is eventually
  -- reformulated into terms of the primitive effects @'Prims' m@ or other
  -- effects in @'Derivs' m@.
  --
  -- In application code, you gain access to effects by placing membership
  -- constraints upon @'Derivs' m@. You can use 'Eff' or 'Effs' for this
  -- purpose.
  --
  -- Although rarely relevant for users, @'Derivs' m@ can also contain effects
  -- that aren't expressed in terms of other effects, as longs as the handler
  -- for the effect can be lifted generically using 'lift'. Such effects don't
  -- need to be part of @'Prims' m@, which is exclusively for primitive effects
  -- whose handlers need special treatment to be lifted.
  --
  -- For example, first order effects such as 'Control.Effect.State.State'
  -- never need to be part of @'Prims' m@. Certain higher-order effects -
  -- such as 'Control.Effect.Cont.Cont' - can also be handled such that they
  -- never need to be primitive.
  type Derivs m :: [Effect]

  -- | The primitive effects that @m@ carries. These are higher-order effects
  -- whose handlers aren't expressed in terms of other effects, and thus need to
  -- be lifted on a carrier-by-carrier basis.
  --
  -- **Never place membership constraints on @'Prims' m@.**
  -- You should only gain access to effects by placing membership constraints
  -- on @'Derivs' m@.
  --
  -- /However/, running interpreters may place other kinds of constraints upon
  -- @'Prims' m@, namely /threading constraints/, marked by the use of
  -- 'Threaders'.
  -- If you want to run such an effect interpreter inside application code, you
  -- have to propagate such threading constraints through your application.
  --
  -- @'Prims' m@ should only contain higher-order effects that can't be lifted
  -- generically using 'lift'. Any other effects can be placed in @'Derivs' m@.
  type Prims  m :: [Effect]

  -- | An @m@-based 'Algebra' (i.e effect handler) over the union
  -- of the primitive effects:
  -- effects that aren't formulated in terms of other effects.
  -- See 'Prims'.
  algPrims :: Algebra' (Prims m) m a

  -- | Any 'Carrier' @m@ must provide a way to describe the derived effects it
  -- carries in terms of the primitive effects.
  -- 'reformulate' is that decription: given any monad @z@ such that
  -- @z@ lifts @m@, then a @z@-based 'Algebra' (i.e. effect handler)
  -- over the derived effects can be created out of a @z@-based 'Algebra' over
  -- the primitive effects.
  reformulate :: Monad z
              => Reformulation' (Derivs m) (Prims m) m z a

  -- An @m@-based algebra (i.e. effect handler) over the union of derived
  -- effects (see @'Derivs' m@).
  --
  -- This is what 'send' makes use of.
  --
  -- 'algDerivs' is subject to the law:
  --
  -- @
  -- algDerivs = 'reformulate' id 'algPrims'
  -- @
  --
  -- which serves as the default implementation.
  algDerivs :: Algebra' (Derivs m) m a
  algDerivs = reformulate id algPrims
  {-# INLINE algDerivs #-}

-- | (Morally) a type synonym for
-- @('Member' e ('Derivs' m), 'Carrier' m).
-- This and 'Effs' are the typical methods to gain
-- access to effects.
--
-- Unlike 'Member', 'Eff' gives 'Bundle' special treatment.
-- As a side-effect, 'Eff' will get stuck if @e@ is a type variable.
--
-- If you need access to some completetely polymorphic effect @e@,
-- use @('Member' e ('Derivs' m), 'Carrier' m)@ instead of @Eff e m@.
type Eff e m = Effs '[e] m

-- | A variant of 'Eff' that takes a list of effects, and expands them into
-- multiple 'Member' constraints on @'Derivs' m@.
-- This and 'Eff' are the typical methods to gain access to effects.
--
-- Like 'Eff', 'Effs' gives 'Bundle' special treatment.
-- As a side-effect, 'Effs' will get stuck if any element of the list
-- is a type variable.
--
-- If you need access to some completetely polymorphic effect @e@,
-- use a separate @'Member' e ('Derivs' m)@ constraint.
type Effs es m = (EffMembers es (Derivs m), Carrier m)


-- | Perform an action of an effect.
--
-- 'send' should be used to create actions of your own effects.
-- For example:
--
-- @
-- data CheckString :: Effect where
--   CheckString :: String -> CheckString m Bool
--
-- checkString :: Eff CheckString m => String -> m Bool
-- checkString str = send (CheckString str)
-- @
--
send :: (Member e (Derivs m), Carrier m) => e m a -> m a
send = algDerivs . inj
{-# INLINE send #-}

deriving via (m :: * -> *) instance Carrier m => Carrier (IdentityT m)

-- | A constraint that @'Prims' m@ satisfies all the constraints in the list
-- @cs@.
--
-- This is used for /threading constraints/.
--
-- Each interpreter that use an underlying
-- non-trivial monad transformer -- such as 'Control.Effect.State.runState',
-- which uses 'Control.Monad.Trans.State.Strict.StateT' internally --
-- must be able to
-- lift all primitive effect handlers of the transformed carrier so that
-- transformed monad can also handle the primitive effects.
--
-- The ability of a monad transformer to lift handlers of a particular
-- primitive effect is called /threading/ that effect. /Threading constraints/
-- correspond to the requirement that the primitive effects of the transformed
-- monad can be thread by the monad transformer.
--
-- For example, the 'Control.Effect.State.runState' places the threading
-- constraint @StateThreads@ on @'Prims' m@.
-- If you want to use interpreters that generate threading constraints
-- inside of application code, then those constraints need to be propagated
-- throughout the application.
--
-- 'Threaders' is used for this purpose. For example,
-- @'Threaders' '['Control.Effect.State.StateThreads', 'Control.Effect.Error.ExceptThreads'] m p@
-- allows you to use 'Control.Effect.State.runState' and
-- 'Control.Effect.Error.runError' with the carrier @m@ inside application code.
--
-- The third argument @p@ should always be a polymorphic type variable, which
-- you can simply provide and ignore.
-- It exists as a work-around to the fact that many threading constraints
-- /don't actually work/ if they operate on @'Prims' m@ directly, since
-- threading constraints often involve quantified constraints, which are fragile
-- in combination with type families -- like 'Prims'.
--
-- So @'Threaders' '[Control.Effect.State.StateThreads]' m p@
-- doesn't expand to @Control.Effect.State.StateThreads (Prims m)@, but rather,
-- @(p ~ Prims m, Control.Effect.State.StateThreads p)@
type Threaders cs m p = (p ~ Prims m, SatisfiesAll p cs)

type family SatisfiesAll (q :: k) cs :: Constraint where
  SatisfiesAll q '[] = ()
  SatisfiesAll q (c ': cs) = (c q, SatisfiesAll q cs)

-- | The identity carrier, which carries no effects at all.
type RunC = Identity

-- | Extract the final result from a computation of which no effects remain
-- to be handled.
run :: RunC a -> a
run = runIdentity
{-# INLINE run #-}

instance Carrier Identity where
  type Derivs Identity = '[]
  type Prims  Identity = '[]

  algPrims = absurdU
  {-# INLINE algPrims #-}

  reformulate _ _ = absurdU
  {-# INLINE reformulate #-}

  algDerivs = absurdU
  {-# INLINE algDerivs #-}

deriving newtype instance Carrier m => Carrier (Itself m)

newtype SubsumeC (e :: Effect) m a = SubsumeC {
    unSubsumeC :: m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
       via m
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance ( Carrier m
         , Member e (Derivs m)
         )
      => Carrier (SubsumeC e m) where
  type Derivs (SubsumeC e m) = e ': Derivs m
  type Prims  (SubsumeC e m) = Prims m

  algPrims = coerce (algPrims @m)
  {-# INLINE algPrims #-}

  reformulate n alg = powerAlg' (reformulate (n .# SubsumeC) alg) $ \e ->
    reformulate (n .# SubsumeC) alg (Union membership e)
  {-# INLINE reformulate #-}

  algDerivs = powerAlg' (coerce (algDerivs @m)) $ \e ->
    coerceAlg (algDerivs @m) (Union membership e)
  {-# INLINE algDerivs #-}

-- | Interpret an effect in terms of another, identical effect.
--
-- This is very rarely useful, but one use-case is to transform
-- reinterpreters into regular interpreters.
--
-- For example,
-- @'subsume' . 'reinterpretSimple' h@ is morally equivalent
-- to @'interpretSimple' h@
subsume :: ( Carrier m
           , Member e (Derivs m)
           )
        => SubsumeC e m a
        -> m a
subsume = unSubsumeC
{-# INLINE subsume #-}
