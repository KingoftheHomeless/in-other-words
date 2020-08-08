{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Carrier.Internal.Compose where

import Control.Applicative
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.Trans
import Control.Monad.Trans.Identity
import Control.Monad.Fix
import Control.Effect.Internal
import Control.Effect.Internal.Derive
import Control.Effect.Internal.Utils
import Control.Monad.Trans.Control

import Unsafe.Coerce

-- | Composition of monad/carrier transformers.
newtype ComposeT t (u :: (* -> *) -> * -> *) m a = ComposeT {
    getComposeT :: t (u m) a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           , Carrier
           )

instance ( MonadTrans t
         , MonadTrans u
         , forall m. Monad m => Monad (u m)
         )
      => MonadTrans (ComposeT t u) where
  lift m = ComposeT (lift (lift m))
  {-# INLINE lift #-}

instance ( MonadTransControl t
         , MonadTransControl u
         , forall m. Monad m => Monad (u m)
         )
      => MonadTransControl (ComposeT t u) where
  type StT (ComposeT t u) a = StT u (StT t a)

  liftWith main = ComposeT $
    liftWith $ \lowerT ->
    liftWith $ \lowerU ->
    main (lowerU . lowerT .# getComposeT)
  {-# INLINE liftWith #-}

  restoreT m = ComposeT (restoreT (restoreT m))
  {-# INLINE restoreT #-}

-- | Composition of a list of carrier transformers.
--
-- This is useful when you have multiple interpretations whose
-- carriers you'd like to treat as one larger object, such that
-- 'lift' lifts past all those carriers.
--
-- For example:
--
-- @
-- data Counter m a where
--   Probe :: Counter m Int
--
-- type CounterC = 'CompositionC'
--   '[ 'Control.Effect.ReinterpretSimpleC' Counter '['Control.Effect.State.State' Int]
--    , 'Control.Effect.State.StateC' Int
--    ]
--
-- runCounter :: ('Control.Effect.Carrier' m, 'Control.Effect.Threaders' '['Control.Effect.State.StateThreads'] m p)
--            => CounterC m a
--            -> m a
-- runCounter =
--    'Control.Effect.State.runState' 0
--  . 'Control.Effect.reinterpretSimple' (\case
--      Probe -> 'Control.Effect.State.state'' (\s -> (s+1,s))
--    )
--  . 'runComposition'
-- @
--
-- Then you have @'lift' :: Monad m => m a -> CounterC m a@
newtype CompositionC ts m a = CompositionC {
    unCompositionC :: CompositionBaseT ts m a
  }

#define DERIVE_COMP_M(ctx)                            \
deriving newtype instance ctx (CompositionBaseT ts m) \
                       => ctx (CompositionC ts m)

#define DERIVE_COMP_T(ctx)                          \
deriving newtype instance ctx (CompositionBaseT ts) \
                       => ctx (CompositionC ts)

DERIVE_COMP_M(Functor)
DERIVE_COMP_M(Applicative)
DERIVE_COMP_M(Monad)
DERIVE_COMP_M(Alternative)
DERIVE_COMP_M(MonadPlus)
DERIVE_COMP_M(MonadFix)
DERIVE_COMP_M(Fail.MonadFail)
DERIVE_COMP_M(MonadIO)
DERIVE_COMP_M(MonadThrow)
DERIVE_COMP_M(MonadCatch)
DERIVE_COMP_M(MonadMask)

-- Yes, this is necessary. Don't ask, I haven't got a clue.
deriving newtype instance (Monad b, MonadBase b (CompositionBaseT ts m))
                       => MonadBase b (CompositionC ts m)
DERIVE_COMP_M(MonadBaseControl b)
DERIVE_COMP_M(Carrier)

DERIVE_COMP_T(MonadTrans)
DERIVE_COMP_T(MonadTransControl)

type family CompositionBaseT' acc ts :: (* -> *) -> * -> * where
  CompositionBaseT' acc '[] = acc
  CompositionBaseT' acc (t ': ts) = CompositionBaseT' (ComposeT acc t) ts

type CompositionBaseT ts = CompositionBaseT' IdentityT ts

type family CompositionBaseM (ts :: [(* -> *) -> * -> *]) (m :: * -> *) where
  CompositionBaseM '[] m = m
  CompositionBaseM (t ': ts) m = t (CompositionBaseM ts m)



-- | Transform @'CompositionC' [t1, t2, ..., tn] m a@ to @t1 (t2 (... (tn m) ...)) a@
runComposition :: CompositionC ts m a
               -> CompositionBaseM ts m a
-- This is a safe use of 'unsafeCoerce'; the two types are always representationally equal,
-- without even needing the transformers in ts to be representational.
-- GHC can only prove that, however, if ts is concrete. We could stick a 'Coercible' constraint,
-- but in order to prove that constraint, both ComposeT and IdentityT needs to be in scope for the
-- user.
-- This seems like too much of a hassle, so unsafeCoerce is used instead.
--
-- TODO: Investigate if the use of unsafeCoerce messes up optimizations.
runComposition = unsafeCoerce
{-# INLINE runComposition #-}
