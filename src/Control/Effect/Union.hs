{-# LANGUAGE DerivingVia #-}
module Control.Effect.Union
  ( -- * Effects
    Union(..)
  , ElemOf(..)

    -- * Actions
  , unionize

  , unionizeHead

    -- * Interpretations
  , runUnion

    -- * Utilities
  , inj
  , decomp
  , extract
  , weaken
  , absurdU
  , absurdMember
  , membership

    -- * Carriers
  , UnionizeC
  , UnionizeHeadC
  , UnionC
  ) where

import Data.Coerce

import Control.Effect

import Control.Effect.Internal
import Control.Effect.Internal.Utils
import Control.Effect.Internal.Union
import Control.Effect.Internal.Derive
import Control.Effect.Internal.Membership
import Control.Effect.Internal.KnownList

-- For coercion purposes
import Control.Monad.Trans.Identity
import Control.Effect.Carrier.Internal.Compose
import Control.Effect.Carrier.Internal.Intro


newtype UnionC (l :: [Effect]) m a = UnionC { unUnionC :: m a }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance ( KnownList l
         , HeadEffs l m
         )
      => Carrier (UnionC l m) where
  type Derivs (UnionC l m) = Union l ': StripPrefix l (Derivs m)
  type Prims  (UnionC l m) = Prims m

  algPrims = coerce (algPrims @m)
  {-# INLINE algPrims #-}

  reformulate (n :: forall x. UnionC l m x -> z x) alg =
    let
      algDerivs' :: Algebra (Derivs m) z
      algDerivs' = reformulate @m (n .# UnionC) alg
    in
      powerAlg (weakenAlgN (singList @l) algDerivs') $ \(Union pr e) ->
        algDerivs' (Union (lengthenMembership @(StripPrefix l (Derivs m)) pr) e)
  {-# INLINE reformulate #-}

  algDerivs =
    let
      algD' :: Algebra (Derivs m) (UnionC l m)
      algD' = coerce (algDerivs @m)
    in
      powerAlg (weakenAlgN (singList @l) algD') $ \(Union pr e) ->
        algD' (Union (lengthenMembership @(StripPrefix l (Derivs m)) pr) e)
  {-# INLINE algDerivs #-}

-- | Run an @'Union' b@ effect by placing the effects of @b@ on top of the
-- effect stack.
--
-- @'Derivs' ('UnionC' b m) = Union b ': 'StripPrefix' b ('Derivs' m)@
--
-- @'Prims'  ('UnionC' b m) = 'Prims' m@
runUnion :: forall b m a
          . ( HeadEffs b m
            , KnownList b
            )
         => UnionC b m a
         -> m a
runUnion = unUnionC
{-# INLINE runUnion #-}

-- | Sends uses of effects in @b@ to a @'Union' b@ effect.
--
-- @'Derivs' (UnionizeC b m) = b ++ 'Derivs' m@
unionize :: ( Eff (Union b) m
            , KnownList b
            )
         => UnionizeC b m a
         -> m a
unionize = unUnionizeC
{-# INLINE unionize #-}

-- | Rewrite uses of effects in @b@ into a @'Union' b@ effect on top of the effect stack.
--
-- @'Derivs' (UnionizeC b m) = b ++ StripPrefix '['Union' b] 'Derivs' m@
type UnionizeHeadC b = CompositionC
 '[ IntroC b '[Union b]
  , UnionizeC b
  ]


unionizeHead :: ( HeadEff (Union b) m
                , KnownList b
                )
             => UnionizeHeadC b m a
             -> m a
unionizeHead = coerce
{-# INLINE unionizeHead #-}


newtype UnionizeC b m a = UnionizeC { unUnionizeC :: m a }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b', MonadBaseControl b'
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance ( KnownList b
         , Eff (Union b) m
         )
      => Carrier (UnionizeC b m) where
  type Derivs (UnionizeC b m) = Append b (Derivs m)
  type Prims  (UnionizeC b m) = Prims m

  algPrims = coerce (algPrims @m)
  {-# INLINE algPrims #-}

  reformulate n alg (Union pr e) =
    case splitMembership @(Derivs m) (singList @b) pr of
      Left pr'  -> reformulate (n .# UnionizeC) alg $ inj (Union pr' e)
      Right pr' -> reformulate (n .# UnionizeC) alg (Union pr' e)
  {-# INLINE reformulate #-}

  algDerivs (Union pr e) =
    case splitMembership @(Derivs m) (singList @b) pr of
      Left pr'  -> UnionizeC $ algDerivs @m $ inj (Union pr' e)
      Right pr' -> UnionizeC $ algDerivs @m (Union pr' e)
  {-# INLINE algDerivs #-}
