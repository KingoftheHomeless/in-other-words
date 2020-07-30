module Control.Effect.NonDet
  ( -- * Effects
    NonDet(..)
  , Cull(..)
  , Cut(..)
  , Split(..)
  , Logic

    -- * Actions
  , choose
  , lose
  , fromList

  , cull

  , cutfail
  , cut
  , call

  , split

    -- * Interpretations
  , NonDetC
  , runNonDet

  , CullCutC
  , runCullCut

  , LogicC
  , runLogic

    -- * Threading constraints
  , NonDetThreads
  ) where

import Control.Monad

import Control.Effect
import Control.Effect.Internal.NonDet

import Control.Effect.Type.NonDet
import Control.Effect.Type.Cull
import Control.Effect.Type.Cut
import Control.Effect.Type.Split


-- | A pseudo-effect for connected 'NonDet', 'Cull', 'Cut', and 'Split' effects.
--
  -- @'Logic'@ should only ever be used inside of 'Eff' and 'Effs'
-- constraints. It is not a real effect! See 'Bundle'.
type Logic = Bundle '[NonDet, Cull, Cut, Split]


-- | Introduce new branches stemming from the current one using a list of values.
fromList :: Eff NonDet m => [a] -> m a
fromList = send . FromList

-- | Introduce two new branches stemming from the current one.
choose :: Eff NonDet m => m a -> m a -> m a
choose ma mb = join $ fromList [ma, mb]
{-# INLINE choose #-}

-- | Fail the current branch and backtrack to the nearest use of 'choose'.
lose :: Eff NonDet m => m a
lose = fromList []
{-# INLINE lose #-}

-- | Cull nondeterminism in the argument, limiting the number of branches
-- it may introduce to be at most 1.
--
-- @'cull' (return True `'choose'` return False) == return True@
-- @'cull' ('lose' `'choose'` return False) == return False@
cull :: Eff Cull m => m a -> m a
cull = send . Cull
{-# INLINE cull #-}

-- | Fail the current branch, and prevent backtracking up until the nearest
-- enclosing use of 'call' (if any).
--
-- @'cutfail' `'choose'` m == 'cutfail'@
cutfail :: Eff Cut m => m a
cutfail = send Cutfail
{-# INLINE cutfail #-}

-- | Commit to the current branch: if all branches stemming from the current one
-- fail, then backtracking will be prevented up until the nearest enclosing use of
-- 'call' (if any).
cut :: Effs '[NonDet, Cut] m => m ()
cut = pure () `choose` cutfail
{-# INLINE cut #-}

-- | Delimit the prevention of backtracking from uses of 'cut' and 'cutfail'.
--
-- @'call' 'cutfail' `'choose'` m = m@
call :: Eff Cut m => m a -> m a
call = send . Call
{-# INLINE call #-}

-- | Split a nondeterministic computation into its first result
-- and the rest of the computation, if possible.
--
-- Note that @'split' 'cutfail' == 'cutfail'@. If you don't want that behavior,
-- use @'split' ('call' m)@ instead of @'split' m@.
split :: Eff Split m => m a -> m (Maybe (a, m a))
split = send . Split id
{-# INLINE split #-}
