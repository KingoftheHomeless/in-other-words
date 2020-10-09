module Control.Effect.Bracket
  ( -- * Effects
    Bracket(..)
  , ExitCase(..)

    -- * Actions
  , generalBracket
  , bracket
  , bracket_
  , bracketOnError
  , onError
  , finally

    -- * Interpretations
  , bracketToIO

  , runBracketLocally

  , ignoreBracket

    -- * Threading utilities
  , threadBracketViaClass

    -- * MonadMask
  , C.MonadMask

    -- * Carriers
  , BracketToIOC
  , BracketLocallyC
  , IgnoreBracketC
  ) where

import Control.Effect
import Control.Effect.Primitive
import Control.Effect.Type.Bracket

import Control.Monad
import Control.Monad.Catch (MonadMask)
import qualified Control.Monad.Catch as C

generalBracket :: Eff Bracket m
               => m a
               -> (a -> ExitCase b -> m c)
               -> (a -> m b)
               -> m (b, c)
generalBracket acquire release use = send (GeneralBracket acquire release use)
{-# INLINE generalBracket #-}

bracket :: Eff Bracket m
        => m a
        -> (a -> m c)
        -> (a -> m b)
        -> m b
bracket acquire release use = do
  (b, _) <- generalBracket acquire (\a _ -> release a) use
  return b
{-# INLINE bracket #-}

bracket_ :: Eff Bracket m
         => m a
         -> m c
         -> m b
         -> m b
bracket_ acquire release use = bracket acquire (const release) (const use)
{-# INLINE bracket_ #-}

bracketOnError :: Eff Bracket m
               => m a
               -> (a -> m c)
               -> (a -> m b)
               -> m b
bracketOnError acquire release use = do
  (b, _) <- generalBracket
              acquire
              (\a -> \case
                ExitCaseSuccess _ -> pure ()
                _ -> void $ release a
              )
              use
  return b
{-# INLINE bracketOnError #-}

onError :: Eff Bracket m => m a -> m b -> m a
onError m h = bracketOnError (pure ()) (const h) (const m)
{-# INLINE onError #-}

finally :: Eff Bracket m => m a -> m b -> m a
finally m h = bracket (pure ()) (const h) (const m)
{-# INLINE finally #-}

data BracketToIOH

instance (Carrier m, MonadMask m)
      => PrimHandler BracketToIOH Bracket m where
  effPrimHandler (GeneralBracket acquire release use) =
    C.generalBracket acquire release use
  {-# INLINEABLE effPrimHandler #-}

type BracketToIOC = InterpretPrimC BracketToIOH Bracket


-- | Run a 'Bracket' by effect that protects against
-- any abortive computation of any effect, as well
-- as any IO exceptions and asynchronous exceptions.
--
-- @'Derivs' ('BracketToIOC' m) = 'Bracket' ': 'Derivs' m@
--
-- @'Prims'  ('BracketToIOC' m) = 'Bracket' ': 'Prims' m@
bracketToIO :: (Carrier m, MonadMask m)
            => BracketToIOC m a
            -> m a
bracketToIO = interpretPrimViaHandler
{-# INLINE bracketToIO #-}

data BracketLocallyH

instance Carrier m => PrimHandler BracketLocallyH Bracket m where
  effPrimHandler (GeneralBracket acquire release use) = do
    a <- acquire
    b <- use a
    c <- release a (ExitCaseSuccess b)
    return (b, c)
  {-# INLINEABLE effPrimHandler #-}

type BracketLocallyC = InterpretPrimC BracketLocallyH Bracket

-- | Run a 'Bracket' effect that protects against
-- any abortive computations of purely local effects
-- -- i.e. effects interpreted before 'runBracketLocally'
-- that are not interpreted in terms of the final monad
-- nor other effects interpreted after 'runBracketLocally'.
--
-- This does /not/ protect against IO exceptions of any kind,
-- including asynchronous exceptions.
--
-- This is more situational compared to 'bracketToIO',
-- but can be useful. For an example, see the [wiki](https://github.com/KingoftheHomeless/in-other-words/wiki/Advanced-topics#bracket).
--
-- @'Derivs' ('BracketLocallyC' m) = 'Bracket' ': 'Derivs' m@
--
-- @'Prims'  ('BracketLocallyC' m) = 'Bracket' ': 'Prims' m@
runBracketLocally :: Carrier m
                  => BracketLocallyC m a
                  -> m a
runBracketLocally = interpretPrimViaHandler
{-# INLINE runBracketLocally #-}


type IgnoreBracketC = InterpretC IgnoreBracketH Bracket

data IgnoreBracketH

instance Carrier m => Handler IgnoreBracketH Bracket m where
  effHandler (GeneralBracket acquire release use) = do
    a <- acquire
    b <- use a
    c <- release a (ExitCaseSuccess b)
    return (b, c)
  {-# INLINEABLE effHandler #-}

-- | Run a 'Bracket' effect by ignoring it, providing no protection at all.
--
-- @'Derivs' ('IgnoreBracketC' m) = 'Bracket' ': 'Derivs' m@
--
-- @'Prims'  ('IgnoreBracketC' m) = 'Prims' m@
ignoreBracket :: Carrier m
              => IgnoreBracketC m a
              -> m a
ignoreBracket = interpretViaHandler
{-# INLINE ignoreBracket #-}
