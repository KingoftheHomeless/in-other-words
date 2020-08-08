{-# LANGUAGE BlockArguments, DerivingVia #-}
module Control.Effect.Writer
  ( -- * Effects
    Tell(..)
  , Listen(..)
  , Pass(..)
  , Writer

  -- * Actions
  , tell
  , listen
  , pass
  , censor

  -- * Interpretations for 'Tell'
  , runTell

  , runTellLazy

  , runTellList

  , runTellListLazy

  , tellToIO
  , runTellIORef
  , runTellTVar

  , tellIntoEndoTell

  , tellToTell
  , tellIntoTell

  -- * Simple variants of interpretations for 'Tell'
  , tellToIOSimple
  , runTellIORefSimple
  , runTellTVarSimple

  , tellToTellSimple
  , tellIntoTellSimple

  -- * Interpretations for 'Tell' + 'Listen'
  , runListen

  , runListenLazy

  , listenToIO
  , runListenTVar

  , listenIntoEndoListen

  -- * Interpretations for 'Writer' ('Tell' + 'Listen' + 'Pass')
  , runWriter

  , runWriterLazy

  , writerToIO
  , runWriterTVar

  , writerToBracket
  , writerToBracketTVar

  , writerIntoEndoWriter

    -- * Other utilities
  , fromEndoWriter

    -- * Threading constraints
  , WriterThreads
  , WriterLazyThreads

    -- * Threading utilities
  , threadListenViaClass
  , threadPassViaClass

    -- * Carriers
  , TellC
  , TellLazyC
  , TellListC
  , TellListLazyC
  , TellIntoEndoTellC
  , ListenC
  , ListenLazyC
  , ListenTVarC
  , ListenIntoEndoListenC
  , WriterC
  , WriterLazyC
  , WriterTVarC
  , WriterToBracketC
  , WriterIntoEndoWriterC
  ) where

import Data.Bifunctor
import Data.Semigroup
import Data.Tuple (swap)
import Data.IORef

import Control.Concurrent.STM

import Control.Monad

import Control.Effect
import Control.Effect.Reader
import Control.Effect.Bracket
import Control.Effect.Type.Listen
import Control.Effect.Type.Pass

import Control.Effect.Carrier
import Control.Effect.Internal.Writer

import qualified Control.Monad.Catch as C

import qualified Control.Monad.Trans.Writer.CPS as W
import qualified Control.Monad.Trans.Writer.Lazy as LW

-- For coercion purposes
import Control.Effect.Internal.Utils
import Control.Effect.Carrier.Internal.Interpret
import Control.Effect.Carrier.Internal.Compose
import Control.Effect.Carrier.Internal.Intro
import Control.Monad.Trans.Identity

-- | A pseudo-effect for connected @'Tell' s@, @'Listen' s@ and @'Pass' s@ effects.
--
-- @'Writer'@ should only ever be used inside of 'Eff' and 'Effs'
-- constraints. It is not a real effect! See 'Bundle'.
type Writer s = Bundle '[Tell s, Listen s, Pass s]

tell :: Eff (Tell s) m => s -> m ()
tell = send . Tell
{-# INLINE tell #-}

listen :: Eff (Listen s) m => m a -> m (s, a)
listen = send . Listen
{-# INLINE listen #-}

pass :: Eff (Pass s) m => m (s -> s, a) -> m a
pass = send . Pass
{-# INLINE pass #-}

censor :: Eff (Pass s) m => (s -> s) -> m a -> m a
censor f = pass . fmap ((,) f)
{-# INLINE censor #-}


data TellListH

type TellListC s = CompositionC
 '[ ReinterpretC TellListH (Tell s) '[Tell (Dual [s])]
  , TellC (Dual [s])
  ]

instance Eff (Tell (Dual [s])) m
      => Handler TellListH (Tell s) m where
  effHandler (Tell s) = tell (Dual [s])
  {-# INLINE effHandler #-}

-- | Run a @'Tell' s@ by gathering the 'tell's into a list.
--
-- The resulting list is produced strictly. See 'runTellListLazy' for a lazy
-- variant.
runTellList :: forall s m a p
             . ( Carrier m
               , Threaders '[WriterThreads] m p
               )
            => TellListC s m a
            -> m ([s], a)
runTellList =
     (fmap . first) (reverse .# getDual)
  .  runTell
  .# reinterpretViaHandler
  .# runComposition
{-# INLINE runTellList #-}

data TellListLazyH

type TellListLazyC s = CompositionC
 '[ ReinterpretC TellListLazyH (Tell s) '[Tell (Endo [s])]
  , TellLazyC (Endo [s])
  ]

instance Eff (Tell (Endo [s])) m
      => Handler TellListLazyH (Tell s) m where
  effHandler (Tell s) = tell (Endo (s:))
  {-# INLINE effHandler #-}

-- | Run a @'Tell' s@ by gathering the 'tell's into a list.
--
-- The resulting list is produced lazily.
runTellListLazy :: forall s m a p
                 . ( Carrier m
                   , Threaders '[WriterLazyThreads] m p
                   )
                => TellListLazyC s m a
                -> m ([s], a)
runTellListLazy =
     fromEndoWriter
  .  runTellLazy
  .# reinterpretViaHandler
  .# runComposition
{-# INLINE runTellListLazy #-}


-- | Run a @'Tell' s@ effect, where @s@ is a 'Monoid', by accumulating
-- all the uses of 'tell'.
--
-- Unlike 'runListen' and 'runWriter', this does not provide the ability to
-- interact with the 'tell's through 'listen' and 'pass'; but also doesn't
-- impose any primitive effects, meaning 'runTell' doesn't restrict what
-- interpreters are run before it.
--
-- @'Derivs' ('TellC' s m) = 'Tell' s ': 'Derivs' m@
--
-- @'Prims'  ('TellC' s m) = 'Prims' m@
--
-- This produces the final accumulation @s@ strictly. See 'runTellLazy' for a
-- lazy variant of this.
runTell :: forall s m a p
         . ( Monoid s
           , Carrier m
           , Threaders '[WriterThreads] m p
           )
        => TellC s m a
        -> m (s, a)
runTell (TellC m) = do
  (a, s) <- W.runWriterT m
  return (s, a)
{-# INLINE runTell #-}

-- | Run connected @'Listen' s@ and @'Tell' s@ effects, where @s@ is a 'Monoid',
-- by accumulating all the uses of 'tell'.
--
-- Unlike 'runWriter', this does not provide the power of 'pass'; but because
-- of that, it also doesn't impose 'Pass' as a primitive effect, meaning
-- a larger variety of interpreters may be run before 'runListen' compared to
-- 'runWriter'.
--
-- @'Derivs' ('ListenC' s m) = 'Listen' s ': 'Tell' s ': 'Derivs' m@
--
-- @'Prims'  ('ListenC' s m) = 'Listen' s ': 'Prims' m@
--
-- This produces the final accumulation strictly. See 'runListenLazy' for a
-- lazy variant of this.
runListen :: forall s m a p
           . ( Monoid s
             , Carrier m
             , Threaders '[WriterThreads] m p
             )
          => ListenC s m a
          -> m (s, a)
runListen (ListenC m) = do
  (a, s) <- W.runWriterT m
  return (s, a)
{-# INLINE runListen #-}

-- | Run connected @'Pass' s@, @'Listen' s@ and @'Tell' s@ effects,
-- -- i.e. @'Writer' s@ -- where @s@ is a 'Monoid', by accumulating all the
-- uses of 'tell'.
--
-- @'Pass' s@ is a fairly restrictive primitive effect. Notably,
-- 'Control.Effect.Cont.runCont' can't be used before 'runWriter'.
-- If you don't need 'pass', consider using 'runTell' or 'runListen' instead.
--
-- @'Derivs' ('WriterC' s m) = 'Pass' s ': 'Listen' s ': 'Tell' s ': 'Derivs' m@
--
-- @'Prims'  ('WriterC' s m) = 'Pass' s ': 'Listen' s ': 'Prims' m@
--
-- This produces the final accumulation strictly. See 'runWriterLazy' for a
-- lazy variant of this.
runWriter :: forall s m a p
           . ( Monoid s
             , Carrier m
             , Threaders '[WriterThreads] m p
             )
          => WriterC s m a
          -> m (s, a)
runWriter (WriterC m) = do
  (a, s) <- W.runWriterT m
  return (s, a)
{-# INLINE runWriter #-}


-- | Run a @'Tell' s@ effect, where @s@ is a 'Monoid', by accumulating all the
-- uses of 'tell' lazily.
--
-- @'Derivs' ('TellLazyC' s m) = 'Tell' s ': 'Derivs' m@
--
-- @'Prims'  ('TellLazyC' s m) = 'Prims' m@
--
-- This is a variant of 'runTell'. that produces the final accumulation
-- lazily. __Use this only if you need__
-- __the laziness, as this would otherwise incur an unneccesary space leak.__
runTellLazy :: forall s m a p
         . ( Monoid s
           , Carrier m
           , Threaders '[WriterLazyThreads] m p
           )
        => TellLazyC s m a
        -> m (s, a)
runTellLazy (TellLazyC m) = swap <$> LW.runWriterT m
{-# INLINE runTellLazy #-}

-- | Run connected @'Listen' s@ and @'Tell' s@ effects,
-- where @s@ is a 'Monoid', by accumulating all the uses of 'tell' lazily.
--
-- @'Derivs' ('ListenLazyC' s m) = 'Listen' s ': 'Tell' s ': 'Derivs' m@
--
-- @'Prims'  ('ListenLazyC' s m) = 'Listen' s ': 'Prims' m@
--
-- This is a variant of 'runListen' that produces the
-- final accumulation lazily. __Use this only if you need__
-- __the laziness, as this would otherwise incur an unneccesary space leak.__
runListenLazy :: forall s m a p
           . ( Monoid s
             , Carrier m
             , Threaders '[WriterThreads] m p
             )
          => ListenLazyC s m a
          -> m (s, a)
runListenLazy (ListenLazyC m) = swap <$> LW.runWriterT m
{-# INLINE runListenLazy #-}

-- | Run connected @'Pass' s@, @'Listen' s@ and @'Tell' s@ effects,
-- -- i.e. @'Writer' s@ -- where @s@ is a 'Monoid',
-- by accumulating all the uses of 'tell' lazily.
--
-- @'Derivs' ('ListenLazyC' s m) = 'Pass' s ': 'Listen' s ': 'Tell' s ': 'Derivs' m@
--
-- @'Prims'  ('ListenLazyC' s m) = 'Pass' s ': 'Listen' s ': 'Prims' m@
--
-- This is a variant of 'runListen' that produces the
-- final accumulation lazily. __Use this only if you need__
-- __the laziness, as this would otherwise incur an unneccesary space leak.__
runWriterLazy :: forall s m a p
               . ( Monoid s
                 , Carrier m
                 , Threaders '[WriterLazyThreads] m p
                 )
              => WriterLazyC s m a
              -> m (s, a)
runWriterLazy (WriterLazyC m) = swap <$> LW.runWriterT m
{-# INLINE runWriterLazy #-}

tellTVar :: ( Monoid s
            , Effs '[Reader (s -> STM ()), Embed IO] m
            )
         => s
         -> m ()
tellTVar o = do
  write <- ask
  embed $ atomically $ write o
{-# INLINE tellTVar #-}


data WriterToEndoWriterH

instance (Monoid s, Eff (Tell (Endo s)) m)
      => Handler WriterToEndoWriterH (Tell s) m where
  effHandler (Tell s) = tell (Endo (s <>))
  {-# INLINE effHandler #-}

instance (Monoid s, Eff (Listen (Endo s)) m)
      => Handler WriterToEndoWriterH (Listen s) m where
  effHandler (Listen m) =
    (fmap . first) (\(Endo f) -> f mempty) $ listen m
  {-# INLINE effHandler #-}

instance (Monoid s, Eff (Pass (Endo s)) m)
      => Handler WriterToEndoWriterH (Pass s) m where
  effHandler (Pass m) =
    pass $
      (fmap . first)
        (\f (Endo ss) -> let !s' = f (ss mempty) in Endo (s' <>))
        m
  {-# INLINE effHandler #-}

fromEndoWriter :: (Monoid s, Functor f)
               => f (Endo s, a)
               -> f (s, a)
fromEndoWriter = (fmap . first) (\(Endo f) -> f mempty)
{-# INLINE fromEndoWriter #-}

type TellIntoEndoTellC s =
  ReinterpretC WriterToEndoWriterH (Tell s) '[Tell (Endo s)]

-- | Rewrite a @'Tell' s@ effect into a @'Tell' ('Endo' s)@ effect.
--
-- This effectively right-associates all uses of 'tell', which
-- asymptotically improves performance if the time complexity of '<>' for the
-- 'Monoid' depends only on the size of the first argument.
-- In particular, you should use this (if you can be bothered) if the monoid
-- is a list, such as 'String'.
--
-- Usage is to combine this with the 'Tell' interpreter of your choice, followed
-- by 'fromEndoWriter', like this:
--
-- @
--    'run'
--  $ ...
--  $ 'fromEndoWriter'
--  $ 'runTell'
--  $ 'tellIntoEndoTell' \@String -- The 'Monoid' must be specified
--  $ ...
-- @
tellIntoEndoTell :: ( Monoid s
                    , HeadEff (Tell (Endo s)) m
                    )
                 => TellIntoEndoTellC s m a
                 -> m a
tellIntoEndoTell = reinterpretViaHandler
{-# INLINE tellIntoEndoTell #-}

type ListenIntoEndoListenC s = CompositionC
  '[ IntroC '[Listen s, Tell s] '[Listen (Endo s), Tell (Endo s)]
   , InterpretC WriterToEndoWriterH (Listen s)
   , InterpretC WriterToEndoWriterH (Tell s)
   ]

-- | Rewrite connected @'Listen' s@ and @'Tell' s@ effects into
-- connected @'Listen' ('Endo' s)@ and @'Tell' ('Endo' s)@ effects.
--
-- This effectively right-associates all uses of 'tell', which
-- asymptotically improves performance if the time complexity of '<>' for the
-- 'Monoid' depends only on the size of the first argument.
-- In particular, you should use this (if you can be bothered) if the monoid
-- is a list, such as String.
--
-- Usage is to combine this with the 'Listen' interpreter of your choice,
-- followed by 'fromEndoWriter', like this:
--
-- @
--    'run'
--  $ ...
--  $ 'fromEndoWriter'
--  $ 'runListen'
--  $ 'listenIntoEndoListen' \@String -- The 'Monoid' must be specified
--  $ ...
-- @
--
listenIntoEndoListen :: ( Monoid s
                        , HeadEffs '[Listen (Endo s), Tell (Endo s)] m
                        )
                     => ListenIntoEndoListenC s m a
                     -> m a
listenIntoEndoListen =
     interpretViaHandler
  .# interpretViaHandler
  .# introUnderMany
  .# runComposition
{-# INLINE listenIntoEndoListen #-}

type WriterIntoEndoWriterC s = CompositionC
  '[ IntroC '[Pass s, Listen s, Tell s]
            '[Pass (Endo s), Listen (Endo s), Tell (Endo s)]
   , InterpretC WriterToEndoWriterH (Pass s)
   , InterpretC WriterToEndoWriterH (Listen s)
   , InterpretC WriterToEndoWriterH (Tell s)
   ]

-- | Rewrite connected @'Pass' s@, @'Listen' s@ and @'Tell' s@ effects
-- -- i.e. @'Writer' s@ -- into connected @'Pass' ('Endo' s)@,
-- @'Listen' ('Endo' s)@ and @'Tell' (Endo s)@ effects on top of the effect
-- stack -- i.e. @'Writer' (Endo s)@.
--
-- This effectively right-associates all uses of 'tell', which
-- asymptotically improves performance if the time complexity of '<>' for the
-- 'Monoid' depends only on the size of the first argument.
-- In particular, you should use this (if you can be bothered) if the
-- monoid is a list, such as String.
--
-- Usage is to combine this with the 'Writer' interpreter of your choice,
-- followed by 'fromEndoWriter', like this:
--
-- @
--    'run'
--  $ ...
--  $ 'fromEndoWriter'
--  $ 'runWriter'
--  $ 'writerIntoEndoWriter' \@String -- The 'Monoid' must be specified
--  $ ...
-- @
writerIntoEndoWriter :: ( Monoid s
                        , HeadEffs
                           '[Pass (Endo s), Listen (Endo s), Tell (Endo s)]
                           m
                        )
                     => WriterIntoEndoWriterC s m a
                     -> m a
writerIntoEndoWriter =
     interpretViaHandler
  .# interpretViaHandler
  .# interpretViaHandler
  .# introUnderMany
  .# runComposition
{-# INLINE writerIntoEndoWriter #-}

-- | Transform a 'Tell' effect into another 'Tell' effect by providing a function
-- to transform the type told.
--
-- This is useful to transform a @'Tell' s@ effect where @s@ isn't a 'Monoid'
-- into a @'Tell' t@ effect where @@ _is_ a 'Monoid', and thus can be
-- interpreted using the various 'Monoid'al 'Tell' interpreters.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'tellToTell' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'tellToTellSimple', which doesn't have a higher-rank type.
tellToTell :: forall s t m a
            . Eff (Tell t) m
           => (s -> t)
           -> InterpretReifiedC (Tell s) m a
           -> m a
tellToTell f = interpret $ \case
  Tell s -> tell (f s)
{-# INLINE tellToTell #-}

-- | Transform a 'Tell' effect into another 'Tell' effect by providing a function
-- to transform the type told.
--
-- This is useful to transform a @'Tell' s@ where @s@ isn't a 'Monoid' into a
-- @'Tell' t@ effect where @@ _is_ a 'Monoid', and thus can be interpreted using
-- the various 'Monoid'al 'Tell' interpreters.
--
-- This is a less performant version of 'tellToTell' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
tellToTellSimple :: forall s t m a p
                  . ( Eff (Tell t) m
                    , Threaders '[ReaderThreads] m p
                    )
                 => (s -> t)
                 -> InterpretSimpleC (Tell s) m a
                 -> m a
tellToTellSimple f = interpretSimple $ \case
  Tell s -> tell (f s)
{-# INLINE tellToTellSimple #-}

-- | Rewrite a 'Tell' effect into another 'Tell' effect on top of the effect
-- stack by providing a function to transform the type told.
--
-- This is useful to rewrite a @'Tell' s@ effect where @s@ isn't a 'Monoid'
-- into a @'Tell' t@ effect where @@ _is_ a 'Monoid', and thus can be
-- interpreted using the various 'Monoid'al 'Tell' interpreters.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'tellToTell' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'tellIntoTellSimple', which doesn't have a higher-rank type.
tellIntoTell :: forall s t m a
              . HeadEff (Tell t) m
             => (s -> t)
             -> ReinterpretReifiedC (Tell s) '[Tell t] m a
             -> m a
tellIntoTell f = reinterpret $ \case
  Tell s -> tell (f s)
{-# INLINE tellIntoTell #-}

-- | Rewrite a 'Tell' effect into another 'Tell' effect on top of the effect
-- stack by providing a function to transform the type told.
--
-- This is useful to rewrite a @'Tell' s@ effect where @s@ isn't a 'Monoid'
-- into a @'Tell' t@ effect where @@ _is_ a 'Monoid', and thus can be
-- interpreted using the various 'Monoid'al 'Tell' interpreters.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'tellToTell' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'tellIntoTellSimple', which doesn't have a higher-rank type.
--
-- This is a less performant version of 'tellIntoTell' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
tellIntoTellSimple :: forall s t m a p
                    . ( HeadEff (Tell t) m
                      , Threaders '[ReaderThreads] m p
                      )
                   => (s -> t)
                   -> ReinterpretSimpleC (Tell s) '[Tell t] m a
                   -> m a
tellIntoTellSimple f = reinterpretSimple $ \case
  Tell s -> tell (f s)
{-# INLINE tellIntoTellSimple #-}



listenTVar :: forall s m a
            . ( Monoid s
              , Effs '[Reader (s -> STM ()), Embed IO, Bracket] m
              )
           => m a
           -> m (s, a)
listenTVar main = do
  writeGlobal <- ask
  localVar    <- embed $ newTVarIO mempty
  switch      <- embed $ newTVarIO True
  let
    writeLocal :: s -> STM ()
    writeLocal o = do
      writeToLocal <- readTVar switch
      when writeToLocal $ do
        s <- readTVar localVar
        writeTVar localVar $! s <> o
      writeGlobal o
  a <- (local (\_ -> writeLocal) main)
         `finally`
       (embed $ atomically $ writeTVar switch False)
  s <- embed $ readTVarIO localVar
  return (s, a)
{-# INLINE listenTVar #-}

passTVar :: forall s m a
          . ( Monoid s
            , Effs '[Reader (s -> STM ()), Embed IO, Bracket] m
            )
         => m (s -> s, a)
         -> m a
passTVar main = do
  writeGlobal <- ask
  localVar    <- embed $ newTVarIO mempty
  switch      <- embed $ newTVarIO True
  let
    writeLocal :: s -> STM ()
    writeLocal o = do
      writeToLocal <- readTVar switch
      if writeToLocal then do
        s <- readTVar localVar
        writeTVar localVar $! s <> o
      else
        writeGlobal o

    commit :: (s -> s) -> IO ()
    commit f = atomically $ do
      notAlreadyCommited <- readTVar switch
      when notAlreadyCommited $ do
        s <- readTVar localVar
        writeGlobal (f s)
        writeTVar switch False

  ((_, a), _) <-
    generalBracket
      (pure ())
      (\_ -> \case
        ExitCaseSuccess (f, _) -> embed (commit f)
        _                      -> embed (commit id)
      )
      (\_ -> local (\_ -> writeLocal) main)
  return a
{-# INLINE passTVar #-}

data WriterToBracketH

type WriterToBracketC s = CompositionC
 '[ IntroC '[Pass s, Listen s, Tell s] '[Local (s -> STM ()), Ask (s -> STM ())]
  , InterpretC WriterToBracketH (Pass s)
  , InterpretC WriterToBracketH (Listen s)
  , InterpretC WriterTVarH (Tell s)
  , ReaderC (s -> STM ())
  ]

instance ( Monoid s
         , Effs '[Reader (s -> STM ()), Embed IO, Bracket] m
         )
      => Handler WriterToBracketH (Listen s) m where
  effHandler (Listen m) = listenTVar m
  {-# INLINE effHandler #-}

instance ( Monoid s
         , Effs '[Reader (s -> STM ()), Embed IO, Bracket] m
         )
      => Handler WriterToBracketH (Pass s) m where
  effHandler (Pass m) = passTVar m
  {-# INLINE effHandler #-}

-- | Run connected @'Pass' s@, @'Listen' s@ and @'Tell' s@ effects
-- -- i.e. @'Writer' s@ -- by accumulating uses of 'tell' through using atomic
-- operations in 'IO', relying on the provided protection of 'Bracket' for
-- the implementation.
--
-- @'Derivs' ('WriterToBracketC' s m) = 'Pass' s ': 'Listen' s : 'Tell' s ': 'Derivs' m@
--
-- @'Prims'  ('WriterToBracketC' s m) = 'Prims' m@
--
-- Note that unlike 'writerToIO', this does not have a higher-rank type.
writerToBracket :: forall s m a p
                 . ( Monoid s
                   , Effs [Embed IO, Bracket] m
                   , Threaders '[ReaderThreads] m p
                   )
                => WriterToBracketC s m a
                -> m (s, a)
writerToBracket m = do
  tvar <- embed $ newTVarIO mempty
  a    <- writerToBracketTVar tvar m
  s    <- embed $ readTVarIO tvar
  return (s, a)
{-# INLINE writerToBracket #-}

-- | Run connected @'Pass' s@, @'Listen' s@ and @'Tell' s@ effects
-- -- i.e. @'Writer' s@ -- by accumulating uses of 'tell' through using atomic
-- operations in 'IO' over a 'TVar', relying on the provided protection
-- of 'Bracket' for the implementation.
--
-- @'Derivs' ('WriterToBracketC' s m) = 'Pass' s ': 'Listen' s : 'Tell' s ': 'Derivs' m@
--
-- @'Prims'  ('WriterToBracketC' s m) = 'Prims' m@
--
-- Note that unlike 'runTellTVar', this does not have a higher-rank type.
writerToBracketTVar :: forall s m a p
                     . ( Monoid s
                       , Effs [Embed IO, Bracket] m
                       , Threaders '[ReaderThreads] m p
                       )
                    => TVar s
                    -> WriterToBracketC s m a
                    -> m a
writerToBracketTVar tvar =
     runReader (\o -> do
       s <- readTVar tvar
       writeTVar tvar $! s <> o
     )
  .# interpretViaHandler
  .# interpretViaHandler
  .# interpretViaHandler
  .# introUnderMany
  .# runComposition
{-# INLINE writerToBracketTVar #-}

data WriterTVarH

type ListenTVarC s = CompositionC
 '[ IntroC '[Listen s, Tell s] '[Local (s -> STM ()), Ask (s -> STM ())]
  , InterpretPrimC WriterTVarH (Listen s)
  , InterpretC WriterTVarH (Tell s)
  , ReaderC (s -> STM ())
  ]

type WriterTVarC s = CompositionC
 '[ IntroC '[Pass s, Listen s, Tell s] '[Local (s -> STM ()), Ask (s -> STM ())]
  , InterpretPrimC WriterTVarH (Pass s)
  , InterpretPrimC WriterTVarH (Listen s)
  , InterpretC WriterTVarH (Tell s)
  , ReaderC (s -> STM ())
  ]

instance ( Monoid s
         , Effs '[Reader (s -> STM ()), Embed IO] m
         )
      => Handler WriterTVarH (Tell s) m where
  effHandler (Tell o) = tellTVar o
  {-# INLINE effHandler #-}


instance ( Monoid s
         , Effs '[Reader (s -> STM ()), Embed IO] m
         , C.MonadMask m
         )
      => PrimHandler WriterTVarH (Listen s) m where
  effPrimHandler (Listen m) = bracketToIO (listenTVar (lift m))
  {-# INLINE effPrimHandler #-}

instance ( Monoid s
         , Effs '[Reader (s -> STM ()), Embed IO] m
         , C.MonadMask m
         )
      => PrimHandler WriterTVarH (Pass s) m where
  effPrimHandler (Pass m) = bracketToIO (passTVar (lift m))
  {-# INLINE effPrimHandler #-}

-- | Run a @'Tell' s@ effect where @s@ is a 'Monoid' by accumulating uses of
-- 'tell' through atomic operations in 'IO'.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'tellToIO' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'tellToIOSimple', which doesn't have a higher-rank type.
tellToIO :: forall s m a
          . ( Monoid s
            , Eff (Embed IO) m
            )
         => InterpretReifiedC (Tell s) m a
         -> m (s, a)
tellToIO m = do
  ref <- embed $ newIORef mempty
  a   <- runTellIORef ref m
  s   <- embed $ readIORef ref
  return (s, a)
{-# INLINE tellToIO #-}

-- | Run a @'Tell' s@ effect where @s@ is a 'Monoid' by accumulating uses of
-- 'tell' through using atomic operations in 'IO' over the provided 'IORef'.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'runTellIORef' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'runTellIORefSimple', which doesn't have a higher-rank type.
runTellIORef :: forall s m a
              . ( Monoid s
                , Eff (Embed IO) m
                )
             => IORef s
             -> InterpretReifiedC (Tell s) m a
             -> m a
runTellIORef ref = interpret $ \case
  Tell o -> embed $ atomicModifyIORef' ref (\s -> (s <> o, ()))
{-# INLINE runTellIORef #-}

-- | Run a @'Tell' s@ effect where @s@ is a 'Monoid' by accumulating uses of
-- 'tell' through using atomic operations in 'IO' over the provided 'TVar'.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'runTellTVar' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'runTellTVarSimple', which doesn't have a higher-rank type.
runTellTVar :: forall s m a
             . ( Monoid s
               , Eff (Embed IO) m
               )
            => TVar s
            -> InterpretReifiedC (Tell s) m a
            -> m a
runTellTVar tvar = interpret $ \case
  Tell o -> embed $ atomically $ do
    s <- readTVar tvar
    writeTVar tvar $! s <> o
{-# INLINE runTellTVar #-}

-- | Run a @'Tell' s@ effect where @s@ is a 'Monoid' by accumulating uses of
-- 'tell' through atomic operations in 'IO'.
--
-- This is a less performant version of 'tellToIO' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
tellToIOSimple :: forall s m a p
                . ( Monoid s
                  , Eff (Embed IO) m
                  , Threaders '[ReaderThreads] m p
                  )
               => InterpretSimpleC (Tell s) m a
               -> m (s, a)
tellToIOSimple m = do
  ref <- embed $ newIORef mempty
  a   <- runTellIORefSimple ref m
  s   <- embed $ readIORef ref
  return (s, a)
{-# INLINE tellToIOSimple #-}

-- | Run a @'Tell' s@ effect where @s@ is a 'Monoid' by accumulating uses of
-- 'tell' through using atomic operations in 'IO' over the provided 'IORef'.
--
-- This is a less performant version of 'tellToIO' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
runTellIORefSimple :: forall s m a p
                    . ( Monoid s
                      , Eff (Embed IO) m
                      , Threaders '[ReaderThreads] m p
                      )
                   => IORef s
                   -> InterpretSimpleC (Tell s) m a
                   -> m a
runTellIORefSimple ref = interpretSimple $ \case
  Tell o -> embed $ atomicModifyIORef' ref (\s -> (s <> o, ()))
{-# INLINE runTellIORefSimple #-}

-- | Run a @'Tell' s@ effect where @s@ is a 'Monoid' by accumulating uses of
-- 'tell' through using atomic operations in 'IO' over the provided 'TVar'.
--
-- This is a less performant version of 'tellToIO' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
runTellTVarSimple :: forall s m a p
                   . ( Monoid s
                     , Eff (Embed IO) m
                     , Threaders '[ReaderThreads] m p
                     )
                  => TVar s
                  -> InterpretSimpleC (Tell s) m a
                  -> m a
runTellTVarSimple tvar = interpretSimple $ \case
  Tell o -> embed $ atomically $ do
    s <- readTVar tvar
    writeTVar tvar $! s <> o
{-# INLINE runTellTVarSimple #-}

-- | Run connected @'Listen' s@ and @'Tell' s@ effects by accumulating uses of
-- 'tell' through using atomic operations in 'IO'.
--
-- @'Derivs' ('ListenTVarC' s m) = 'Listen' s : 'Tell' s ': 'Derivs' m@
--
-- @'Prims' ('ListenTVarC' s m) = 'Listen' s ': 'Prims' m@
--
-- Note that unlike 'tellToIO', this does not have a higher-rank type.
listenToIO :: forall s m a p
            . ( Monoid s
              , Eff (Embed IO) m
              , C.MonadMask m
              , Threaders '[ReaderThreads] m p
              )
           => ListenTVarC s m a
           -> m (s, a)
listenToIO m = do
  tvar <- embed $ newTVarIO mempty
  a    <- runListenTVar tvar m
  s    <- embed $ readTVarIO tvar
  return (s, a)
{-# INLINE listenToIO #-}

-- | Run connected @'Listen' s@ and @'Tell' s@ effects by accumulating uses of
-- 'tell' through using atomic operations in 'IO' over the provided 'TVar'.
--
-- @'Derivs' ('ListenTVarC' s m) = 'Listen' s : 'Tell' s ': 'Derivs' m@
--
-- @'Prims'  ('ListenTVarC' s m) = 'Listen' s ': 'Prims' m@
--
-- Note that unlike 'runTellTVar', this does not have a higher-rank type.
runListenTVar :: forall s m a p
               . ( Monoid s
                 , Eff (Embed IO) m
                 , C.MonadMask m
                 , Threaders '[ReaderThreads] m p
                 )
              => TVar s
              -> ListenTVarC s m a
              -> m a
runListenTVar tvar =
     runReader (\o -> do
       s <- readTVar tvar
       writeTVar tvar $! s <> o
     )
  .# interpretViaHandler
  .# interpretPrimViaHandler
  .# introUnderMany
  .# runComposition
{-# INLINE runListenTVar #-}

-- | Run connected @'Pass' s@, @'Listen' s@ and @'Tell' s@ effects
-- -- i.e. @'Writer' s@ -- by accumulating uses of 'tell' through using atomic
-- operations in 'IO'.
--
-- @'Derivs' ('WriterTVarC' s m) = 'Pass' s ': 'Listen' s : 'Tell' s ': 'Derivs' m@
--
-- @'Prims'  ('WriterTVarC' s m) = 'Pass' s ': 'Listen' s ': 'Prims' m@
--
-- Note that unlike 'tellToIO', this does not have a higher-rank type.
writerToIO :: forall s m a p
            . ( Monoid s
              , Eff (Embed IO) m
              , C.MonadMask m
              , Threaders '[ReaderThreads] m p
              )
           => WriterTVarC s m a
           -> m (s, a)
writerToIO m = do
  tvar <- embed $ newTVarIO mempty
  a    <- runWriterTVar tvar m
  s    <- embed $ readTVarIO tvar
  return (s, a)
{-# INLINE writerToIO #-}

-- | Run connected @'Pass' s@, @'Listen' s@ and @'Tell' s@ effects
-- -- i.e. @'Writer' s@ -- by accumulating uses of 'tell' through using atomic
-- operations in 'IO' over a 'TVar'.
--
-- @'Derivs' ('WriterTVarC' s m) = 'Pass' s ': 'Listen' s : 'Tell' s ': 'Derivs' m@
--
-- @'Prims' ('WriterTVarC' s m) = 'Pass' s ': 'Listen' s ': 'Prims' m@
--
-- Note that unlike 'runTellTVar', this does not have a higher-rank type.
runWriterTVar :: forall s m a p
               . ( Monoid s
                 , Eff (Embed IO) m
                 , C.MonadMask m
                 , Threaders '[ReaderThreads] m p
                 )
              => TVar s
              -> WriterTVarC s m a
              -> m a
runWriterTVar tvar =
     runReader (\o -> do
       s <- readTVar tvar
       writeTVar tvar $! s <> o
     )
  .# interpretViaHandler
  .# interpretPrimViaHandler
  .# interpretPrimViaHandler
  .# introUnderMany
  .# runComposition
{-# INLINE runWriterTVar #-}
