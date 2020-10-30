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

  , runTellAction

  , tellIntoEndoTell
  , tellToTell
  , tellIntoTell

  , ignoreTell

  -- * Simple variants of interpretations for 'Tell'
  , tellToIOSimple
  , runTellIORefSimple
  , runTellTVarSimple

  , runTellActionSimple

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

    -- * MonadMask
  , C.MonadMask

    -- * Carriers
  , TellC
  , TellLazyC
  , TellListC
  , TellListLazyC
  , TellIntoEndoTellC
  , IgnoreTellC
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
import Control.Effect.Type.ListenPrim
import Control.Effect.Type.WriterPrim

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

-- | A pseudo-effect for connected @'Tell' o@, @'Listen' o@ and @'Pass' o@ effects.
--
-- @'Writer'@ should only ever be used inside of 'Eff' and 'Effs'
-- constraints. It is not a real effect! See 'Bundle'.
type Writer o = Bundle '[Tell o, Listen o, Pass o]

tell :: Eff (Tell o) m => o -> m ()
tell = send . Tell
{-# INLINE tell #-}

listen :: Eff (Listen o) m => m a -> m (o, a)
listen = send . Listen
{-# INLINE listen #-}

pass :: Eff (Pass o) m => m (o -> o, a) -> m a
pass = send .# Pass
{-# INLINE pass #-}

censor :: Eff (Pass o) m => (o -> o) -> m a -> m a
censor f = pass . fmap ((,) f)
{-# INLINE censor #-}


data TellListH

type TellListC o = CompositionC
 '[ ReinterpretC TellListH (Tell o) '[Tell (Dual [o])]
  , TellC (Dual [o])
  ]

instance Eff (Tell (Dual [o])) m
      => Handler TellListH (Tell o) m where
  effHandler (Tell o) = tell (Dual [o])
  {-# INLINEABLE effHandler #-}

-- | Run a @'Tell' o@ effect by gathering the 'tell's into a list.
--
-- @'Derivs' ('TellListC' o m) = 'Tell' o ': 'Derivs' m@
--
-- @'Prims'  ('TellListC' o m) = 'Prims' m@
--
-- The resulting list is produced strictly. See 'runTellListLazy' for a lazy
-- variant.
runTellList :: forall o m a p
             . ( Carrier m
               , Threaders '[WriterThreads] m p
               )
            => TellListC o m a
            -> m ([o], a)
runTellList =
     (fmap . first) (reverse .# getDual)
  .  runTell
  .# reinterpretViaHandler
  .# runComposition
{-# INLINE runTellList #-}

data TellListLazyH

type TellListLazyC o = CompositionC
 '[ ReinterpretC TellListLazyH (Tell o) '[Tell (Endo [o])]
  , TellLazyC (Endo [o])
  ]

instance Eff (Tell (Endo [o])) m
      => Handler TellListLazyH (Tell o) m where
  effHandler (Tell o) = tell (Endo (o:))
  {-# INLINEABLE effHandler #-}

-- | Run a @'Tell' o@ by gathering the 'tell's into a list.
--
-- @'Derivs' ('TellListLazyC' o m) = 'Tell' o ': 'Derivs' m@
--
-- @'Prims'  ('TellListLazyC' o m) = 'Prims' m@
--
-- This is a variant of 'runTellList' that produces the
-- final list lazily. __Use this only if you need__
-- __the laziness, as this would otherwise incur an unneccesary space leak.__
runTellListLazy :: forall o m a p
                 . ( Carrier m
                   , Threaders '[WriterLazyThreads] m p
                   )
                => TellListLazyC o m a
                -> m ([o], a)
runTellListLazy =
     fromEndoWriter
  .  runTellLazy
  .# reinterpretViaHandler
  .# runComposition
{-# INLINE runTellListLazy #-}


-- | Run a @'Tell' o@ effect, where @o@ is a 'Monoid', by accumulating
-- all the uses of 'tell'.
--
-- You may want to combine this with 'tellIntoTell'.
--
-- Unlike 'runListen' and 'runWriter', this does not provide the ability to
-- interact with the 'tell's through 'listen' and 'pass'; but also doesn't
-- impose any primitive effects, meaning 'runTell' doesn't restrict what
-- interpreters are run before it.
--
-- @'Derivs' ('TellC' o m) = 'Tell' o ': 'Derivs' m@
--
-- @'Prims'  ('TellC' o m) = 'Prims' m@
--
-- This produces the final accumulation @o@ strictly. See 'runTellLazy' for a
-- lazy variant of this.
runTell :: forall o m a p
         . ( Monoid o
           , Carrier m
           , Threaders '[WriterThreads] m p
           )
        => TellC o m a
        -> m (o, a)
runTell (TellC m) = do
  (a, o) <- W.runWriterT m
  return (o, a)
{-# INLINE runTell #-}

-- | Run connected @'Listen' o@ and @'Tell' o@ effects, where @o@ is a 'Monoid',
-- by accumulating all the uses of 'tell'.
--
-- Unlike 'runWriter', this does not provide the power of 'pass'; but because
-- of that, it also doesn't impose 'Pass' as a primitive effect, meaning
-- a larger variety of interpreters may be run before 'runListen' compared to
-- 'runWriter'.
--
-- @'Derivs' ('ListenC' o m) = 'Listen' o ': 'Tell' o ': 'Derivs' m@
--
-- @'Prims'  ('ListenC' o m) = 'ListenPrim' o ': 'Prims' m@
--
-- This produces the final accumulation strictly. See 'runListenLazy' for a
-- lazy variant of this.
runListen :: forall o m a p
           . ( Monoid o
             , Carrier m
             , Threaders '[WriterThreads] m p
             )
          => ListenC o m a
          -> m (o, a)
runListen (ListenC m) = do
  (a, o) <- W.runWriterT m
  return (o, a)
{-# INLINE runListen #-}

-- | Run connected @'Pass' o@, @'Listen' o@ and @'Tell' o@ effects,
-- -- i.e. @'Writer' o@ -- where @o@ is a 'Monoid', by accumulating all the
-- uses of 'tell'.
--
-- @'Pass' o@ is a fairly restrictive primitive effect. Notably,
-- 'Control.Effect.Cont.runCont' can't be used before 'runWriter'.
-- If you don't need 'pass', consider using 'runTell' or 'runListen' instead.
--
-- @'Derivs' ('WriterC' o m) = 'Pass' o ': 'Listen' o ': 'Tell' o ': 'Derivs' m@
--
-- @'Prims'  ('WriterC' o m) = 'WriterPrim' o ': 'Prims' m@
--
-- This produces the final accumulation strictly. See 'runWriterLazy' for a
-- lazy variant of this.
runWriter :: forall o m a p
           . ( Monoid o
             , Carrier m
             , Threaders '[WriterThreads] m p
             )
          => WriterC o m a
          -> m (o, a)
runWriter (WriterC m) = do
  (a, o) <- W.runWriterT m
  return (o, a)
{-# INLINE runWriter #-}


-- | Run a @'Tell' o@ effect, where @o@ is a 'Monoid', by accumulating all the
-- uses of 'tell' lazily.
--
-- @'Derivs' ('TellLazyC' o m) = 'Tell' o ': 'Derivs' m@
--
-- @'Prims'  ('TellLazyC' o m) = 'Prims' m@
--
-- This is a variant of 'runTell' that produces the final accumulation
-- lazily. __Use this only if you need__
-- __the laziness, as this would otherwise incur an unneccesary space leak.__
runTellLazy :: forall o m a p
         . ( Monoid o
           , Carrier m
           , Threaders '[WriterLazyThreads] m p
           )
        => TellLazyC o m a
        -> m (o, a)
runTellLazy (TellLazyC m) = swap <$> LW.runWriterT m
{-# INLINE runTellLazy #-}

-- | Run connected @'Listen' o@ and @'Tell' o@ effects,
-- where @o@ is a 'Monoid', by accumulating all the uses of 'tell' lazily.
--
-- @'Derivs' ('ListenLazyC' o m) = 'Listen' o ': 'Tell' o ': 'Derivs' m@
--
-- @'Prims'  ('ListenLazyC' o m) = 'ListenPrim' o ': 'Prims' m@
--
-- This is a variant of 'runListen' that produces the
-- final accumulation lazily. __Use this only if you need__
-- __the laziness, as this would otherwise incur an unneccesary space leak.__
runListenLazy :: forall o m a p
           . ( Monoid o
             , Carrier m
             , Threaders '[WriterThreads] m p
             )
          => ListenLazyC o m a
          -> m (o, a)
runListenLazy (ListenLazyC m) = swap <$> LW.runWriterT m
{-# INLINE runListenLazy #-}

-- | Run connected @'Pass' o@, @'Listen' o@ and @'Tell' o@ effects,
-- -- i.e. @'Writer' o@ -- where @o@ is a 'Monoid',
-- by accumulating all the uses of 'tell' lazily.
--
-- @'Derivs' ('ListenLazyC' o m) = 'Pass' o ': 'Listen' o ': 'Tell' o ': 'Derivs' m@
--
-- @'Prims'  ('ListenLazyC' o m) = 'WriterPrim' o ': 'Prims' m@
--
-- This is a variant of 'runListen' that produces the
-- final accumulation lazily. __Use this only if you need__
-- __the laziness, as this would otherwise incur an unneccesary space leak.__
runWriterLazy :: forall o m a p
               . ( Monoid o
                 , Carrier m
                 , Threaders '[WriterLazyThreads] m p
                 )
              => WriterLazyC o m a
              -> m (o, a)
runWriterLazy (WriterLazyC m) = swap <$> LW.runWriterT m
{-# INLINE runWriterLazy #-}

tellTVar :: ( Monoid o
            , Effs '[Ask (o -> STM ()), Embed IO] m
            )
         => o
         -> m ()
tellTVar o = do
  write <- ask
  embed $ atomically $ write o
{-# INLINE tellTVar #-}


data WriterToEndoWriterH

instance (Monoid o, Eff (Tell (Endo o)) m)
      => Handler WriterToEndoWriterH (Tell o) m where
  effHandler (Tell o) = tell (Endo (o <>))
  {-# INLINEABLE effHandler #-}

instance (Monoid o, Eff (Listen (Endo o)) m)
      => Handler WriterToEndoWriterH (Listen o) m where
  effHandler (Listen m) =
    (fmap . first) (\(Endo f) -> f mempty) $ listen m
  {-# INLINEABLE effHandler #-}

instance (Monoid o, Eff (Pass (Endo o)) m)
      => Handler WriterToEndoWriterH (Pass o) m where
  effHandler (Pass m) =
    pass $
      (fmap . first)
        (\f (Endo ss) -> let !s' = f (ss mempty) in Endo (s' <>))
        m
  {-# INLINEABLE effHandler #-}

fromEndoWriter :: (Monoid o, Functor f)
               => f (Endo o, a)
               -> f (o, a)
fromEndoWriter = (fmap . first) (\(Endo f) -> f mempty)
{-# INLINE fromEndoWriter #-}

type TellIntoEndoTellC o =
  ReinterpretC WriterToEndoWriterH (Tell o) '[Tell (Endo o)]

-- | Rewrite a @'Tell' o@ effect into a @'Tell' ('Endo' o)@ effect.
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
tellIntoEndoTell :: ( Monoid o
                    , HeadEff (Tell (Endo o)) m
                    )
                 => TellIntoEndoTellC o m a
                 -> m a
tellIntoEndoTell = reinterpretViaHandler
{-# INLINE tellIntoEndoTell #-}

type ListenIntoEndoListenC o = CompositionC
  '[ IntroC '[Listen o, Tell o] '[Listen (Endo o), Tell (Endo o)]
   , InterpretC WriterToEndoWriterH (Listen o)
   , InterpretC WriterToEndoWriterH (Tell o)
   ]

-- | Rewrite connected @'Listen' o@ and @'Tell' o@ effects into
-- connected @'Listen' ('Endo' o)@ and @'Tell' ('Endo' o)@ effects.
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
listenIntoEndoListen :: ( Monoid o
                        , HeadEffs '[Listen (Endo o), Tell (Endo o)] m
                        )
                     => ListenIntoEndoListenC o m a
                     -> m a
listenIntoEndoListen =
     interpretViaHandler
  .# interpretViaHandler
  .# introUnderMany
  .# runComposition
{-# INLINE listenIntoEndoListen #-}

type WriterIntoEndoWriterC o = CompositionC
  '[ IntroC '[Pass o, Listen o, Tell o]
            '[Pass (Endo o), Listen (Endo o), Tell (Endo o)]
   , InterpretC WriterToEndoWriterH (Pass o)
   , InterpretC WriterToEndoWriterH (Listen o)
   , InterpretC WriterToEndoWriterH (Tell o)
   ]

-- | Rewrite connected @'Pass' o@, @'Listen' o@ and @'Tell' o@ effects
-- -- i.e. @'Writer' o@ -- into connected @'Pass' ('Endo' o)@,
-- @'Listen' ('Endo' o)@ and @'Tell' ('Endo' o)@ effects on top of the effect
-- stack -- i.e. @'Writer' ('Endo' o)@.
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
writerIntoEndoWriter :: ( Monoid o
                        , HeadEffs
                           '[Pass (Endo o), Listen (Endo o), Tell (Endo o)]
                           m
                        )
                     => WriterIntoEndoWriterC o m a
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
-- This is useful to transform a @'Tell' o@ effect where @o@ isn't a 'Monoid'
-- into a @'Tell' o'@ effect where @o'@ /is/ a 'Monoid', and thus can be
-- interpreted using the various 'Monoid'al 'Tell' interpreters.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'tellToTell' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'tellToTellSimple', which doesn't have a higher-rank type.
tellToTell :: forall o o' m a
            . Eff (Tell o') m
           => (o -> o')
           -> InterpretReifiedC (Tell o) m a
           -> m a
tellToTell f = interpret $ \case
  Tell o -> tell (f o)
{-# INLINE tellToTell #-}

-- | Transform a 'Tell' effect into another 'Tell' effect by providing a function
-- to transform the type told.
--
-- This is useful to transform a @'Tell' o@ where @o@ isn't a 'Monoid' into a
-- @'Tell' p@ effect where @p@ /is/ a 'Monoid', and thus can be interpreted using
-- the various 'Monoid'al 'Tell' interpreters.
--
-- This is a less performant version of 'tellToTell' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
tellToTellSimple :: forall o o' m a p
                  . ( Eff (Tell o') m
                    , Threaders '[ReaderThreads] m p
                    )
                 => (o -> o')
                 -> InterpretSimpleC (Tell o) m a
                 -> m a
tellToTellSimple f = interpretSimple $ \case
  Tell o -> tell (f o)
{-# INLINE tellToTellSimple #-}

-- | Rewrite a 'Tell' effect into another 'Tell' effect on top of the effect
-- stack by providing a function to transform the type told.
--
-- This is useful to rewrite a @'Tell' o@ effect where @o@ isn't a 'Monoid'
-- into a @'Tell' t@ effect where @t@ /is/ a 'Monoid', and thus can be
-- interpreted using the various 'Monoid'al 'Tell' interpreters.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'tellToTell' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'tellIntoTellSimple', which doesn't have a higher-rank type.
tellIntoTell :: forall o o' m a
              . HeadEff (Tell o') m
             => (o -> o')
             -> ReinterpretReifiedC (Tell o) '[Tell o'] m a
             -> m a
tellIntoTell f = reinterpret $ \case
  Tell o -> tell (f o)
{-# INLINE tellIntoTell #-}

-- | Rewrite a 'Tell' effect into another 'Tell' effect on top of the effect
-- stack by providing a function to transform the type told.
--
-- This is useful to rewrite a @'Tell' o@ effect where @o@ isn't a 'Monoid'
-- into a @'Tell' o'@ effect where @o'@ /is/ a 'Monoid', and thus can be
-- interpreted using the various 'Monoid'al 'Tell' interpreters.
--
-- This is a less performant version of 'tellIntoTell' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
tellIntoTellSimple :: forall o o' m a p
                    . ( HeadEff (Tell o') m
                      , Threaders '[ReaderThreads] m p
                      )
                   => (o -> o')
                   -> ReinterpretSimpleC (Tell o) '[Tell o'] m a
                   -> m a
tellIntoTellSimple f = reinterpretSimple $ \case
  Tell o -> tell (f o)
{-# INLINE tellIntoTellSimple #-}



listenTVar :: forall o m a
            . ( Monoid o
              , Effs '[Reader (o -> STM ()), Embed IO, Bracket] m
              )
           => m a
           -> m (o, a)
listenTVar main = do
  writeGlobal <- ask
  localVar    <- embed $ newTVarIO mempty
  switch      <- embed $ newTVarIO True
  let
    writeLocal :: o -> STM ()
    writeLocal o = do
      writeToLocal <- readTVar switch
      when writeToLocal $ do
        s <- readTVar localVar
        writeTVar localVar $! s <> o
      writeGlobal o
  a <- (local (\_ -> writeLocal) main)
         `finally`
       (embed $ atomically $ writeTVar switch False)
  o <- embed $ readTVarIO localVar
  return (o, a)

passTVar :: forall o m a
          . ( Monoid o
            , Effs '[Reader (o -> STM ()), Embed IO, Bracket] m
            )
         => m (o -> o, a)
         -> m a
passTVar main = do
  writeGlobal <- ask
  localVar    <- embed $ newTVarIO mempty
  switch      <- embed $ newTVarIO True
  let
    writeLocal :: o -> STM ()
    writeLocal o = do
      writeToLocal <- readTVar switch
      if writeToLocal then do
        s <- readTVar localVar
        writeTVar localVar $! s <> o
      else
        writeGlobal o

    commit :: (o -> o) -> IO ()
    commit f = atomically $ do
      notAlreadyCommited <- readTVar switch
      when notAlreadyCommited $ do
        o <- readTVar localVar
        writeGlobal (f o)
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

data WriterToBracketH

type WriterToBracketC o = CompositionC
 '[ IntroC '[Pass o, Listen o, Tell o] '[Local (o -> STM ()), Ask (o -> STM ())]
  , InterpretC WriterToBracketH (Pass o)
  , InterpretC WriterToBracketH (Listen o)
  , InterpretC WriterTVarH (Tell o)
  , ReaderC (o -> STM ())
  ]

instance ( Monoid o
         , Effs '[Reader (o -> STM ()), Embed IO, Bracket] m
         )
      => Handler WriterToBracketH (Listen o) m where
  effHandler (Listen m) = listenTVar m
  {-# INLINEABLE effHandler #-}

instance ( Monoid o
         , Effs '[Reader (o -> STM ()), Embed IO, Bracket] m
         )
      => Handler WriterToBracketH (Pass o) m where
  effHandler (Pass m) = passTVar m
  {-# INLINEABLE effHandler #-}

-- | Run connected @'Pass' o@, @'Listen' o@ and @'Tell' o@ effects
-- -- i.e. @'Writer' o@ -- by accumulating uses of 'tell' through using atomic
-- operations in 'IO', relying on the provided protection of 'Bracket' for
-- the implementation.
--
-- @'Derivs' ('WriterToBracketC' o m) = 'Pass' o ': 'Listen' o : 'Tell' o ': 'Derivs' m@
--
-- @'Prims'  ('WriterToBracketC' o m) = 'Control.Effect.Type.ReaderPrim.ReaderPrim' (o -> STM ()) ': 'Prims' m@
--
-- Note that unlike 'tellToIO', this does not have a higher-rank type.
writerToBracket :: forall o m a p
                 . ( Monoid o
                   , Effs [Embed IO, Bracket] m
                   , Threaders '[ReaderThreads] m p
                   )
                => WriterToBracketC o m a
                -> m (o, a)
writerToBracket m = do
  tvar <- embed $ newTVarIO mempty
  a    <- writerToBracketTVar tvar m
  o    <- embed $ readTVarIO tvar
  return (o, a)
{-# INLINE writerToBracket #-}

-- | Run connected @'Pass' o@, @'Listen' o@ and @'Tell' o@ effects
-- -- i.e. @'Writer' o@ -- by accumulating uses of 'tell' through using atomic
-- operations in 'IO' over a 'TVar', relying on the provided protection
-- of 'Bracket' for the implementation.
--
-- @'Derivs' ('WriterToBracketC' o m) = 'Pass' o ': 'Listen' o : 'Tell' o ': 'Derivs' m@
--
-- @'Prims'  ('WriterToBracketC' o m) = 'Control.Effect.Type.ReaderPrim.ReaderPrim' (o -> STM ()) ': 'Prims' m@
--
-- Note that unlike 'runTellTVar', this does not have a higher-rank type.
writerToBracketTVar :: forall o m a p
                     . ( Monoid o
                       , Effs [Embed IO, Bracket] m
                       , Threaders '[ReaderThreads] m p
                       )
                    => TVar o
                    -> WriterToBracketC o m a
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

type ListenTVarC o = CompositionC
 '[ IntroC '[Listen o, Tell o]
     '[ ListenPrim o
      , Local (o -> STM ())
      , Ask (o -> STM ())
      ]
  , InterpretC WriterTVarH (Listen o)
  , InterpretC WriterTVarH (Tell o)
  , InterpretPrimC WriterTVarH (ListenPrim o)
  , ReaderC (o -> STM ())
  ]

type WriterTVarC o = CompositionC
 '[ IntroC '[Pass o, Listen o, Tell o]
     '[ ListenPrim o
      , WriterPrim o
      , Local (o -> STM ())
      , Ask (o -> STM ())
      ]
  , InterpretC WriterTVarH (Pass o)
  , InterpretC WriterTVarH (Listen o)
  , InterpretC WriterTVarH (Tell o)
  , InterpretC WriterTVarH (ListenPrim o)
  , InterpretPrimC WriterTVarH (WriterPrim o)
  , ReaderC (o -> STM ())
  ]

instance ( Monoid o
         , Effs '[Reader (o -> STM ()), Embed IO] m
         )
      => Handler WriterTVarH (Tell o) m where
  effHandler (Tell o) = tellTVar o
  {-# INLINEABLE effHandler #-}

instance Eff (ListenPrim o) m
      => Handler WriterTVarH (Listen o) m where
  effHandler (Listen m) = send $ ListenPrimListen m
  {-# INLINEABLE effHandler #-}

instance Eff (WriterPrim o) m
      => Handler WriterTVarH (Pass o) m where
  effHandler (Pass m) = send $ WriterPrimPass m
  {-# INLINEABLE effHandler #-}

instance Eff (WriterPrim o) m
      => Handler WriterTVarH (ListenPrim o) m where
  effHandler = \case
    ListenPrimTell o   -> send $ WriterPrimTell o
    ListenPrimListen m -> send $ WriterPrimListen m
  {-# INLINEABLE effHandler #-}

instance ( Monoid o
         , Effs '[Reader (o -> STM ()), Embed IO] m
         , C.MonadMask m
         )
      => PrimHandler WriterTVarH (ListenPrim o) m where
  effPrimHandler = \case
    ListenPrimTell   o -> tellTVar o
    ListenPrimListen m -> bracketToIO (listenTVar (lift m))
  {-# INLINEABLE effPrimHandler #-}

instance ( Monoid o
         , Effs '[Reader (o -> STM ()), Embed IO] m
         , C.MonadMask m
         )
      => PrimHandler WriterTVarH (WriterPrim o) m where
  effPrimHandler = \case
    WriterPrimTell   o -> tellTVar o
    WriterPrimListen m -> bracketToIO (listenTVar (lift m))
    WriterPrimPass   m -> bracketToIO (passTVar (lift m))
  {-# INLINEABLE effPrimHandler #-}

-- | Run a @'Tell' o@ effect where @o@ is a 'Monoid' by accumulating uses of
-- 'tell' through atomic operations in 'IO'.
--
-- You may want to combine this with 'tellIntoTell'.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'tellToIO' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'tellToIOSimple', which doesn't have a higher-rank type.
tellToIO :: forall o m a
          . ( Monoid o
            , Eff (Embed IO) m
            )
         => InterpretReifiedC (Tell o) m a
         -> m (o, a)
tellToIO m = do
  ref <- embed $ newIORef mempty
  a   <- runTellIORef ref m
  o   <- embed $ readIORef ref
  return (o, a)
{-# INLINE tellToIO #-}

-- | Run a @'Tell' o@ effect where @o@ is a 'Monoid' by accumulating uses of
-- 'tell' through using atomic operations in 'IO' over the provided 'IORef'.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'runTellIORef' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'runTellIORefSimple', which doesn't have a higher-rank type.
runTellIORef :: forall o m a
              . ( Monoid o
                , Eff (Embed IO) m
                )
             => IORef o
             -> InterpretReifiedC (Tell o) m a
             -> m a
runTellIORef ref = interpret $ \case
  Tell o -> embed $ atomicModifyIORef' ref (\s -> (s <> o, ()))
{-# INLINE runTellIORef #-}

-- | Run a @'Tell' o@ effect where @o@ is a 'Monoid' by accumulating uses of
-- 'tell' through using atomic operations in 'IO' over the provided 'TVar'.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'runTellTVar' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'runTellTVarSimple', which doesn't have a higher-rank type.
runTellTVar :: forall o m a
             . ( Monoid o
               , Eff (Embed IO) m
               )
            => TVar o
            -> InterpretReifiedC (Tell o) m a
            -> m a
runTellTVar tvar = interpret $ \case
  Tell o -> embed $ atomically $ do
    s <- readTVar tvar
    writeTVar tvar $! s <> o
{-# INLINE runTellTVar #-}

-- | Run a @'Tell' o@ effect where @o@ is a 'Monoid' by accumulating uses of
-- 'tell' through atomic operations in 'IO'.
--
-- You may want to combine this with 'tellIntoTellSimple'.
--
-- This is a less performant version of 'tellToIO' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
tellToIOSimple :: forall o m a p
                . ( Monoid o
                  , Eff (Embed IO) m
                  , Threaders '[ReaderThreads] m p
                  )
               => InterpretSimpleC (Tell o) m a
               -> m (o, a)
tellToIOSimple m = do
  ref <- embed $ newIORef mempty
  a   <- runTellIORefSimple ref m
  o   <- embed $ readIORef ref
  return (o, a)
{-# INLINE tellToIOSimple #-}

-- | Run a @'Tell' o@ effect where @o@ is a 'Monoid' by accumulating uses of
-- 'tell' through using atomic operations in 'IO' over the provided 'IORef'.
--
-- This is a less performant version of 'tellToIO' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
runTellIORefSimple :: forall o m a p
                    . ( Monoid o
                      , Eff (Embed IO) m
                      , Threaders '[ReaderThreads] m p
                      )
                   => IORef o
                   -> InterpretSimpleC (Tell o) m a
                   -> m a
runTellIORefSimple ref = interpretSimple $ \case
  Tell o -> embed $ atomicModifyIORef' ref (\s -> (s <> o, ()))
{-# INLINE runTellIORefSimple #-}

-- | Run a @'Tell' o@ effect where @o@ is a 'Monoid' by accumulating uses of
-- 'tell' through using atomic operations in 'IO' over the provided 'TVar'.
--
-- This is a less performant version of 'tellToIO' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
runTellTVarSimple :: forall o m a p
                   . ( Monoid o
                     , Eff (Embed IO) m
                     , Threaders '[ReaderThreads] m p
                     )
                  => TVar o
                  -> InterpretSimpleC (Tell o) m a
                  -> m a
runTellTVarSimple tvar = interpretSimple $ \case
  Tell o -> embed $ atomically $ do
    s <- readTVar tvar
    writeTVar tvar $! s <> o
{-# INLINE runTellTVarSimple #-}

-- | Run connected @'Listen' o@ and @'Tell' o@ effects by accumulating uses of
-- 'tell' through using atomic operations in 'IO'.
--
-- @'Derivs' ('ListenTVarC' o m) = 'Listen' o ': 'Tell' o ': 'Derivs' m@
--
-- @'Prims'  ('ListenTVarC' o m) = 'ListenPrim' o ': 'Control.Effect.Type.ReaderPrim.ReaderPrim' (o -> STM ()) ': 'Prims' m@
--
-- Note that unlike 'tellToIO', this does not have a higher-rank type.
listenToIO :: forall o m a p
            . ( Monoid o
              , Eff (Embed IO) m
              , C.MonadMask m
              , Threaders '[ReaderThreads] m p
              )
           => ListenTVarC o m a
           -> m (o, a)
listenToIO m = do
  tvar <- embed $ newTVarIO mempty
  a    <- runListenTVar tvar m
  o    <- embed $ readTVarIO tvar
  return (o, a)
{-# INLINE listenToIO #-}

-- | Run connected @'Listen' o@ and @'Tell' o@ effects by accumulating uses of
-- 'tell' through using atomic operations in 'IO' over the provided 'TVar'.
--
-- @'Derivs' ('ListenTVarC' o m) = 'Listen' o : 'Tell' o ': 'Derivs' m@
--
-- @'Prims'  ('ListenTVarC' o m) = 'ListenPrim' o ': 'Control.Effect.Type.ReaderPrim.ReaderPrim' (o -> STM ()) ': 'Prims' m@
--
-- Note that unlike 'runTellTVar', this does not have a higher-rank type.
runListenTVar :: forall o m a p
               . ( Monoid o
                 , Eff (Embed IO) m
                 , C.MonadMask m
                 , Threaders '[ReaderThreads] m p
                 )
              => TVar o
              -> ListenTVarC o m a
              -> m a
runListenTVar tvar =
     runReader (\o -> do
       s <- readTVar tvar
       writeTVar tvar $! s <> o
     )
  .# interpretPrimViaHandler
  .# interpretViaHandler
  .# interpretViaHandler
  .# introUnderMany
  .# runComposition
{-# INLINE runListenTVar #-}

-- | Run connected @'Pass' o@, @'Listen' o@ and @'Tell' o@ effects
-- -- i.e. @'Writer' o@ -- by accumulating uses of 'tell' through using atomic
-- operations in 'IO'.
--
-- @'Derivs' ('WriterTVarC' o m) = 'Pass' o ': 'Listen' o : 'Tell' o ': 'Derivs' m@
--
-- @'Prims'  ('WriterTVarC' o m) = 'WriterPrim' o ': 'Control.Effect.Type.ReaderPrim.ReaderPrim' (o -> STM ()) ': 'Prims' m@
--
-- Note that unlike 'tellToIO', this does not have a higher-rank type.
writerToIO :: forall o m a p
            . ( Monoid o
              , Eff (Embed IO) m
              , C.MonadMask m
              , Threaders '[ReaderThreads] m p
              )
           => WriterTVarC o m a
           -> m (o, a)
writerToIO m = do
  tvar <- embed $ newTVarIO mempty
  a    <- runWriterTVar tvar m
  o    <- embed $ readTVarIO tvar
  return (o, a)
{-# INLINE writerToIO #-}

-- | Run connected @'Pass' o@, @'Listen' o@ and @'Tell' o@ effects
-- -- i.e. @'Writer' o@ -- by accumulating uses of 'tell' through using atomic
-- operations in 'IO' over a 'TVar'.
--
-- @'Derivs' ('WriterTVarC' o m) = 'Pass' o ': 'Listen' o : 'Tell' o ': 'Derivs' m@
--
-- @'Prims'  ('WriterTVarC' o m) = 'WriterPrim' o ': 'Control.Effect.Type.ReaderPrim.ReaderPrim' (o -> STM ()) ': 'Prims' m@
--
-- Note that unlike 'runTellTVar', this does not have a higher-rank type.
runWriterTVar :: forall o m a p
               . ( Monoid o
                 , Eff (Embed IO) m
                 , C.MonadMask m
                 , Threaders '[ReaderThreads] m p
                 )
              => TVar o
              -> WriterTVarC o m a
              -> m a
runWriterTVar tvar =
     runReader (\o -> do
       s <- readTVar tvar
       writeTVar tvar $! s <> o
     )
  .# interpretPrimViaHandler
  .# interpretViaHandler
  .# interpretViaHandler
  .# interpretViaHandler
  .# interpretViaHandler
  .# introUnderMany
  .# runComposition
{-# INLINE runWriterTVar #-}


-- | Run a 'Tell' effect by providing an action to be executed
-- at each use of 'tell'.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'runTellAction' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower 'runTellActionSimple',
-- which doesn't have a higher-rank type.
runTellAction :: forall o m a
               . Carrier m
              => (o -> m ())
              -> InterpretReifiedC (Tell o) m a
              -> m a
runTellAction act = interpret $ \case
  Tell o -> liftBase (act o)
{-# INLINE runTellAction #-}

-- | Run a 'Tell' effect by providing an action to be executed
-- at each use of 'tell'.
--
-- This is a less performant version of 'runTellAction' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
runTellActionSimple :: forall o m a p
                     . (Carrier m, Threaders '[ReaderThreads] m p)
                    => (o -> m ())
                    -> InterpretSimpleC (Tell o) m a
                    -> m a
runTellActionSimple act = interpretSimple $ \case
  Tell o -> liftBase (act o)
{-# INLINE runTellActionSimple #-}

data IgnoreTellH

instance Carrier m
      => Handler IgnoreTellH (Tell o) m where
  effHandler (Tell _) = pure ()
  {-# INLINEABLE effHandler #-}

type IgnoreTellC o = InterpretC IgnoreTellH (Tell o)

-- | Run a 'Tell' effect by ignoring it, doing no output at all.
ignoreTell :: forall o m a
            . Carrier m
           => IgnoreTellC o m a -> m a
ignoreTell = interpretViaHandler
{-# INLINE ignoreTell #-}
