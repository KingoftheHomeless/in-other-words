{-# LANGUAGE DerivingVia #-}
module Control.Effect.Trace
  ( -- * Effects
    Trace(..)

    -- * Actions
  , trace
  , traceShow

    -- * Interpretations
  , runTraceList

  , runTraceListIO

  , runTracePrinting
  , runTraceToHandle

  , ignoreTrace

  , traceIntoTell

    -- * Simple variants of interprations
  , runTraceListIOSimple
  , runTraceToHandleSimple

    -- * Threading constraints
  , WriterThreads

    -- * Carriers
  , TraceListC
  , TracePrintingC
  , IgnoreTraceC
  , TraceIntoTellC
  ) where

import Data.IORef
import Data.Semigroup

import Control.Effect
import Control.Effect.Writer

import System.IO

-- For coercion purposes
import Control.Effect.Carrier
import Control.Effect.Internal.Utils
import Control.Effect.Carrier.Internal.Interpret
import Control.Effect.Carrier.Internal.Compose
import Control.Effect.Carrier.Internal.Intro
import qualified Control.Monad.Trans.Writer.CPS as CPS


-- | An effect for debugging by printing/logging strings.
data Trace :: Effect where
  Trace :: String -> Trace m ()

-- | Log the provided string
trace :: Eff Trace m => String -> m ()
trace = send . Trace
{-# INLINE trace #-}

-- | 'show' the provided item and log it.
traceShow :: (Show a, Eff Trace m) => a -> m ()
traceShow = trace . show
{-# INLINE traceShow #-}

newtype TraceListC m a = TraceListC {
    unTraceListC ::
        TraceIntoTellC
      ( TellListC String
      ( m
      )) a
  } deriving ( Functor, Applicative, Monad
             , Alternative, MonadPlus
             , MonadFix, MonadFail, MonadIO
             , MonadThrow, MonadCatch, MonadMask
             , MonadBase b, MonadBaseControl b
             )
    deriving (MonadTrans, MonadTransControl)
    via CompositionBaseT
     '[ TraceIntoTellC
      , TellListC String
      ]

deriving instance (Carrier m, Threads (CPS.WriterT (Dual [String])) (Prims m))
               => Carrier (TraceListC m)

-- | Run a 'Trace' effect purely by accumulating all 'trace'd strings
-- into a list.
runTraceList :: forall m a p
              . ( Threaders '[WriterThreads] m p
                , Carrier m
                )
             => TraceListC m a
             -> m ([String], a)
runTraceList =
     runTellList
  .# traceIntoTell
  .# unTraceListC
{-# INLINE runTraceList #-}

data TracePrintingH

instance Eff (Embed IO) m
      => Handler TracePrintingH Trace m where
  effHandler (Trace str) = embed $ hPutStrLn stderr str
  {-# INLINEABLE effHandler #-}

type TracePrintingC = InterpretC TracePrintingH Trace

-- | Run a 'Trace' effect by printing each 'trace'd string
-- to stderr.
runTracePrinting :: Eff (Embed IO) m
                 => TracePrintingC m a
                 -> m a
runTracePrinting = interpretViaHandler
{-# INLINE runTracePrinting #-}

-- | Run 'Trace' effect by providing each 'trace'd string
-- to the provided 'Handle'.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'runTraceToHandle' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower 'runTraceToHandleSimple',
-- which doesn't have a higher-rank type.
runTraceToHandle :: Eff (Embed IO) m
                 => Handle
                 -> InterpretReifiedC Trace m a
                 -> m a
runTraceToHandle hdl = interpret $ \case
  Trace str -> embed $ hPutStrLn hdl str
{-# INLINE runTraceToHandle #-}

-- | Run 'Trace' effect by providing each 'trace'd string
-- to the provided 'Handle'.
--
-- This is a less performant version of 'runTraceToHandle' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
runTraceToHandleSimple :: forall m a p
                        . ( Eff (Embed IO) m
                          , Threaders '[ReaderThreads] m p
                          )
                       => Handle
                       -> InterpretSimpleC Trace m a
                       -> m a
runTraceToHandleSimple hdl = interpretSimple $ \case
  Trace str -> embed $ hPutStrLn hdl str
{-# INLINE runTraceToHandleSimple #-}

-- | Run a 'Trace' effect by accumulating all 'trace'd strings
-- into a list using atomic operations in IO.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'runTraceListIO' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower 'runTraceListIOSimple',
-- which doesn't have a higher-rank type.
runTraceListIO :: Eff (Embed IO) m
               => InterpretReifiedC Trace m a
               -> m ([String], a)
runTraceListIO m = do
  ref <- embed (newIORef [])
  a   <- (`interpret` m) $ \case
    Trace o -> embed (atomicModifyIORef' ref (\s -> (o:s, ())))
  s   <- reverse <$> embed (readIORef ref)
  return (s, a)
{-# INLINE runTraceListIO #-}


-- | Run a 'Trace' effect by accumulating all 'trace'd strings
-- into a list using atomic operations in IO.
--
-- This is a less performant version of 'runTraceListIOSimple' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
runTraceListIOSimple :: forall m a p
                      . ( Eff (Embed IO) m
                        , Threaders '[ReaderThreads] m p
                        )
                     => InterpretSimpleC Trace m a
                     -> m ([String], a)
runTraceListIOSimple m = do
  ref <- embed (newIORef [])
  a   <- (`interpretSimple` m) $ \case
    Trace o -> embed (atomicModifyIORef' ref (\s -> (o:s, ())))
  s   <- reverse <$> embed (readIORef ref)
  return (s, a)
{-# INLINE runTraceListIOSimple #-}

data IgnoreTraceH

instance Carrier m
      => Handler IgnoreTraceH Trace m where
  effHandler (Trace _) = pure ()
  {-# INLINEABLE effHandler #-}

type IgnoreTraceC = InterpretC IgnoreTraceH Trace

-- | Run a 'Trace' effect by ignoring it, doing no logging at all.
ignoreTrace :: Carrier m
            => IgnoreTraceC m a
            -> m a
ignoreTrace = interpretViaHandler
{-# INLINE ignoreTrace #-}

data TraceToTellH

instance Eff (Tell String) m
      => Handler TraceToTellH Trace m where
  effHandler (Trace str) = tell str
  {-# INLINEABLE effHandler #-}

type TraceIntoTellC = ReinterpretC TraceToTellH Trace '[Tell String]

-- | Rewrite a 'Trace' effect into a @'Tell' String@ effect on top of the
-- effect stack.
traceIntoTell :: HeadEff (Tell String) m
              => TraceIntoTellC m a
              -> m a
traceIntoTell = reinterpretViaHandler
{-# INLINE traceIntoTell #-}
