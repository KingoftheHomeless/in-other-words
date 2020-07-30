module Control.Effect.Trace
  ( -- * Effect
    Trace(..)

    -- * Actions
  , trace
  , traceShow

    -- * Interpretations
  , TraceListC
  , runTraceList

  , runTraceListIO

  , runTracePrinting
  , runTraceToHandle

  , IgnoreTraceC
  , ignoreTrace

  , TraceIntoTellC
  , traceIntoTell

    -- * Simple variants of interprations
  , runTraceListIOSimple
  , runTraceToHandleSimple

    -- * Threading constraints
  , WriterThreads
  ) where

import Data.IORef

import Control.Effect
import Control.Effect.Writer

import System.IO

-- For coercion purposes
import Control.Effect.Internal.Utils
import Control.Effect.Carrier.Internal.Interpret
import Control.Effect.Carrier.Internal.Compose
import Control.Effect.Carrier.Internal.Intro
import Control.Monad.Trans.Identity


data Trace m a where
  Trace :: String -> Trace m ()

trace :: Eff Trace m => String -> m ()
trace = send . Trace
{-# INLINE trace #-}

traceShow :: (Show a, Eff Trace m) => a -> m ()
traceShow = trace . show
{-# INLINE traceShow #-}

type TraceListC = CompositionC
 '[ TraceIntoTellC
  , TellListC String
  ]

runTraceList :: forall m a p
              . ( Threaders '[WriterThreads] m p
                , Carrier m
                )
             => TraceListC m a
             -> m ([String], a)
runTraceList =
     runTellList
  .# traceIntoTell
  .# runComposition
{-# INLINE runTraceList #-}

data TracePrintingH

instance Eff (Embed IO) m
      => Handler TracePrintingH Trace m where
  effHandler (Trace str) = embed $ hPutStrLn stderr str
  {-# INLINE effHandler #-}

type TracePrintingC = InterpretC TracePrintingH Trace

runTracePrinting :: Eff (Embed IO) m
                 => TracePrintingC m a
                 -> m a
runTracePrinting = interpretViaHandler
{-# INLINE runTracePrinting #-}

runTraceToHandle :: Eff (Embed IO) m
                 => Handle
                 -> InterpretReifiedC Trace m a
                 -> m a
runTraceToHandle hdl = interpret $ \case
  Trace str -> embed $ hPutStrLn hdl str
{-# INLINE runTraceToHandle #-}

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
  {-# INLINE effHandler #-}

type IgnoreTraceC = InterpretC IgnoreTraceH Trace

ignoreTrace :: Carrier m
            => IgnoreTraceC m a
            -> m a
ignoreTrace = interpretViaHandler
{-# INLINE ignoreTrace #-}

data TraceToTellH

instance Eff (Tell String) m
      => Handler TraceToTellH Trace m where
  effHandler (Trace str) = tell str
  {-# INLINE effHandler #-}

type TraceIntoTellC = ReinterpretC TraceToTellH Trace '[Tell String]

traceIntoTell :: HeadEff (Tell String) m
              => TraceIntoTellC m a
              -> m a
traceIntoTell = reinterpretViaHandler
{-# INLINE traceIntoTell #-}
