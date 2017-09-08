{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) General
--
--  Author : Axel Simon, Manuel M. T. Chakravarty, Duncan Coutts
--
--  Created: 11 October 2005
--
--  Copyright (C) 2000..2005 Axel Simon, Manuel M. T. Chakravarty, Duncan Coutts
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- main event loop, and events
--
module System.Glib.MainLoop (
  HandlerId,
  timeoutAdd,
  timeoutAddFull,
  timeoutRemove,
  idleAdd,
  idleRemove,
  IOCondition(..),
  inputAdd,
  inputRemove,
  Priority,
  priorityLow,
  priorityDefaultIdle,
  priorityHighIdle,
  priorityDefault,
  priorityHigh,
  MainLoop,
  mainLoopNew,
  mainLoopRun,
  mainLoopQuit,
  mainLoopIsRunning,
  MainContext,
  mainContextNew,
  mainContextDefault,
  mainContextIteration,
  mainContextFindSourceById,
  Source(..),
  sourceAttach,
  sourceSetPriority,
  sourceGetPriority,
  sourceDestroy,
#if GLIB_CHECK_VERSION(2,12,0)
  sourceIsDestroyed
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GObject      (DestroyNotify, destroyFunPtr)

{#context lib="glib" prefix ="g"#}

{#pointer SourceFunc#}

foreign import ccall "wrapper" mkSourceFunc :: (Ptr () -> IO {#type gint#}) -> IO SourceFunc

type HandlerId = {#type guint#}

-- Turn a function into a function pointer and a destructor pointer.
--
makeCallback :: IO {#type gint#} -> IO (SourceFunc, DestroyNotify)
makeCallback fun = do
  funPtr <- mkSourceFunc (const fun)
  return (funPtr, destroyFunPtr)

-- | Sets a function to be called at regular intervals, with the default
-- priority 'priorityDefault'. The function is called repeatedly until it
-- returns @False@, after which point the timeout function will not be called
-- again. The first call to the function will be at the end of the first interval.
--
-- Note that timeout functions may be delayed, due to the processing of other
-- event sources. Thus they should not be relied on for precise timing. After
-- each call to the timeout function, the time of the next timeout is
-- recalculated based on the current time and the given interval (it does not
-- try to 'catch up' time lost in delays).
--
timeoutAdd :: IO Bool -> Int -> IO HandlerId
timeoutAdd fun msec = timeoutAddFull fun priorityDefault msec

-- | Sets a function to be called at regular intervals, with the given
-- priority. The function is called repeatedly until it returns @False@, after
-- which point the timeout function will not be called again. The first call
-- to the function will be at the end of the first interval.
--
-- Note that timeout functions may be delayed, due to the processing of other
-- event sources. Thus they should not be relied on for precise timing. After
-- each call to the timeout function, the time of the next timeout is
-- recalculated based on the current time and the given interval (it does not
-- try to 'catch up' time lost in delays).
--
timeoutAddFull :: IO Bool -> Priority -> Int -> IO HandlerId
timeoutAddFull fun pri msec = do
  (funPtr, dPtr) <- makeCallback (liftM fromBool fun)
  {#call unsafe g_timeout_add_full#}
    (fromIntegral pri)
    (fromIntegral msec)
    funPtr
    (castFunPtrToPtr funPtr)
    dPtr

-- | Remove a previously added timeout handler by its 'HandlerId'.
--
timeoutRemove :: HandlerId -> IO ()
timeoutRemove id = {#call source_remove#} id >> return ()

-- | Add a callback that is called whenever the system is idle.
--
-- * A priority can be specified via an integer. This should usually be
--   'priorityDefaultIdle'.
--
-- * If the function returns @False@ it will be removed.
--
idleAdd :: IO Bool -> Priority -> IO HandlerId
idleAdd fun pri = do
  (funPtr, dPtr) <- makeCallback (liftM fromBool fun)
  {#call unsafe idle_add_full#} (fromIntegral pri) funPtr
    (castFunPtrToPtr funPtr) dPtr

-- | Remove a previously added idle handler by its 'HandlerId'.
--
idleRemove :: HandlerId -> IO ()
idleRemove id = {#call source_remove#} id >> return ()

-- | Flags representing a condition to watch for on a file descriptor.
--
-- [@IOIn@]             There is data to read.
-- [@IOOut@]            Data can be written (without blocking).
-- [@IOPri@]            There is urgent data to read.
-- [@IOErr@]            Error condition.
-- [@IOHup@]            Hung up (the connection has been broken, usually for
--                      pipes and sockets).
-- [@IOInvalid@]        Invalid request. The file descriptor is not open.
--
{# enum IOCondition {
          G_IO_IN   as IOIn,
          G_IO_OUT  as IOOut,
          G_IO_PRI  as IOPri,
          G_IO_ERR  as IOErr,
          G_IO_HUP  as IOHup,
          G_IO_NVAL as IOInvalid
        } deriving (Eq, Bounded) #}
instance Flags IOCondition

{#pointer *IOChannel newtype#}
{#pointer IOFunc#}

foreign import ccall "wrapper" mkIOFunc :: (Ptr IOChannel -> CInt -> Ptr () -> IO {#type gboolean#}) -> IO IOFunc

type FD = Int

-- | Adds the file descriptor into the main event loop with the given priority.
--
inputAdd ::
    FD            -- ^ a file descriptor
 -> [IOCondition] -- ^ the condition to watch for
 -> Priority      -- ^ the priority of the event source
 -> IO Bool       -- ^ the function to call when the condition is satisfied.
                  --   The function should return False if the event source
                  --   should be removed.
 -> IO HandlerId  -- ^ the event source id
inputAdd fd conds pri fun = do
  funPtr <- mkIOFunc (\_ _ _ -> liftM fromBool fun)
  channel <- {#call unsafe g_io_channel_unix_new #} (fromIntegral fd)
  {#call unsafe g_io_add_watch_full#}
    (IOChannel channel)
    (fromIntegral pri)
    ((fromIntegral . fromFlags) conds)
    funPtr
    (castFunPtrToPtr funPtr)
    destroyFunPtr

inputRemove :: HandlerId -> IO ()
inputRemove id = {#call source_remove#} id >> return ()

-- Standard priorities

#define G_PRIORITY_HIGH            -100
#define G_PRIORITY_DEFAULT          0
#define G_PRIORITY_HIGH_IDLE        100
#define G_PRIORITY_DEFAULT_IDLE     200
#define G_PRIORITY_LOW              300

-- | Priorities for installing callbacks.
--
type Priority = Int

priorityHigh :: Int
priorityHigh = G_PRIORITY_HIGH

priorityDefault :: Int
priorityDefault = G_PRIORITY_DEFAULT

priorityHighIdle :: Int
priorityHighIdle = G_PRIORITY_HIGH_IDLE

priorityDefaultIdle :: Int
priorityDefaultIdle = G_PRIORITY_DEFAULT_IDLE

priorityLow :: Int
priorityLow = G_PRIORITY_LOW

-- | A main event loop abstraction.
{# pointer *GMainLoop as MainLoop foreign newtype #}

-- | An opaque datatype representing a set of sources to be handled in
--   a main loop.
{# pointer *GMainContext as MainContext foreign newtype #}

-- | Create a new 'MainLoop'.
mainLoopNew :: Maybe MainContext -- ^ @context@ - the context to use, or 'Nothing' to use the default context
            -> Bool              -- ^ @isRunning@ - 'True' to indicate that the loop is running; 'False' otherwise
            -> IO MainLoop       -- ^ the new 'MainLoop'
mainLoopNew context isRunning =
    do let context' = maybe (MainContext nullForeignPtr) id context
       loopPtr <- {# call main_loop_new #} context' $ fromBool isRunning
       liftM MainLoop $ newForeignPtr loopPtr mainLoopFinalizer
foreign import ccall unsafe "&g_main_loop_unref"
    mainLoopFinalizer :: FunPtr (Ptr MainLoop -> IO ())

-- | Runs a main loop until 'mainLoopQuit' is called on the
--   loop. If this is called for the thread of the loop's
--   'MainContext', it will process events from the loop, otherwise it
--   will simply wait.
mainLoopRun :: MainLoop
            -> IO ()
mainLoopRun loop =
    {# call main_loop_run #} loop

-- | Stops a 'MainLoop' from running. Any calls to mainLoopRun for the
--   loop will return.
mainLoopQuit :: MainLoop
             -> IO ()
mainLoopQuit loop =
    {# call main_loop_quit #} loop

-- | Checks to see if the main loop is currently being run via mainLoopRun.
mainLoopIsRunning :: MainLoop
                  -> IO Bool
mainLoopIsRunning loop =
    liftM toBool $ {# call main_loop_is_running #} loop

-- | Gets a 'MainLoop's context.
mainLoopGetContext :: MainLoop
                   -> MainContext
mainLoopGetContext loop =
    MainContext $ unsafePerformIO $
        {# call main_loop_get_context #} loop >>=
            flip newForeignPtr mainContextFinalizer

foreign import ccall unsafe "&g_main_context_unref"
    mainContextFinalizer :: FunPtr (Ptr MainContext -> IO ())

-- | Creates a new 'MainContext'.
mainContextNew :: IO MainContext
mainContextNew =
    newContextMarshal {# call main_context_new #}

-- | The default 'MainContext'. This is the main context used for main
--   loop functions when a main loop is not explicitly specified.
mainContextDefault :: MainContext
mainContextDefault =
    unsafePerformIO $ newContextMarshal {# call main_context_default #}

newContextMarshal action =
    do ptr <- action
       liftM MainContext $ newForeignPtr ptr mainContextFinalizer

-- | Runs a single iteration for the given main loop. This involves
--   checking to see if any event sources are ready to be processed,
--   then if no events sources are ready and @mayBlock@ is 'True',
--   waiting for a source to become ready, then dispatching the
--   highest priority events sources that are ready. Note that even
--   when @mayBlock@ is 'True', it is still possible for
--   'mainContextIteration' to return 'False', since the the wait
--   may be interrupted for other reasons than an event source
--   becoming ready.
mainContextIteration :: MainContext
                     -> Bool
                     -> IO Bool
mainContextIteration context mayBlock =
    liftM toBool $ {# call main_context_iteration #} context (fromBool mayBlock)

mainContextFindSourceById :: MainContext
                          -> HandlerId
                          -> IO Source
mainContextFindSourceById context id =
    {# call main_context_find_source_by_id #} context (fromIntegral id) >>= newSource . castPtr

{# pointer *GSource as Source foreign newtype #}
newSource :: Ptr Source
          -> IO Source
newSource sourcePtr =
    liftM Source $ newForeignPtr sourcePtr sourceFinalizer
foreign import ccall unsafe "&g_source_unref"
    sourceFinalizer :: FunPtr (Ptr Source -> IO ())

sourceAttach :: Source
             -> MainContext
             -> IO HandlerId
sourceAttach source context =
    liftM fromIntegral $ {# call source_attach #} source context

sourceSetPriority :: Source
                  -> Priority
                  -> IO ()
sourceSetPriority source priority =
    {# call source_set_priority #} source $ fromIntegral priority

sourceGetPriority :: Source
                  -> IO Priority
sourceGetPriority source =
    liftM fromIntegral $ {# call source_get_priority #} source

sourceDestroy :: Source
              -> IO ()
sourceDestroy source =
    {# call source_destroy #} source

#if GLIB_CHECK_VERSION(2,12,0)
sourceIsDestroyed :: Source
                  -> IO Bool
sourceIsDestroyed source =
    liftM toBool $ {# call source_is_destroyed #} source
#endif

sourceRemove :: HandlerId
             -> IO Bool
sourceRemove tag =
    liftM toBool $ {# call source_remove #} $ fromIntegral tag
