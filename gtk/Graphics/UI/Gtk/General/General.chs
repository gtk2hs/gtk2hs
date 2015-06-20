{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) General
--
--  Author : Axel Simon, Manuel M. T. Chakravarty
--
--  Created: 8 December 1998
--
--  Copyright (C) 2000..2005 Axel Simon, Manuel M. T. Chakravarty
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
-- library initialization, main event loop, and events
--
module Graphics.UI.Gtk.General.General (
--  getDefaultLanguage,
  -- * Initialisation
  initGUI,

  -- ** Support for OS threads
  unsafeInitGUIForThreadedRTS,
  postGUISync,
  postGUIAsync,
  threadsEnter,
  threadsLeave,

  -- * Main event loop
  mainGUI,
  mainQuit,

  -- ** Less commonly used event loop functions
  eventsPending,
  mainLevel,
  mainIteration,
  mainIterationDo,
  mainDoEvent,

  -- ** Call when mainloop is left
#if GTK_MAJOR_VERSION < 3
  quitAddDestroy,
  quitAdd,
  quitRemove,
#endif

  -- * Grab widgets
  grabAdd,
  grabGetCurrent,
  grabRemove,

  -- * Timeout and idle callbacks
  Priority,
  priorityLow,
  priorityDefaultIdle,
  priorityHighIdle,
  priorityDefault,
  priorityHigh,
  timeoutAdd,
  timeoutAddFull,
  timeoutRemove,
  idleAdd,
  idleRemove,
  inputAdd,
  inputRemove,
  IOCondition(..),
  HandlerId,
  FD
  ) where

import Control.Applicative
import Prelude
import System.Environment (getProgName, getArgs)
import Control.Monad      (liftM, when)
import Control.Concurrent (rtsSupportsBoundThreads, newEmptyMVar,
                           putMVar, takeMVar)

import System.Glib.FFI
import System.Glib.UTFString
import qualified System.Glib.MainLoop as ML
import System.Glib.MainLoop ( Priority, priorityLow, priorityDefaultIdle,
  priorityHighIdle, priorityDefault, priorityHigh, timeoutRemove, idleRemove,
  inputRemove, IOCondition(..), HandlerId )
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
import Graphics.UI.Gtk.Gdk.EventM (EventM)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
{#import Graphics.UI.Gtk.Types#}

{#context lib="gtk" prefix ="gtk"#}

{-
-- | Retreive the current language.
-- * This function returns a String which's pointer can be used later on for
--   comarisions.
--
--getDefaultLanguage :: GlibString string => IO string
--getDefaultLanguage = do
--  strPtr <- {#call unsafe get_default_language#}
--  str <- peekUTFString strPtr
--  destruct strPtr
--  return str
-}

unsafeInitGUIForThreadedRTS = initGUI

-- We compile this module using -#includ"gtk/wingtk.h" to bypass the win32 abi
-- check however we do not compile users programs with this header so if
-- initGUI was ever inlined in a users program, then that program would not
-- bypass the abi check and would fail on startup. So to stop that we must
-- prevent initGUI from being inlined.
{-# NOINLINE initGUI #-}
-- | Initialize the GUI.
--
-- This must be called before any other function in the Gtk2Hs library.
--
-- This function initializes the GUI toolkit and parses all Gtk
-- specific arguments. The remaining arguments are returned. If the
-- initialization of the toolkit fails for whatever reason, an exception
-- is thrown.
--
-- * Throws: @error \"Cannot initialize GUI.\"@
--
--
-- * If you want to use Gtk2Hs and in a multi-threaded application then it is your obligation
--   to ensure that all calls to Gtk+ happen in a single OS thread.
--   If you want to make calls to Gtk2Hs functions from a Haskell thread other
--   than the one that calls this functions and 'mainGUI' then you will have to
--   \'post\' your GUI actions to the main GUI thread. You can do this using
--   'postGUISync' or 'postGUIAsync'. See also 'threadsEnter'.
--
initGUI :: IO [String]
initGUI = do
  initialise
  when rtsSupportsBoundThreads initialiseGThreads
  -- note: initizliseGThreads calls 'threadsEnter'
  prog <- getProgName
  args <- getArgs
  let allArgs = (prog:args)
  withMany withUTFString (map stringToGlib allArgs) $ \addrs  ->
    withArrayLen       addrs   $ \argc argv ->
    with               argv    $ \argvp ->
    with               argc    $ \argcp -> do
      res <- {#call unsafe init_check#} (castPtr argcp) (castPtr argvp)
      if (toBool res) then do
        argc'   <- peek argcp
        argv'   <- peek argvp
        _:addrs'  <- peekArray argc' argv'  -- drop the program name
        mapM ((glibToString <$>) . peekUTFString) addrs'
        else error "Cannot initialize GUI."

-- g_thread_init aborts the whole program if it's called more than once so
-- we've got to keep track of whether or not we've called it already. Sigh.
--
foreign import ccall "hsgthread.h gtk2hs_threads_initialise"
  initialiseGThreads :: IO ()

foreign import ccall "hsgthread.h gtk2hs_initialise"
  initialise :: IO ()

-- | Post an action to be run in the main GUI thread.
--
-- The current thread blocks until the action completes and the result is
-- returned.
--
postGUISync :: IO a -> IO a
postGUISync action = do
  resultVar <- newEmptyMVar
  idleAdd (action >>= putMVar resultVar >> return False) priorityDefault
  takeMVar resultVar

-- | Post an action to be run in the main GUI thread.
--
-- The current thread continues and does not wait for the result of the
-- action.
--
postGUIAsync :: IO () -> IO ()
postGUIAsync action = do
  idleAdd (action >> return False) priorityDefault
  return ()

-- | Acquire the global Gtk lock.
--
-- * During normal operation, this lock is held by the thread from which all
--   interaction with Gtk is performed. When calling 'mainGUI', the thread will
--   release this global lock before it waits for user interaction. During this
--   time it is, in principle, possible to use a different OS thread (any other
--   Haskell thread that is bound to the Gtk OS thread will be blocked anyway)
--   to interact with Gtk by explicitly acquiring the lock, calling Gtk functions
--   and releasing the lock. However, the Gtk functions that are called from this
--   different thread may not trigger any calls to the OS since this will
--   lead to a crash on Windows (the Win32 API can only be used from a single
--   thread). Since it is very hard to tell which function only interacts on
--   Gtk data structures and which function call actual OS functions, it
--   is best not to use this feature at all. A better way to perform updates
--   in the background is to spawn a Haskell thread and to perform the update
--   to Gtk widgets using 'postGUIAsync' or 'postGUISync'. These will execute
--   their arguments from the main loop, that is, from the OS thread of Gtk,
--   thereby ensuring that any Gtk and OS function can be called.
--
{#fun gdk_threads_enter as threadsEnter {} -> `()' #}

-- | Release the global Gtk lock.
--
-- * The use of this function is not recommended. See 'threadsEnter'.
--
{#fun unsafe gdk_threads_leave as threadsLeave {} -> `()' #}

-- | Inquire the number of events pending on the event queue
--
eventsPending :: IO Int
eventsPending  = liftM fromIntegral {#call events_pending#}

-- | Run the Gtk+ main event loop.
--
mainGUI :: IO ()
mainGUI  = {#call main#}

-- | Inquire the main loop level.
--
-- * Callbacks that take more time to process can call 'mainIteration' to keep
-- the GUI responsive. Each time the main loop is restarted this way, the main
-- loop counter is increased. This function returns this counter.
--
mainLevel :: IO Int
mainLevel  = liftM (toEnum.fromEnum) {#call unsafe main_level#}

-- | Exit the main event loop.
--
mainQuit :: IO ()
mainQuit  = {#call main_quit#}

-- | Process an event, block if necessary.
--
-- * Returns @True@ if 'mainQuit' was called while processing the event.
--
mainIteration :: IO Bool
mainIteration  = liftM toBool {#call main_iteration#}

-- | Process a single event.
--
-- * Called with @True@, this function behaves as 'mainIteration' in that it
-- waits until an event is available for processing. It will return
-- immediately, if passed @False@.
--
-- * Returns @True@ if the 'mainQuit' was called while processing the event.
--
mainIterationDo :: Bool -> IO Bool
mainIterationDo blocking =
  liftM toBool $ {#call main_iteration_do#} (fromBool blocking)

-- | Processes a single GDK event. This is public only to allow filtering of events between GDK and
-- GTK+. You will not usually need to call this function directly.
--
-- While you should not call this function directly, you might want to know how exactly events are
-- handled. So here is what this function does with the event:
--
--  1. Compress enter\/leave notify events. If the event passed build an enter\/leave pair together with
--     the next event (peeked from GDK) both events are thrown away. This is to avoid a backlog of
--     (de-)highlighting widgets crossed by the pointer.
--
--  2. Find the widget which got the event. If the widget can't be determined the event is thrown away
--     unless it belongs to a INCR transaction. In that case it is passed to
--     'selectionIncrEvent'.
--
--  3. Then the event is passed on a stack so you can query the currently handled event with
--  'getCurrentEvent'.
--
--  4. The event is sent to a widget. If a grab is active all events for widgets that are not in the
--     contained in the grab widget are sent to the latter with a few exceptions:
--
--       * Deletion and destruction events are still sent to the event widget for obvious reasons.
--
--       * Events which directly relate to the visual representation of the event widget.
--
--       * Leave events are delivered to the event widget if there was an enter event delivered to it
--         before without the paired leave event.
--
--       * Drag events are not redirected because it is unclear what the semantics of that would be.
--
--     Another point of interest might be that all key events are first passed through the key snooper
--     functions if there are any. Read the description of 'keySnooperInstall' if you need this
--     feature.
--
--  5. After finishing the delivery the event is popped from the event stack.
mainDoEvent :: EventM t ()
mainDoEvent = do
  ptr <- ask
  liftIO $ {#call main_do_event #} (castPtr ptr)

#if GTK_MAJOR_VERSION < 3
-- | Trigger destruction of object in case the mainloop at level @mainLevel@ is quit.
--
-- Removed in Gtk3.
quitAddDestroy :: ObjectClass obj
                 => Int -- ^ @mainLevel@ Level of the mainloop which shall trigger the destruction.
                 -> obj -- ^ @object@     Object to be destroyed.
                 -> IO ()
quitAddDestroy mainLevel obj =
  {#call quit_add_destroy #}
     (fromIntegral mainLevel)
     (toObject obj)

-- | Registers a function to be called when an instance of the mainloop is left.
--
-- Removed in Gtk3.
quitAdd :: Int -- ^ @mainLevel@ Level at which termination the function shall be called. You can pass 0 here to have the function run at the current mainloop.
        -> (IO Bool) -- ^ @function@   The function to call. This should return 'False' to be removed from the list of quit handlers. Otherwise the function might be called again.
        -> IO Int -- ^ returns    A handle for this quit handler (you need this for 'quitRemove')
quitAdd mainLevel func = do
  funcPtr <- mkGtkFunction $ \ _ ->
    liftM fromBool func
  liftM fromIntegral $
            {#call quit_add #}
              (fromIntegral mainLevel)
              funcPtr
              nullPtr

{#pointer GtkFunction#}

foreign import ccall "wrapper" mkGtkFunction ::
  (Ptr () -> IO {#type gboolean#}) -> IO GtkFunction

-- | Removes a quit handler by its identifier.
--
-- Removed in Gtk3.
quitRemove :: Int -- ^ @quitHandlerId@ Identifier for the handler returned when installing it.
           -> IO ()
quitRemove quitHandlerId =
  {#call quit_remove #} (fromIntegral quitHandlerId)
#endif

-- | add a grab widget
--
grabAdd :: WidgetClass wd => wd -> IO ()
grabAdd  = {#call grab_add#} . toWidget

-- | inquire current grab widget
--
grabGetCurrent :: IO (Maybe Widget)
grabGetCurrent  = do
  wPtr <- {#call grab_get_current#}
  if (wPtr==nullPtr) then return Nothing else
    liftM Just $ makeNewObject mkWidget (return wPtr)

-- | remove a grab widget
--
grabRemove :: WidgetClass w => w -> IO ()
grabRemove  = {#call grab_remove#} . toWidget

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
-- This function differs from 'ML.timeoutAdd' in that the action will
-- be executed within the global Gtk+ lock. It is therefore possible to
-- call Gtk+ functions from the action.
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
-- This function differs from 'ML.timeoutAddFull' in that the action will
-- be executed within the global Gtk+ lock. It is therefore possible to
-- call Gtk+ functions from the action.
--
timeoutAddFull :: IO Bool -> Priority -> Int -> IO HandlerId
timeoutAddFull fun pri msec =
  ML.timeoutAddFull (threadsEnter >> fun >>= \r -> threadsLeave >> return r)
                    pri msec

-- | Add a callback that is called whenever the system is idle.
--
-- * A priority can be specified via an integer. This should usually be
--   'priorityDefaultIdle'.
--
-- * If the function returns @False@ it will be removed.
--
-- This function differs from 'ML.idleAdd' in that the action will
-- be executed within the global Gtk+ lock. It is therefore possible to
-- call Gtk+ functions from the action.
--
idleAdd :: IO Bool -> Priority -> IO HandlerId
idleAdd fun pri =
  ML.idleAdd (threadsEnter >> fun >>= \r -> threadsLeave >> return r) pri

type FD = Int

-- | Adds the file descriptor into the main event loop with the given priority.
--
-- This function differs from 'ML.inputAdd' in that the action will
-- be executed within the global Gtk+ lock. It is therefore possible to
-- call Gtk+ functions from the action.
--
inputAdd ::
    FD            -- ^ a file descriptor
 -> [IOCondition] -- ^ the condition to watch for
 -> Priority      -- ^ the priority of the event source
 -> IO Bool       -- ^ the function to call when the condition is satisfied.
                  --   The function should return False if the event source
                  --   should be removed.
 -> IO HandlerId  -- ^ the event source id
inputAdd fd conds pri fun =
  ML.inputAdd fd conds pri (threadsEnter >> fun >>= \r -> threadsLeave >> return r)
