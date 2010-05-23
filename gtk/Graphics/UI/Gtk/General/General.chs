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
-- TODO
--
-- quitAddDestroy, quitAdd, quitRemove
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
  IOCondition,
  HandlerId
  ) where

import System.Environment (getProgName, getArgs)
import Control.Monad      (liftM, mapM, when)
import Control.Concurrent (rtsSupportsBoundThreads, newEmptyMVar,
                           putMVar, takeMVar)
import Data.IORef         (IORef, newIORef, readIORef, writeIORef)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.MainLoop
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{#context lib="gtk" prefix ="gtk"#}

{-
-- | Retreive the current language.
-- * This function returns a String which's pointer can be used later on for
--   comarisions.
--
--getDefaultLanguage :: IO String
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
--   to ensure that all calls to Gtk+ happen on a single OS thread.
--   If you want to make calls to Gtk2Hs functions from a Haskell thread other
--   than the one that calls this functions and 'mainGUI' then you will have to
--   \'post\' your GUI actions to the main GUI thread. You can do this using
--   'postGUISync' or 'postGUIAsync'. See also 'threadsEnter'.
--
initGUI :: IO [String]
initGUI = do
  when rtsSupportsBoundThreads initialiseGThreads
  threadsEnter
  prog <- getProgName
  args <- getArgs
  let allArgs = (prog:args)
  withMany withUTFString allArgs $ \addrs  ->
    withArrayLen       addrs   $ \argc argv ->
    with	       argv    $ \argvp ->
    with	       argc    $ \argcp -> do 
      res <- {#call unsafe init_check#} (castPtr argcp) (castPtr argvp)
      if (toBool res) then do
        argc'   <- peek argcp
        argv'   <- peek argvp
        _:addrs'  <- peekArray argc' argv'  -- drop the program name
        mapM peekUTFString addrs'
        else error "Cannot initialize GUI."

-- g_thread_init aborts the whole program if it's called more than once so
-- we've got to keep track of whether or not we've called it already. Sigh.
--
foreign import ccall "hsgthread.h gtk2hs_threads_initialise"
  initialiseGThreads :: IO ()

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

-- | Acquired the global Gtk lock.
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
{#fun unsafe gdk_threads_enter as threadsEnter {} -> `()' #}

-- | Release the global Gtk lock.
--
-- * The use of this function is not recommended. See 'threadsEnter'.
--
{#fun unsafe gdk_threads_leave as threadsLeave {} -> `()' #}

-- | Inquire the number of events pending on the event queue
--
eventsPending :: IO Int
eventsPending  = liftM fromIntegral {#call unsafe events_pending#}

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
