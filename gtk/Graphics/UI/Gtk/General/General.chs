-- -*-haskell-*-
--  GIMP Toolkit (GTK) General
--
--  Author : Axel Simon, Manuel M. T. Chakravarty
--
--  Created: 8 December 1998
--
--  Version $Revision: 1.7 $ from $Date: 2005/05/14 01:50:40 $
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
-- TODO
-- 
--  * quitAddDestroy, quitAdd, quitRemove, inputAdd, inputRemove
--
module Graphics.UI.Gtk.General.General (
--  getDefaultLanguage,
  initGUI,
  eventsPending,
  mainGUI,
  mainLevel,
  mainQuit,
  mainIteration,
  mainIterationDo,
  grabAdd,
  grabGetCurrent,
  grabRemove,
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
  HandlerId
  ) where

import System   (getProgName, getArgs, ExitCode(ExitSuccess, ExitFailure))
import Monad	(liftM, mapM)
import Data.IORef	 (newIORef, readIORef, writeIORef)
import Control.Exception (ioError, Exception(ErrorCall))

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject		(DestroyNotify, mkFunPtrDestroyNotify)
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}	 
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums    (InputCondition(..))
import Graphics.UI.Gtk.General.Structs	(priorityLow, priorityDefaultIdle,
					priorityHighIdle, priorityDefault,
					priorityHigh)

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

-- We compile this module using -#includ"gtk/wingtk.h" to bypass the win32 abi
-- check however we do not compile users programs with this headder so if
-- initGUI was ever inlined in a users program, then that program would not
-- bypass the abi check and would fail on startup. So to stop that we must
-- prevent initGUI from being inlined.
{-# NOINLINE initGUI #-}
-- | Initialize the GUI binding.
--
-- * This function initialized the GUI toolkit and parses all Gtk
--   specific arguments. The remaining arguments are returned. If the
--   initialization of the toolkit fails for whatever reason, an exception
--   is thrown.
--
-- * Throws: @ErrorCall "Cannot initialize GUI."@
--
initGUI :: IO [String]
initGUI = do
  prog <- getProgName
  args <- getArgs
  let allArgs = (prog:args)
      argc    = length allArgs
  withMany withUTFString allArgs $ \addrs  ->
    withArray	       addrs   $ \argv ->
    withObject	       argv    $ \argvp ->
    withObject	       argc    $ \argcp -> do 
      res <- {#call unsafe init_check#} (castPtr argcp) (castPtr argvp)
      if (toBool res) then do
        argc'   <- peek argcp
        argv'   <- peek argvp
        _:addrs'  <- peekArray argc' argv'  -- drop the program name
        mapM peekUTFString addrs'
        else error "Cannot initialize GUI."

-- | Inquire the number of events pending on the event
-- queue
--
eventsPending :: IO Int
eventsPending  = liftM fromIntegral {#call unsafe events_pending#}

-- | Run GTK+'s main event loop.
--
mainGUI :: IO ()
mainGUI  = {#call main#}

-- | Inquire the main loop level.
--
-- * Callbacks that take more time to process can call 
--   'loopIteration' to keep the GUI responsive. Each time
--   the main loop is restarted this way, the main loop counter is
--   increased. This function returns this counter.
--
mainLevel :: IO Int
mainLevel  = liftM (toEnum.fromEnum) {#call unsafe main_level#}

-- | Exit the main event loop.
--
mainQuit :: IO ()
mainQuit  = {#call main_quit#}

-- | Process an event, block if necessary.
--
-- * Returns @True@ if the 'loopQuit' was called while
--   processing the event.
--
mainIteration :: IO Bool
mainIteration  = liftM toBool {#call main_iteration#}

-- | Process a single event.
--
-- * Called with @True@, this function behaves as
--   'loopIteration' in that it waits until an event is available
--   for processing. The function will return immediately, if passed
--   @False@.
--
-- * Returns @True@ if the 'loopQuit' was called while
--   processing the event.
--

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

{#pointer GSourceFunc as Function#}

foreign import ccall "wrapper" mkHandler :: IO {#type gint#} -> IO Function

type HandlerId = {#type guint#}

-- Turn a function into a function pointer and a destructor pointer.
--
makeCallback :: IO {#type gint#} -> IO (Function, DestroyNotify)
makeCallback fun = do
  funPtr <- mkHandler fun
  dPtr <- mkFunPtrDestroyNotify funPtr
  return (funPtr, dPtr)

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
timeoutAddFull :: IO Bool -> Int -> Int -> IO HandlerId
timeoutAddFull fun pri msec = do
  (funPtr, dPtr) <- makeCallback (liftM fromBool fun)
  {#call unsafe g_timeout_add_full#}
    (fromIntegral pri)
    (fromIntegral msec)
    funPtr
    nullPtr
    dPtr

-- | Remove a previously added timeout handler by its
-- 'TimeoutId'.
--
timeoutRemove :: HandlerId -> IO ()
timeoutRemove id = {#call unsafe g_source_remove#} id >> return ()

-- | Add a callback that is called whenever the system is
-- idle.
--
-- * A priority can be specified via an integer. This should usually be
--   'priorityDefaultIdle'.
--
-- * If the function returns @False@ it will be removed.
--
idleAdd :: IO Bool -> Int -> IO HandlerId
idleAdd fun pri = do
  (funPtr, dPtr) <- makeCallback (liftM fromBool fun)
  {#call unsafe g_idle_add_full#} (fromIntegral pri) funPtr
    nullPtr dPtr

-- | Remove a previously added idle handler by its
-- 'TimeoutId'.
--
idleRemove :: HandlerId -> IO ()
idleRemove id = {#call unsafe g_source_remove#} id >> return ()

