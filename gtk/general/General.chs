{-# OPTIONS -cpp #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry General@
--
--  Author : Axel Simon
--	     Manuel M. T. Chakravarty
--
--  Created: 8 December 1998
--
--  Version $Revision: 1.11 $ from $Date: 2003/07/09 22:42:44 $
--
--  Copyright (c) [2000..2002] Axel Simon
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- @description@ --------------------------------------------------------------
--
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
-- 
--  * quitAddDestroy, quitAdd, quitRemove, inputAdd, inputRemove
--
module General(
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
  mkDestructor,
  DestroyNotify,
  priorityLow,
  priorityDefault,
  priorityHigh,
  timeoutAdd,
  timeoutRemove,
  idleAdd,
  idleRemove,
  HandlerId
  ) where

import Prelude
       hiding   (init)
import System   (getProgName, getArgs, ExitCode(ExitSuccess, ExitFailure))
import Monad	(liftM, mapM)
import FFI

import LocalData(newIORef, readIORef, writeIORef)
import Exception (ioError, Exception(ErrorCall))
import Object	(makeNewObject)
{#import Hierarchy#}	 
{#import Signal#}
import Enums    (InputCondition(..))
import Structs	(priorityLow, priorityDefault, priorityHigh)

{#context lib="gtk" prefix ="gtk"#}

-- @function getDefaultLanguage@ Retreive the current language.
-- * This function returns a String which's pointer can be used later on for
--   comarisions.
--
--getDefaultLanguage :: IO String
--getDefaultLanguage = do
--  strPtr <- {#call unsafe get_default_language#}
--  str <- peekUTFString strPtr
--  destruct strPtr
--  return str


-- @function initGUI@ Initialize the GUI binding.
--
-- * This function initialized the GUI toolkit and parses all Gtk
--   specific arguments. The remaining arguments are returned. If the
--   initialization of the toolkit fails for whatever reason, an exception
--   is thrown.
--
-- * Throws: @literal ErrorCall "Cannot initialize GUI."@
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

-- @function eventsPending@ Inquire the number of events pending on the event
-- queue
--
eventsPending :: IO Int
eventsPending  = liftM fromIntegral {#call unsafe events_pending#}

-- @function mainGUI@ Run GTK+'s main event loop.
--
mainGUI :: IO ()
mainGUI  = {#call main#}

-- @function mainLevel@ Inquire the main loop level.
--
-- * Callbacks that take more time to process can call 
--   @ref function loopIteration@ to keep the GUI responsive. Each time
--   the main loop is restarted this way, the main loop counter is
--   increased. This function returns this counter.
--
mainLevel :: IO Int
mainLevel  = liftM (toEnum.fromEnum) {#call unsafe main_level#}

-- @function mainQuit@ Exit the main event loop.
--
mainQuit :: IO ()
mainQuit  = {#call main_quit#}

-- @function mainIteration@ Process an event, block if necessary.
--
-- * Returns @literal True@ if the @ref function loopQuit@ was called while
--   processing the event.
--
mainIteration :: IO Bool
mainIteration  = liftM toBool {#call main_iteration#}

-- @function mainIterationDo@ Process a single event.
--
-- * Called with @literal True@, this function behaves as
--   @ref function loopIteration@ in that it waits until an event is available
--   for processing. The function will return immediately, if passed
--   @literal False@.
--
-- * Returns @literal True@ if the @ref function loopQuit@ was called while
--   processing the event.
--

--
mainIterationDo :: Bool -> IO Bool
mainIterationDo blocking = 
  liftM toBool $ {#call main_iteration_do#} (fromBool blocking)

-- @function grabAdd@ add a grab widget
--
grabAdd :: WidgetClass wd => wd -> IO ()
grabAdd  = {#call grab_add#} . toWidget

-- @function grabGetCurrent@ inquire current grab widget
--
grabGetCurrent :: IO (Maybe Widget)
grabGetCurrent  = do
  wPtr <- {#call grab_get_current#}
  if (wPtr==nullPtr) then return Nothing else 
    liftM Just $ makeNewObject mkWidget (return wPtr)

-- @function grabRemove@ remove a grab widget
--
grabRemove :: WidgetClass w => w -> IO ()
grabRemove  = {#call grab_remove#} . toWidget

{#pointer Function#}

{#pointer DestroyNotify#}

#if __GLASGOW_HASKELL__>=600

foreign import ccall "wrapper" mkHandler :: IO {#type gint#} -> IO Function

foreign import ccall "wrapper" mkDestructor :: IO () -> IO DestroyNotify

#else

foreign export dynamic mkHandler :: IO {#type gint#} -> IO Function

foreign export dynamic mkDestructor :: IO () -> IO DestroyNotify

#endif

type HandlerId = {#type guint#}

-- Turn a function into a function pointer and a destructor pointer.
--
makeCallback :: IO {#type gint#} -> IO (Function, DestroyNotify)
makeCallback fun = do
  funPtr <- mkHandler fun
  dRef <- newIORef nullFunPtr
  dPtr <- mkDestructor $ do
    freeHaskellFunPtr funPtr
    dPtr <- readIORef dRef
    freeHaskellFunPtr dPtr
  writeIORef dRef dPtr
  return (funPtr, dPtr)

-- @function timeoutAdd@ Register a function that is to be called after
-- @ref arg interval@ ms have been elapsed.
--
-- * If the function returns @literal False@ it will be removed.
--
timeoutAdd :: IO Bool -> Int -> IO HandlerId
timeoutAdd fun msec = do
  (funPtr, dPtr) <- makeCallback (liftM fromBool fun)
  {#call unsafe timeout_add_full#} (fromIntegral msec) funPtr nullFunPtr 
    nullPtr dPtr

-- @function timeoutRemove@ Remove a previously added timeout handler by its
-- @ref type TimeoutId@.
--
timeoutRemove :: HandlerId -> IO ()
timeoutRemove  = {#call unsafe timeout_remove#}

-- @function idleAdd@ Add a callback that is called whenever the system is
-- idle.
--
-- * A priority can be specified via an integer. This should usually be
--   @ref constant priorityDefault@.
--
-- * If the function returns @literal False@ it will be removed.
--
idleAdd :: IO Bool -> Int -> IO HandlerId
idleAdd fun pri = do
  (funPtr, dPtr) <- makeCallback (liftM fromBool fun)
  {#call unsafe idle_add_full#} (fromIntegral pri) funPtr nullFunPtr
    nullPtr dPtr

-- @function idleRemove@ Remove a previously added idle handler by its
-- @ref type TimeoutId@.
--
idleRemove :: HandlerId -> IO ()
idleRemove  = {#call unsafe idle_remove#}

