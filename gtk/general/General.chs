-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry General initialization@
--
--  Author : Manuel M. T. Chakravarty
--  Created: 8 December 1998
--
--  Version $Revision: 1.5 $ from $Date: 2002/07/21 16:07:17 $
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
{-# OPTIONS -optc-include gtk/gtk.h #-}
module General(
--  getDefaultLanguage,
  init,
  eventsPending,
  main,
  mainLevel,
  mainQuit,
  mainIteration,
  grabAdd,
  grabGetCurrent,
  grabRemove,
  mkDestructor,
  DestroyNotify,
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
import Foreign
import UTFCForeign
import LocalData(newIORef, readIORef, writeIORef)
import Object	(makeNewObject)
{#import Hierarchy#}	 
{#import Signal#}
import Enums    (InputCondition(..))

{#context lib="gtk" prefix ="gtk"#}

-- @dunno@Retreive the current language.
-- * This function returns a String which's pointer can be used later on for
--   comarisions.
--
-- *  @literal@
--getDefaultLanguage :: IO String
--getDefaultLanguage = do
--  strPtr <- {#call unsafe get_default_language#}
--  str <- peekCString strPtr
--  destruct strPtr
--  return str


-- @method init@ initialize GTK+
--
-- * extracts all GTK+ specific arguments from the given options list
--
init :: Maybe (String, [String]) -> IO (String, [String])
init Nothing = do
  prog <- getProgName
  args <- getArgs
  init $ Just (prog, args)
init (Just (prog, args)) = do
  let allArgs = (prog:args)
      argc    = length allArgs
  withMany withCString allArgs $ \addrs  ->
    withArray	       addrs   $ \argv ->
    withObject	       argv    $ \argvp ->
    withObject	       argc    $ \argcp ->
    do 
--      {#call unsafe init#} argcp argvp
      {#call unsafe init#} (castPtr argcp) (castPtr argvp)
      argc'   <- peek argcp
      argv'   <- peek argvp
      addrs'  <- peekArray argc' argv'
      _:args' <- mapM peekCString addrs'  -- drop the program name
      return (prog, args')

-- @method eventsPending@ Inquire the number of events pending on the event
-- queue
--
eventsPending :: IO Int
eventsPending  = liftM fromIntegral {#call unsafe events_pending#}

-- @method main@ GTK+'s main event loop
--
main :: IO ()
main  = {#call main#}

-- @method mainLevel@ Inquire the main level
--
mainLevel :: IO Int
mainLevel  = liftM (toEnum.fromEnum) {#call unsafe main_level#}

-- @method mainQuit@ Exit the main event loop
--
mainQuit :: IO ()
mainQuit  = {#call main_quit#}

-- @method mainIteration@ process events
--
mainIteration :: IO Bool
mainIteration  = liftM toBool {#call main_iteration#}

-- @method mainIterationDo@ process events
--
mainIterationDo :: Bool -> IO Bool
mainIterationDo blocking = 
  liftM toBool $ {#call main_iteration_do#} (fromBool blocking)

-- @method grabAdd@ add a grab widget
--
grabAdd :: WidgetClass wd => wd -> IO ()
grabAdd  = {#call grab_add#} . toWidget

-- @method grabGetCurrent@ inquire current grab widget
--
grabGetCurrent :: IO (Maybe Widget)
grabGetCurrent  = do
  wPtr <- {#call grab_get_current#}
  if (wPtr==nullPtr) then return Nothing else 
    liftM Just $ makeNewObject mkWidget (return wPtr)

-- @method grabRemove@ remove a grab widget
--
grabRemove :: WidgetClass w => w -> IO ()
grabRemove  = {#call grab_remove#} . toWidget

{#pointer Function#}

foreign export dynamic mkHandler :: IO {#type gint#} -> IO Function

{#pointer DestroyNotify#}

foreign export dynamic mkDestructor :: IO () -> IO DestroyNotify

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

-- @method timeoutAdd@ Register a function that is to be called after
-- @ref arg interval@ ms have been elapsed.
--
-- * If the function returns False it will be removed.
--
timeoutAdd :: IO Bool -> Int -> IO HandlerId
timeoutAdd fun msec = do
  (funPtr, dPtr) <- makeCallback (liftM fromBool fun)
  {#call unsafe timeout_add_full#} (fromIntegral msec) funPtr nullFunPtr 
    nullPtr dPtr

-- @method timeoutRemove@ Remove a previously added timeout handler by its
-- @ref type TimeoutId@.
--
timeoutRemove :: HandlerId -> IO ()
timeoutRemove  = {#call unsafe timeout_remove#}

-- @method idleAdd@ Add a callback that is called whenever the system is idle.
--
-- * A priority can be specified.
--
idleAdd :: IO Bool -> Int -> IO HandlerId
idleAdd fun pri = do
  (funPtr, dPtr) <- makeCallback (liftM fromBool fun)
  {#call unsafe idle_add_full#} (fromIntegral pri) funPtr nullFunPtr
    nullPtr dPtr

-- @method idleRemove@ Remove a previously added idle handler by its
-- @ref arg TimeoutId@.
--
idleRemove :: HandlerId -> IO ()
idleRemove  = {#call unsafe idle_remove#}

