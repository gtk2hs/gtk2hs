-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: General initialization
--
--  Author : Manuel M. T. Chakravarty
--  Created: 8 December 1998
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
--
--  Copyright (c) [1998..2001] Manuel M. T. Chakravarty
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
--- DESCRIPTION ---------------------------------------------------------------
--
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------
-- 
--  * quitAddDestroy, quitAdd, quitRemove, timeoutAdd, timeoutRemove, idleAdd,
--    idleRemove, inputAdd, inputRemove
--
{-# OPTIONS -optc-include gtk/gtk.h #-}
module General(
  setLocale,
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
  DestroyNotify
  ) where

import Prelude
       hiding   (init)
import System   (getProgName, getArgs, ExitCode(ExitSuccess, ExitFailure))
import Monad	(liftM, mapM)
import Foreign
import UTFCForeign
import IOExts	(newIORef, readIORef, writeIORef)
import Object	(makeNewObject)
{#import Hierarchy#}	 
{#import Signal#}
import Enums    (InputCondition(..))

{#context lib="gtk" prefix ="gtk"#}

-- Ensure that the current locale is read. (EXPORTED)
--
setLocale :: IO String
setLocale = do
  strPtr <- {#call unsafe set_locale#}
  str <- peekCString strPtr
  destruct strPtr
  return str


-- Retreive the current language. (EXPORTED)
--
-- * This function returns a String which's pointer can be used later on
--   for comarisions.
--
--getDefaultLanguage :: IO String
--getDefaultLanguage = do
--  strPtr <- {#call unsafe get_default_language#}
--  str <- peekCString strPtr
--  destruct strPtr
--  return str

-- initialize GTK+ (EXPORTED)
--
-- * extracts all GTK+ specific arguments from the given options list
--
init                     :: Maybe (String, [String]) -> IO (String, [String])
init Nothing              = do
  prog <- getProgName
  args <- getArgs
  init $ Just (prog, args)
init (Just (prog, args))  = do
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

-- Inquire the number of events pending on the event queue (EXPORTED)
--
eventsPending :: IO Bool
eventsPending  = liftM toBool {#call unsafe events_pending#}

-- GTK+'s main event loop (EXPORTED)
--
main :: IO ()
main  = {#call main#}

-- Inquire the main level (EXPORTED)
--
mainLevel :: IO Int
mainLevel  = liftM (toEnum.fromEnum) {#call unsafe main_level#}

-- Exit the main event loop (EXPORTED)
--
mainQuit :: IO ()
mainQuit  = {#call main_quit#}

-- process events (EXPORTED)
--
mainIteration :: IO Bool
mainIteration  = liftM toBool {#call main_iteration#}

-- process events (EXPORTED)
--
mainIterationDo          :: Bool -> IO Bool
mainIterationDo blocking  = 
  liftM toBool $ {#call main_iteration_do#} (fromBool blocking)

--  add a grab widget (EXPORTED)
--
grabAdd :: WidgetClass wd =>  wd -> IO ()
grabAdd  = {#call grab_add#} . toWidget

-- inquire current grab widget (EXPORTED)
--
grabGetCurrent :: IO (Maybe Widget)
grabGetCurrent  = do
  wPtr <- {#call grab_get_current#}
  if (wPtr==nullPtr) then return Nothing else 
    liftM Just $ makeNewObject mkWidget (return wPtr)

--  remove a grab widget (EXPORTED)
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

-- Register a function that is to be called after @interval ms have been
-- elapsed. (EXPORTED)
--
-- * If the function returns False it will be removed.
--
timeoutAdd :: Int -> IO Bool -> IO HandlerId
timeoutAdd msec fun = do
  (funPtr, dPtr) <- makeCallback (liftM fromBool fun)
  {#call unsafe timeout_add_full#} (fromIntegral msec) funPtr nullFunPtr 
    nullPtr dPtr

-- Remove a previously added timeout handler by its @TimeoutId. (EXPORTED)
--
timeoutRemove :: HandlerId -> IO ()
timeoutRemove = {#call unsafe timeout_remove#}

-- Add a callback that is called whenever the system is idle. (EXPORTED)
--
-- * A priority can be specified.
--
idleAdd :: Int -> IO Bool -> IO HandlerId
idleAdd pri fun = do
  (funPtr, dPtr) <- makeCallback (liftM fromBool fun)
  {#call unsafe idle_add_full#} (fromIntegral pri) funPtr nullFunPtr
    nullPtr dPtr

-- Remove a previously added idle handler by its @TimeoutId. (EXPORTED)
--
idleRemove :: HandlerId -> IO ()
idleRemove = {#call unsafe idle_remove#}

