{-# OPTIONS -cpp #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) GConf API
--
--  Author : Duncan Coutts
--  Created: 16 April 2004
--
--  Copyright (c) 2004 Duncan Coutts
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
-- |
--
--  GConf is a system for maintaining program configuration information.
--  The main difference between GConf and traditional configuration file
--  API's is that GConf is 'live'. Applications can be
--  notified of changes in their configuration, it allows desktop wide setting
--  to be propogated without restarting all applications, or multiple instances
--  of a single application to synchronise their configuration. It is similar
--  in many ways to the Win32 Registry with its directory-like structure.
--
--  This module only binds the client library which is the only interface that
--  normal applications should need. Only special GConf apps need lower level
--  access.
--
--  * Some low level functions have not been bound
--  eg @gconf_client_get_for_engine@
--

module System.Gnome.GConf.GConfClient (

  -- * Data types
  --
  GConf, GConfPreloadType, GConfError,

  -- * Creation operation
  --
  gconfGetDefault,
  
  -- * Registering for change notifications
  --
  gconfAddDir, gconfRemoveDir,
  gconfNotifyAdd, gconfNotifyRemove,
  onValueChanged, afterValueChanged,
  
  -- * Getting and setting configuration values
  --
  gconfGet, gconfSet,
--  gconfGetFloat,
--  gconfGetInt,
--  gconfGetString,
--  gconfGetBool,
--  gconfGetSchema,
--  gconfGetList,
--  gconfGetPair,
  gconfGetWithoutDefault,
  gconfGetDefaultFromSchema,
  gconfUnset,

  -- * Caching control
  --
  gconfClearCache, gconfPreload,
  gconfSuggestSync,

  -- * Navigation
  --
  gconfAllEntries,
  gconfAllDirs,
  gconfDirExists,

  -- * GConfValue
  --
  GConfValueClass,
  GConfPrimitiveValueClass,
  GConfValue,
  GConfValueDyn(..),
  
) where

import Monad	(liftM, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.GError   (GErrorDomain, GErrorClass(..), propagateGError)
import System.Glib.GObject  (makeNewGObject)
{#import System.Gnome.GConf.Signals#}
{#import System.Gnome.GConf.Types#}
{#import System.Gnome.GConf.GConfValue#}

{# context lib="gconf" prefix ="gconf_client" #}

{# enum GConfClientPreloadType as GConfPreloadType {underscoreToCase} #}

-- | The "GError" exceptions that can be thrown by functions in this module
{# enum GConfError {underscoreToCase} #}

gconfErrorDomain :: GErrorDomain
gconfErrorDomain = unsafePerformIO {#call unsafe gconf_error_quark#}

instance GErrorClass GConfError where
  gerrorDomain _ = gconfErrorDomain

{# pointer *GConfEntry newtype #}

-- Operations

-- Creation operations

-- | Create a new GConf object using the default configuration engine.
--
gconfGetDefault :: IO GConf
gconfGetDefault =
  makeNewGObject mkGConf {# call gconf_client_get_default #}


-- Registering for change notifications

-- | Add a directory to the list of directories the
-- GConf will watch. You should use gconfNotifyAdd to associate
-- change handlers with specific keys.
--
-- * Added directories may not overlap. That is, if you add \"\/foo\", you may
-- not add \"\/foo\/bar\". However you can add \"\/foo\" and \"\/bar\". You can
-- also add \"\/foo\" multiple times; if you add a directory multiple times, it
-- will not be removed until you call 'gconfRemoveDir' an equal number of
-- times.
--
-- * Note that the watch is recursive, all keys below the given directory will
--   be watched. So it would be a bad idea to watch the root \"\/\".
--
gconfAddDir :: GConf -> String -> IO ()
gconfAddDir gc key = gconfAddDirWithPreload gc key PreloadNone

-- | Like 'gconfAddDir' but with the option to specify a preload mode.
--
-- As a rule of thumb, if you plan to get the value of almost all the keys in a
-- directory, preloading that directory will probably enhance performance. If
-- you plan to use only half the keys, preloading is likely a bad idea.
-- 
-- * PreloadNone specifies that no preload occurs
-- * PreloadOnelevel loads the immediate children of the directory
-- * PreloadRecursive loads all children of the directory and its
--   subdirectories, recursively.
-- 
gconfAddDirWithPreload :: GConf -> String -> GConfPreloadType -> IO ()
gconfAddDirWithPreload gc key preload =
  propagateGError $ \gerrorPtr ->
  withCString key $ \strPtr ->
  {# call gconf_client_add_dir #} gc strPtr
    (fromIntegral $ fromEnum preload) gerrorPtr

-- | Remove a directory from the list created with 'gconfAddDir'. If any
-- notifications have been added below this directory with 'gconfNotifyAdd',
-- those notifications will be disabled until you re-add the removed directory.
--
-- * Note that if a directory has been added multiple times, you must remove it
-- the same number of times before the remove takes effect.
--
gconfRemoveDir :: GConf -> String -> IO ()
gconfRemoveDir gc key =
  propagateGError $ \gerrorPtr ->
  withCString key $ \strPtr ->    
    {# call gconf_client_remove_dir #} gc strPtr gerrorPtr


{#pointer GFreeFunc#}

foreign import ccall "wrapper" mkDestructor :: IO () -> IO GFreeFunc

type GConfClientNotifyFunc = Ptr () ->         --GConfClient *client
                             {#type guint#} -> --guint cnxn_id
                             Ptr () ->         --GConfEntry *entry
                             Ptr () ->         --gpointer user_data
                             IO ()

foreign import ccall "wrapper" mkHandler_GConfClientNotifyFunc ::
  GConfClientNotifyFunc -> IO (FunPtr GConfClientNotifyFunc)

connect_GConfClientNotifyFunc :: 
  GConf ->
  String ->
  (GConfEntry -> IO ()) ->
  IO GConfConnectId
connect_GConfClientNotifyFunc gc key user = do
  hPtr <- mkHandler_GConfClientNotifyFunc
    (\_ _ entryPtr _ -> user (GConfEntry $ castPtr entryPtr))
  dRef <- newIORef nullFunPtr
  dPtr <- mkDestructor $ do
    freeHaskellFunPtr hPtr
    dPtr <- readIORef dRef
    freeHaskellFunPtr dPtr
  writeIORef dRef dPtr
  cnxId <- propagateGError $ \gerrorPtr ->
           withCString key $ \strPtr ->
           {# call gconf_client_notify_add #} gc strPtr
             (castFunPtr hPtr) nullPtr dPtr gerrorPtr
  return $ GConfConnectId cnxId

newtype GConfConnectId = GConfConnectId {#type guint#}

gconfNotifyAdd :: GConfValueClass value =>
                        GConf ->
                        String ->
                        (String -> value -> IO ()) ->
				                IO GConfConnectId
gconfNotifyAdd gc key handler =
  connect_GConfClientNotifyFunc gc key (convertValueChangedHandler handler)
  where convertValueChangedHandler :: GConfValueClass value =>
                                      (String -> value -> IO ()) ->
                                      (GConfEntry -> IO ())
        convertValueChangedHandler handler entry = do
          keyStrPtr <- {# call unsafe gconf_entry_get_key #} entry
          valuePtr <- {# call unsafe gconf_entry_get_value #} entry
          key <- peekUTFString keyStrPtr
          value <- marshalFromGConfValue (GConfValue valuePtr)
          handler key value

gconfNotifyRemove :: GConf -> GConfConnectId -> IO ()
gconfNotifyRemove gc (GConfConnectId cxid) =
  {# call gconf_client_notify_remove #} gc cxid


-- Getting and setting configuration values

-- | Gets the value of a configuration key.
--
gconfGet :: GConfValueClass value => GConf
         -> String   -- ^ Name of the key
	 -> IO value
gconfGet gc key = do
  value <- propagateGError $ \gerrorPtr ->
           withCString key $ \strPtr ->
           {# call gconf_client_get #} gc strPtr gerrorPtr
  marshalFromGConfValue (GConfValue value)
  
gconfGetInt :: GConf -> String -> IO Int
gconfGetInt = gconfGet

gconfGetBool :: GConf -> String -> IO Bool
gconfGetBool = gconfGet

gconfGetFloat :: GConf -> String -> IO Double
gconfGetFloat = gconfGet

gconfGetString :: GConf -> String -> IO String
gconfGetString = gconfGet

gconfGetPair :: GConfValueClass (a,b) =>
                GConf -> String -> IO (a, b)
gconfGetPair = gconfGet

gconfGetList :: GConfValueClass [a] =>
                GConf -> String -> IO [a]
gconfGetList = gconfGet

-- | Sets the value of a configuration key.
--
gconfSet :: GConfValueClass value => GConf
         -> String  -- ^ Name of the key
	 -> value   -- ^ New value
	 -> IO ()
gconfSet gc key val = do
  value@(GConfValue ptr) <- marshalToGConfValue val
  if ptr == nullPtr
    then gconfUnset gc key
    else propagateGError $ \gerrorPtr ->
         withCString key $ \strPtr ->
         {# call gconf_client_set #} gc strPtr value gerrorPtr

gconfSetInt :: GConf -> String -> Int -> IO ()
gconfSetInt = gconfSet

gconfSetBool :: GConf -> String -> Bool -> IO ()
gconfSetBool = gconfSet

gconfSetFloat :: GConf -> String -> Double -> IO ()
gconfSetFloat = gconfSet

gconfSetString :: GConf -> String -> String -> IO ()
gconfSetString = gconfSet

gconfSetPair :: GConfValueClass (a,b) =>
                GConf -> String -> (a, b) -> IO ()
gconfSetPair = gconfSet

gconfSetList :: GConfValueClass [a] =>
                GConf -> String -> [a] -> IO ()
gconfSetList = gconfSet

-- | Gets the value of a configuration key.
--
-- Same as 'gconfGet', but doesn't look for a default value if the key is
-- unset.
--
gconfGetWithoutDefault :: GConfValueClass value =>
                                GConf -> String -> IO value
gconfGetWithoutDefault gc key = do
  value <- propagateGError $ \gerrorPtr ->
           withCString key $ \strPtr ->
           {# call gconf_client_get_without_default #} gc strPtr gerrorPtr
  marshalFromGConfValue (GConfValue value)

-- | Returns the default value stored in the key's schema, if the key has a
-- schema associated and the schema exists and the schema contains a default
-- value. Note that 'gconfSet' already returns the default value if no other
-- value is found, so normally you do not need this function.
--
gconfGetDefaultFromSchema :: GConfValueClass value =>
                             GConf -> String -> IO value
gconfGetDefaultFromSchema gc key = do
  value <- propagateGError $ \gerrorPtr ->
           withCString key $ \strPtr ->
           {# call gconf_client_get_default_from_schema #} gc strPtr gerrorPtr
  marshalFromGConfValue (GConfValue value)

-- | Unsets the value of key; if key is already unset, has no effect. An error
-- of note is 'GConfOverridden', indicating that the system administrator has
-- \"forced\" a value for this key.
--
gconfUnset :: GConf -> String -> IO ()
gconfUnset gc key =
  propagateGError $ \gerrorPtr ->
  withCString key $ \strPtr -> do
  {# call gconf_client_unset #} gc strPtr gerrorPtr
  return ()

-- | Dumps everything out of the GConf client-side cache. If you know you're
-- done using the GConf for a while, you can call this function to save some
-- memory.
--
gconfClearCache :: GConf -> IO ()
gconfClearCache gc = {# call gconf_client_clear_cache #} gc

-- | Preloads a directory. Normally you do this when you call
-- 'gconfAddDirWithPreload', but if you've called 'gconfClearCache' there may
-- be a reason to do it again.
--
gconfPreload :: GConf -> String -> GConfPreloadType -> IO ()
gconfPreload gc key preload =
  propagateGError $ \gerrorPtr ->
  withCString key $ \strPtr ->
  {# call gconf_client_preload #} gc strPtr
    (fromIntegral $ fromEnum preload) gerrorPtr

-- | Suggests to gconfd that you've just finished a block of changes, and it
-- would be an optimal time to sync to permanent storage. This is only a
-- suggestion; and gconfd will eventually sync even if you don't call 
-- 'gconfSuggestSync'. This function is just a "hint" provided to gconfd to
-- maximize efficiency and minimize data loss.
--
gconfSuggestSync :: GConf -> IO ()
gconfSuggestSync gc =
  propagateGError $ \gerrorPtr ->
  {# call gconf_client_suggest_sync #} gc gerrorPtr

-- |  
--
gconfAllEntries :: GConf -> String -> IO [(String, GConfValueDyn)]
gconfAllEntries gc dir = do
  gsList <- propagateGError $ \gerrorPtr ->
            withCString dir $ \strPtr ->
            {# call gconf_client_all_entries #} gc strPtr gerrorPtr
  entryList <- fromGSList gsList
  mapM (\entry -> do let entry' = GConfEntry entry
                     keyStrPtr <- {# call unsafe gconf_entry_get_key #} entry'
                     valuePtr <- {# call unsafe gconf_entry_get_value #} entry'
                     key <- peekUTFString keyStrPtr
                     value <- marshalFromGConfValue (GConfValue valuePtr)
                     -- gconf_entry_free is depreciated, use gconf_entry_unref
                     -- however gconf_entry_unref is not documented and docs
                     -- still say to use gconf_entry_free. Confusing.
                     {# call unsafe gconf_entry_free #} entry'
                     return (key,value))
       entryList

-- | 
--
gconfAllDirs :: GConf -> String -> IO [String]
gconfAllDirs gc dir = do
  gsList <- withCString dir $ \strPtr ->
    {# call gconf_client_all_dirs #} gc strPtr nullPtr
  dirList <- fromGSList gsList
  mapM (\strPtr -> do str <- peekUTFString strPtr
                      {# call unsafe g_free #} (castPtr strPtr)
                      return str)
       dirList

-- | 
--
gconfDirExists :: GConf -> String -> IO Bool
gconfDirExists gc dir =
  withCString dir $ \strPtr ->
  liftM toBool $ {# call gconf_client_dir_exists #} gc strPtr nullPtr


-- Signals

onValueChanged, afterValueChanged :: GConf ->
                                     (String -> Maybe GConfValueDyn -> IO ()) ->
				                             IO (ConnectId GConf)
onValueChanged gc handler =
  connect_STRING_PTR__NONE "value_changed" False gc
  (convertValueChangedHandler handler)
afterValueChanged gc handler =
  connect_STRING_PTR__NONE "value_changed" True gc
  (convertValueChangedHandler handler)

convertValueChangedHandler :: (String -> Maybe GConfValueDyn -> IO ()) ->
                              (String -> Ptr GConfValue -> IO ())
convertValueChangedHandler handler key ptr = do
  value <- marshalFromGConfValue (GConfValue $ castPtr ptr)
  handler key value
