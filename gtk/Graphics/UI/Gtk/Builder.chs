{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) XML Interface Parser
--
--  Author: John Millikin
--
--  Created: 19 November 2009
--
--  Copyright (C) 2009 John Millikin
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
-- Notes:
--
-- Like the @libglade@ bindings, this module does not support signal
-- auto-connection.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Build an interface from an XML UI definition
--
-- All functions in this module are only available in Gtk 2.12 or higher.
--
module Graphics.UI.Gtk.Builder
#if !GTK_CHECK_VERSION(2,12,0)
  () where
#else
  (
-- * Detail
--
-- A 'Builder' is an auxiliary object that reads textual descriptions of a
-- user interface and instantiates the described objects. To pass a
-- description to a 'Builder', perform 'builderAddFromFile' or
-- 'builderAddFromString'. These computations can be performed multiple
-- times; the builder merges the content of all descriptions.
--
-- A 'Builder' holds a reference to all objects that it has constructed and
-- drops these references when it is finalized. This finalization can cause
-- the destruction of non-widget objects or widgets which are not contained
-- in a toplevel window. For toplevel windows constructed by a builder, it
-- is the responsibility of the user to perform 'widgetDestroy' to get rid
-- of them and all the widgets they contain.
--
-- The computations 'builderGetObject' and 'builderGetObjects' can be used
-- to access the widgets in the interface by the names assigned to them
-- inside the UI description. Toplevel windows returned by these functions
-- will stay around until the user explicitly destroys them with
-- 'widgetDestroy'. Other widgets will either be part of a larger hierarchy
-- constructed by the builder (in which case you should not have to worry
-- about their lifecycle), or without a parent, in which case they have to
-- be added to some container to make use of them. Non-widget objects need
-- to be reffed with 'objectRef' to keep them beyond the lifespan of the
-- builder.
--
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'GtkBuilder'
-- @

-- * Types
    Builder
  , BuilderClass
  , castToBuilder
  , gTypeBuilder
  , toBuilder
  , BuilderError (..)

-- * Constructing and adding objects
  , builderNew
  , builderAddFromFile
  , builderAddFromString
#if GTK_CHECK_VERSION(2,14,0)
  , builderAddObjectsFromFile
  , builderAddObjectsFromString
#endif

-- * Retrieving objects
  , builderGetObject
  , builderGetObjects
  , builderGetObjectRaw
  , builderSetTranslationDomain
  , builderGetTranslationDomain
  ) where

import Control.Exception (evaluate, throwIO, ErrorCall (..))
import System.Glib.FFI
import System.Glib.GError
import System.Glib.GList
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

{# enum GtkBuilderError as BuilderError {underscoreToCase} deriving (Show, Eq) #}

---------------------------------------
-- Constructing and adding objects

-- | Creates a new 'Builder' object.
builderNew :: IO Builder
builderNew =
  wrapNewGObject mkBuilder $
  {# call unsafe builder_new #}

-- | Parses a file containing a GtkBuilder UI definition and merges it with
-- the current contents of the 'Builder'.
--
-- * If an error occurs, the computation will throw an exception that can
--   be caught using e.g. 'System.Glib.GError.catchGErrorJust' and one of the
--   error codes in 'BuilderError'.
--
builderAddFromFile :: GlibFilePath fp => Builder -> fp -> IO ()
builderAddFromFile builder path =
  propagateGError $ \errPtrPtr ->
  withUTFFilePath path $ \pathPtr ->
  {# call unsafe builder_add_from_file #}
    builder pathPtr errPtrPtr
    >> return ()

-- | Parses a string containing a GtkBuilder UI definition and merges it
-- with the current contents of the 'Builder'.
--
-- * If an error occurs, the computation will throw an exception that can
--   be caught using e.g. 'System.Glib.GError.catchGErrorJust' and one of the
--   error codes in 'BuilderError'.
--
builderAddFromString :: GlibString string => Builder -> string -> IO ()
builderAddFromString builder str =
  propagateGError $ \errPtrPtr ->
  withUTFStringLen str $ \(strPtr, strLen) ->
  {# call unsafe builder_add_from_string #}
    builder strPtr (fromIntegral strLen) errPtrPtr
    >> return ()

#if GTK_CHECK_VERSION(2,14,0)
-- | Parses a file containing a GtkBuilder UI definition building only
-- the requested objects and merges them with the current contents of
-- the 'Builder'.
--
-- * If an error occurs, the computation will throw an exception that can
--   be caught using e.g. 'System.Glib.GError.catchGErrorJust' and one of the
--   error codes in 'BuilderError'.
--
builderAddObjectsFromFile :: (GlibString string, GlibFilePath fp)
 => Builder
 -> fp
 -> [string] -- ^ Object IDs
 -> IO ()
builderAddObjectsFromFile builder path ids =
  propagateGError $ \errPtrPtr ->
  withUTFFilePath path $ \pathPtr ->
  withUTFStringArray0 ids $ \idsPtr ->
  {# call unsafe builder_add_objects_from_file #}
    builder pathPtr idsPtr errPtrPtr
    >> return ()

-- | Parses a string containing a GtkBuilder UI definition building only
-- the requested objects and merges them with the current contents of
-- the 'Builder'.
--
-- * If an error occurs, the computation will throw an exception that can
--   be caught using e.g. 'System.Glib.GError.catchGErrorJust' and one of the
--   error codes in 'BuilderError'.
--
builderAddObjectsFromString :: GlibString string
 => Builder
 -> string
 -> [string] -- ^ Object IDs
 -> IO ()
builderAddObjectsFromString builder str ids =
  propagateGError $ \errPtrPtr ->
  withUTFStringLen str $ \(strPtr, strLen) ->
  withUTFStringArray0 ids $ \idsPtr ->
  {# call unsafe builder_add_objects_from_string #}
    builder strPtr (fromIntegral strLen) idsPtr errPtrPtr
    >> return ()
#endif

---------------------------------------
-- Retrieving objects

-- | Gets the object with the given name. Note that this computation does
-- not increment the reference count of the returned object.
builderGetObjectRaw :: GlibString string => Builder
 -> string           -- The ID of the object in the UI file, eg \"button1\".
 -> IO (Maybe GObject)
builderGetObjectRaw builder name =
  withUTFString name $ \namePtr ->
  maybeNull (makeNewGObject mkGObject) $
  {# call unsafe builder_get_object #}
    builder namePtr

-- | Gets the object with the given name, with a conversion function. Note
-- that this computation does not increment the reference count of the
-- returned object.
--
-- If the object with the given ID is not of the requested type, an
-- exception will be thrown.
--
builderGetObject :: (GObjectClass cls, GlibString string) =>
    Builder
 -> (GObject -> cls) -- ^ A dynamic cast function which returns an object
                     -- of the expected type, eg 'castToButton'
 -> string           -- The ID of the object in the UI file, eg \"button1\".
 -> IO cls
builderGetObject builder cast name = do
  raw <- builderGetObjectRaw builder name
  case raw of
    Just obj -> evaluate . cast $ obj
    Nothing  -> throwIO . ErrorCall $
      "Gtk.Builder.builderGetObject: no object named " ++ show name ++ " in the builder."

-- | Gets all objects that have been constructed by builder. Note that this
-- computation does not increment the reference counts of the returned
-- objects.
builderGetObjects :: Builder -> IO [GObject]
builderGetObjects builder =
  {# call unsafe builder_get_objects #}
    builder
    >>= readGSList
    >>= mapM (makeNewGObject mkGObject . return)

-- | Sets the translation domain of the 'Builder'.
builderSetTranslationDomain :: GlibString string => Builder -> Maybe string -> IO ()
builderSetTranslationDomain builder domain =
  maybeWith withUTFString domain $ \domainPtr ->
  {# call unsafe builder_set_translation_domain #}
    builder domainPtr

-- | Gets the translation domain of the 'Builder'.
builderGetTranslationDomain :: GlibString string => Builder -> IO (Maybe string)
builderGetTranslationDomain builder =
  {# call unsafe builder_get_translation_domain #}
    builder
    >>= maybePeek peekUTFString

#endif
