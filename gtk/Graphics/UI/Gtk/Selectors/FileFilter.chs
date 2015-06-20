{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget FileFilter
--
--  Author : Duncan Coutts
--
--  Created: 26 February 2005
--
--  Copyright (C) 2005 Duncan Coutts
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
-- A filter for selecting a file subset
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.Selectors.FileFilter (
-- * Detail
--
-- | A 'FileFilter' can be used to restrict the files being shown in a
-- 'FileChooser'. Files can be filtered based on their name (with
-- 'fileFilterAddPattern'), on their mime type (with 'fileFilterAddMimeType'),
-- or by a custom filter function (with 'fileFilterAddCustom').
--
-- Filtering by mime types handles aliasing and subclassing of mime types;
-- e.g. a filter for \"text\/plain\" also matches a file with mime type
-- \"application\/rtf\", since \"application\/rtf\" is a subclass of
-- \"text\/plain\". Note that 'FileFilter' allows wildcards for the subtype of
-- a mime type, so you can e.g. filter for \"image\/\*\".
--
-- Normally, filters are used by adding them to a 'FileChooser', see
-- 'Graphics.UI.Gtk.Selectors.FileChooser.fileChooserAddFilter'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----FileFilter
-- @

#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  FileFilter,
  FileFilterClass,
  castToFileFilter, gTypeFileFilter,
  toFileFilter,
  FileFilterFlags(..),

-- * Constructors
  fileFilterNew,

-- * Methods
  fileFilterSetName,
  fileFilterGetName,
  fileFilterAddMimeType,
  fileFilterAddPattern,
  fileFilterAddCustom,
#if GTK_CHECK_VERSION(2,6,0)
  fileFilterAddPixbufFormats,
#endif

-- * Attributes
  fileFilterName,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Flags                (Flags, fromFlags)
import System.Glib.UTFString
import System.Glib.Attributes
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,4,0)

{# enum FileFilterFlags {underscoreToCase} deriving(Bounded,Show,Eq) #}
instance Flags FileFilterFlags

--------------------
-- Constructors

-- | Creates a new 'FileFilter' with no rules added to it. Such a filter
-- doesn't accept any files, so is not particularly useful until you add rules
-- with 'fileFilterAddMimeType', 'fileFilterAddPattern', or
-- 'fileFilterAddCustom'.
--
fileFilterNew :: IO FileFilter
fileFilterNew =
  makeNewObject mkFileFilter $
  {# call gtk_file_filter_new #}

--------------------
-- Methods

-- | Sets the human-readable name of the filter; this is the string that will
-- be displayed in the file selector user interface if there is a selectable
-- list of filters.
--
fileFilterSetName :: GlibString string
 => FileFilter
 -> string -- ^ @name@ - the human-readable-name for the filter
 -> IO ()
fileFilterSetName self name =
  withUTFString name $ \namePtr ->
  {# call gtk_file_filter_set_name #}
    self
    namePtr

-- | Gets the human-readable name for the filter. See 'fileFilterSetName'.
--
fileFilterGetName :: GlibString string
 => FileFilter
 -> IO string -- ^ returns The human-readable name of the filter
fileFilterGetName self =
  {# call gtk_file_filter_get_name #}
    self
  >>= peekUTFString

-- | Adds a rule allowing a given mime type to @filter@.
--
fileFilterAddMimeType :: GlibString string
 => FileFilter
 -> string     -- ^ @mimeType@ - name of a MIME type
 -> IO ()
fileFilterAddMimeType self mimeType =
  withUTFString mimeType $ \mimeTypePtr ->
  {# call gtk_file_filter_add_mime_type #}
    self
    mimeTypePtr

-- | Adds a rule allowing a shell style glob to a filter.
--
fileFilterAddPattern :: GlibString string
 => FileFilter
 -> string     -- ^ @pattern@ - a shell style glob
 -> IO ()
fileFilterAddPattern self pattern =
  withUTFString pattern $ \patternPtr ->
  {# call gtk_file_filter_add_pattern #}
    self
    patternPtr

-- | Adds rule to a filter that allows files based on a custom callback
-- function. The list of flags @needed@ which is passed in provides information
-- about what sorts of information that the filter function needs; this allows
-- Gtk+ to avoid retrieving expensive information when it isn't needed by the
-- filter.
--
fileFilterAddCustom :: GlibString string => FileFilter
 -> [FileFilterFlags]     -- ^ @needed@ - list of flags indicating the
                          -- information that the custom filter function needs.
 -> (   Maybe string      -- filename
     -> Maybe string      -- uri
     -> Maybe string      -- display name
     -> Maybe string      -- mime type
     -> IO Bool)          -- ^ @(\filename uri displayName mimeType -> ...)@ -
                          -- filter function; if the function
                          -- returns @True@, then the file will be displayed.
 -> IO ()
fileFilterAddCustom self needed func = do
  hPtr <- mkHandler_GtkFileFilterFunc
    (\filterInfoPtr _ -> do
      filenamePtr    <- {# get GtkFileFilterInfo->filename     #} filterInfoPtr
      uriPtr         <- {# get GtkFileFilterInfo->uri          #} filterInfoPtr
      displayNamePtr <- {# get GtkFileFilterInfo->display_name #} filterInfoPtr
      mimeTypePtr    <- {# get GtkFileFilterInfo->mime_type    #} filterInfoPtr
      filename    <- maybePeek peekUTFString filenamePtr
      uri         <- maybePeek peekUTFString uriPtr
      displayName <- maybePeek peekUTFString displayNamePtr
      mimeType    <- maybePeek peekUTFString mimeTypePtr
      liftM fromBool $ func filename uri displayName mimeType)
  {# call gtk_file_filter_add_custom #}
    self
    ((fromIntegral . fromFlags) needed)
    hPtr
    (castFunPtrToPtr hPtr)
    destroyFunPtr

{#pointer *GtkFileFilterInfo as GtkFileFilterInfoPtr #}

type GtkFileFilterFunc =
  GtkFileFilterInfoPtr -> --GtkFileFilterInfo *filter_info
  Ptr () ->               --gpointer user_data
  IO CInt

foreign import ccall "wrapper" mkHandler_GtkFileFilterFunc ::
  GtkFileFilterFunc ->
  IO (FunPtr GtkFileFilterFunc)

#if GTK_CHECK_VERSION(2,6,0)
-- | Adds a rule allowing image files in the formats supported by 'Pixbuf'.
--
-- * Available since Gtk+ version 2.6
--
fileFilterAddPixbufFormats :: FileFilter -> IO ()
fileFilterAddPixbufFormats self =
  {# call gtk_file_filter_add_pixbuf_formats #}
    self
#endif

--------------------
-- Attributes

-- | \'name\' property. See 'fileFilterGetName' and 'fileFilterSetName'
--
fileFilterName :: GlibString string => Attr FileFilter string
fileFilterName = newAttr
  fileFilterGetName
  fileFilterSetName
#endif
