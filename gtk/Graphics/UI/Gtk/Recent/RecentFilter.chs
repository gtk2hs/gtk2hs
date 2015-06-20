{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget RecentFilter
--
--  Author : Andy Stewart
--
--  Created: 27 Mar 2010
--
--  Copyright (C) 2010 Andy Stewart
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
-- A filter for selecting a subset of recently used files
--
-- * Module available since Gtk+ version 2.10
--
module Graphics.UI.Gtk.Recent.RecentFilter (

-- * Detail
--
-- | A 'RecentFilter' can be used to restrict the files being shown in a
-- 'RecentChooser'. Files can be filtered based on their name (with
-- 'recentFilterAddPattern'), on their mime type (with
-- 'fileFilterAddMimeType'), on the application that has registered them (with
-- 'recentFilterAddApplication'), or by a custom filter function (with
-- 'recentFilterAddCustom').
--
-- Filtering by mime type handles aliasing and subclassing of mime types;
-- e.g. a filter for text\/plain also matches a file with mime type
-- application\/rtf, since application\/rtf is a subclass of text\/plain. Note
-- that 'RecentFilter' allows wildcards for the subtype of a mime type, so you
-- can e.g. filter for image\/.
--
-- Normally, filters are used by adding them to a 'RecentChooser', see
-- 'recentChooserAddFilter', but it is also possible to manually use a filter
-- on a file with 'recentFilterFilter'.
--
-- Recently used files are supported since Gtk+ 2.10.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----RecentFilter
-- @

#if GTK_CHECK_VERSION(2,10,0)
-- * Types
  RecentFilter,
  RecentFilterClass,
  castToRecentFilter,
  toRecentFilter,

-- * Enums
  RecentFilterFlags(..),

-- * Constructors
  recentFilterNew,

-- * Methods
  recentFilterGetName,
  recentFilterSetName,
  recentFilterAddMimeType,
  recentFilterAddPattern,
  recentFilterAddPixbufFormats,
  recentFilterAddApplication,
  recentFilterAddGroup,
  recentFilterAddAge,
#endif
  ) where

#if GTK_CHECK_VERSION(2,10,0)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

---------------------
-- Enums
-- | These flags indicate what parts of a 'RecentFilterInfo' struct are filled or need to be filled.
{#enum RecentFilterFlags {underscoreToCase} deriving (Bounded,Eq,Show)#}

--------------------
-- Constructors

-- | Creates a new 'RecentFilter' with no rules added to it. Such filter does
-- not accept any recently used resources, so is not particularly useful until
-- you add rules with 'recentFilterAddPattern', 'recentFilterAddMimeType',
-- 'recentFilterAddApplication', 'recentFilterAddAge'. To create a filter that
-- accepts any recently used resource, use:
--
-- > filter <- recentFilterNew
-- > recentFilterAddPattern filter "*"
--
-- * Available since Gtk+ version 2.10
--
recentFilterNew :: IO RecentFilter
recentFilterNew =
  makeNewObject mkRecentFilter $
  {# call gtk_recent_filter_new #}

--------------------
-- Methods
-- | Gets the human-readable name for the filter. See 'recentFilterSetName'.
--
-- * Available since Gtk+ version 2.10
--
recentFilterGetName :: (RecentFilterClass self, GlibString string) => self
 -> IO string -- ^ returns the name of the filter
recentFilterGetName self =
  {# call gtk_recent_filter_get_name #}
    (toRecentFilter self)
  >>= peekUTFString

-- | Sets the human-readable name of the filter; this is the string that will be displayed in the
-- recently used resources selector user interface if there is a selectable list of filters.
--
-- * Available since Gtk+ version 2.10
--
recentFilterSetName :: (RecentFilterClass self, GlibString string) => self
 -> string -- ^ @name@ - then human readable name of @filter@
 -> IO ()
recentFilterSetName self name =
  withUTFString name $ \namePtr ->
  {# call gtk_recent_filter_set_name #}
    (toRecentFilter self)
    namePtr

-- | Adds a rule that allows resources based on their registered MIME type.
--
-- * Available since Gtk+ version 2.10
--
recentFilterAddMimeType :: (RecentFilterClass self, GlibString string) => self
 -> string -- ^ @mimeType@ - a MIME type
 -> IO ()
recentFilterAddMimeType self mimeType =
  withUTFString mimeType $ \mimeTypePtr ->
  {# call gtk_recent_filter_add_mime_type #}
    (toRecentFilter self)
    mimeTypePtr

-- | Adds a rule that allows resources based on a pattern matching their
-- display name.
--
-- * Available since Gtk+ version 2.10
--
recentFilterAddPattern :: (RecentFilterClass self, GlibString string) => self
 -> string -- ^ @pattern@ - a file pattern
 -> IO ()
recentFilterAddPattern self pattern =
  withUTFString pattern $ \patternPtr ->
  {# call gtk_recent_filter_add_pattern #}
    (toRecentFilter self)
    patternPtr

-- | Adds a rule allowing image files in the formats supported by 'Pixbuf'.
--
recentFilterAddPixbufFormats :: RecentFilterClass self => self -> IO ()
recentFilterAddPixbufFormats self =
  {# call gtk_recent_filter_add_pixbuf_formats #}
    (toRecentFilter self)

-- | Adds a rule that allows resources based on the name of the application
-- that has registered them.
--
--
-- * Available since Gtk+ version 2.10
--
recentFilterAddApplication :: (RecentFilterClass self, GlibString string) => self
 -> string -- ^ @application@ - an application name
 -> IO ()
recentFilterAddApplication self application =
  withUTFString application $ \applicationPtr ->
  {# call gtk_recent_filter_add_application #}
    (toRecentFilter self)
    applicationPtr

-- | Adds a rule that allows resources based on the name of the group to which
-- they belong
--
--
-- * Available since Gtk+ version 2.10
--
recentFilterAddGroup :: (RecentFilterClass self, GlibString string) => self
 -> string -- ^ @group@ - a group name
 -> IO ()
recentFilterAddGroup self group =
  withUTFString group $ \groupPtr ->
  {# call gtk_recent_filter_add_group #}
    (toRecentFilter self)
    groupPtr

-- | Adds a rule that allows resources based on their age - that is, the
-- number of days elapsed since they were last modified.
--
-- * Available since Gtk+ version 2.10
--
recentFilterAddAge :: RecentFilterClass self => self
 -> Int -- ^ @days@ - number of days
 -> IO ()
recentFilterAddAge self days =
  {# call gtk_recent_filter_add_age #}
    (toRecentFilter self)
    (fromIntegral days)
#endif
