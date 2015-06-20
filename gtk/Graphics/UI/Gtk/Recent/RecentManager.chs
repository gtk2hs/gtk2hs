{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget RecentManager
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
-- Managing Recently Used Files
--
-- * Module available since Gtk+ version 2.10
--
-- TODO:
--      GtkRecentData
--      gtk_recent_manager_add_full
--
module Graphics.UI.Gtk.Recent.RecentManager (

-- * Detail
--
-- | 'RecentManager' provides a facility for adding, removing and looking up
-- recently used files. Each recently used file is identified by its URI, and
-- has meta-data associated to it, like the names and command lines of the
-- applications that have registered it, the number of time each application
-- has registered the same file, the mime type of the file and whether the file
-- should be displayed only by the applications that have registered it.
--
-- The 'RecentManager' acts like a database of all the recently used files.
-- You can create new 'RecentManager' objects, but it is more efficient to use
-- the standard recent manager for the 'Screen' so that informations about the
-- recently used files is shared with other people using them. In case the
-- default screen is being used, adding a new recently used file is as simple
-- as:
--
-- Recently used files are supported since Gtk+ 2.10.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----RecentManager
-- @

#if GTK_CHECK_VERSION(2,10,0)
-- * Types
  RecentManager,
  RecentManagerClass,
  castToRecentManager,
  toRecentManager,

-- * Constructors
  recentManagerNew,

-- * Methods
  recentManagerGetDefault,
  recentManagerAddItem,
  recentManagerRemoveItem,
  recentManagerLookupItem,
  recentManagerHasItem,
  recentManagerMoveItem,
  recentManagerGetItems,
  recentManagerPurgeItems,

-- * Attributes
  recentManagerFilename,
  recentManagerLimit,
  recentManagerSize,

-- * Signals
  recentManagerChanged,
#endif
  ) where

#if GTK_CHECK_VERSION(2,10,0)

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GList
import System.Glib.UTFString
import System.Glib.GError   (propagateGError, checkGError)
{#import Graphics.UI.Gtk.Recent.RecentInfo#} (RecentInfo, mkRecentInfo)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new recent manager object. Recent manager objects are used to
-- handle the list of recently used resources. A 'RecentManager' object
-- monitors the recently used resources list, and emits the \"changed\" signal
-- each time something inside the list changes.
--
-- * Available since Gtk+ version 2.10
--
recentManagerNew :: IO RecentManager
recentManagerNew =
  wrapNewGObject mkRecentManager $
  {# call gtk_recent_manager_new #}

--------------------
-- Methods

-- | Gets a unique instance of 'RecentManager'.
--
-- * Available since Gtk+ version 2.10
--
recentManagerGetDefault :: IO RecentManager -- ^ returns A unique 'RecentManager'.
recentManagerGetDefault =
  makeNewGObject mkRecentManager $
  {# call gtk_recent_manager_get_default #}

-- | Adds a new resource, pointed by @uri@, into the recently used resources
-- list.
--
-- This function automatically retrieves some of the needed metadata and
-- setting other metadata to common default values; it then feeds the data to
-- 'recentManagerAddFull'.
--
-- See 'recentManagerAddFull' if you want to explicitly define the metadata
-- for the resource pointed by @uri@.
--
--
-- * Available since Gtk+ version 2.10
--
recentManagerAddItem :: (RecentManagerClass self, GlibString string) => self
 -> string  -- ^ @uri@ - a valid URI
 -> IO Bool -- ^ returns @True@ if the new item was successfully added to the
            -- recently used resources list
recentManagerAddItem self uri =
  liftM toBool $
  withUTFString uri $ \uriPtr ->
  {# call gtk_recent_manager_add_item #}
    (toRecentManager self)
    uriPtr

-- | Removes a resource pointed by @uri@ from the recently used resources list
-- handled by a recent manager.
--
--
-- * Available since Gtk+ version 2.10
--
recentManagerRemoveItem :: (RecentManagerClass self, GlibString string) => self
 -> string  -- ^ @uri@ - the URI of the item you wish to remove
 -> IO Bool -- ^ returns @True@ if the item pointed by @uri@ has been
            -- successfully removed by the recently used resources list, and
            -- @False@ otherwise.
recentManagerRemoveItem self uri =
      checkGError (\errorPtr ->
                       liftM toBool $
                       withUTFString uri $ \uriPtr ->
                       {# call gtk_recent_manager_remove_item #}
                            (toRecentManager self)
                            uriPtr
                            errorPtr)
                  (\_ -> return False)

-- | Searches for a URI inside the recently used resources list, and returns a
-- structure containing informations about the resource like its MIME type, or
-- its display name.
--
--
-- * Available since Gtk+ version 2.10
--
recentManagerLookupItem :: (RecentManagerClass self, GlibString string) => self
 -> string                -- ^ @uri@ - a URI
 -> IO RecentInfo -- ^ returns a 'RecentInfo'
                          -- structure containing information about the
                          -- resource pointed by @uri@, or {@NULL@, FIXME: this
                          -- should probably be converted to a Maybe data type}
                          -- if the URI was not registered in the recently used
                          -- resources list.
recentManagerLookupItem self uri =
  propagateGError $ \errorPtr ->
  withUTFString uri $ \uriPtr -> do
  result <- {# call unsafe gtk_recent_manager_lookup_item #}
           (toRecentManager self)
           uriPtr
           errorPtr
  mkRecentInfo result

-- | Checks whether there is a recently used resource registered with @uri@
-- inside the recent manager.
--
--
-- * Available since Gtk+ version 2.10
--
recentManagerHasItem :: (RecentManagerClass self, GlibString string) => self
 -> string  -- ^ @uri@ - a URI
 -> IO Bool -- ^ returns @True@ if the resource was found, @False@ otherwise.
recentManagerHasItem self uri =
  liftM toBool $
  withUTFString uri $ \uriPtr ->
  {# call gtk_recent_manager_has_item #}
    (toRecentManager self)
    uriPtr

-- | Changes the location of a recently used resource from @uri@ to @newUri@.
--
-- Please note that this function will not affect the resource pointed by
-- the URIs, but only the URI used in the recently used resources list.
--
--
-- * Available since Gtk+ version 2.10
--
recentManagerMoveItem :: (RecentManagerClass self, GlibString string) => self
 -> string  -- ^ @uri@ - the URI of a recently used resource
 -> string  -- ^ @newUri@ - the new URI of the recently used resource to remove the item pointed by @uri@ in the list
 -> IO Bool -- ^ returns @True@ on success.
recentManagerMoveItem self uri newUri =
  checkGError ( \errorPtr ->
                    liftM toBool $
                    withUTFString newUri $ \newUriPtr ->
                    withUTFString uri $ \uriPtr ->
                    {# call gtk_recent_manager_move_item #}
                         (toRecentManager self)
                         uriPtr
                         newUriPtr
                         errorPtr)
              (\_ -> return False)

-- | Gets the list of recently used resources.
--
--
-- * Available since Gtk+ version 2.10
--
recentManagerGetItems :: RecentManagerClass self => self
 -> IO [RecentInfo]                        -- ^ returns a list of newly allocated
                            -- 'RecentInfo' objects.
recentManagerGetItems self = do
  glist <- {# call gtk_recent_manager_get_items #}
            (toRecentManager self)
  list <- fromGList glist
  mapM mkRecentInfo list

-- | Purges every item from the recently used resources list.
--
--
-- * Available since Gtk+ version 2.10
--
recentManagerPurgeItems :: RecentManagerClass self => self
 -> IO Int -- ^ returns the number of items that have been removed from the
           -- recently used resources list.
recentManagerPurgeItems self =
  liftM fromIntegral $
  propagateGError $ \errorPtr ->
  {# call gtk_recent_manager_purge_items #}
    (toRecentManager self)
    errorPtr

--------------------
-- Attributes

-- | The full path to the file to be used to store and read the recently used resources list
--
-- Default value: 'Nothing'
--
-- * Available since Gtk+ version 2.10
--
recentManagerFilename :: (RecentManagerClass self, GlibString string) => ReadAttr self string
recentManagerFilename = readAttrFromStringProperty "filename"

-- | The maximum number of items to be returned by the 'recentManagerGetItems' function.
--
-- Allowed values: >= 'GMaxulong'
--
-- Default value: -1
--
--
-- * Available since Gtk+ version 2.10
--
recentManagerLimit :: RecentManagerClass self => Attr self Int
recentManagerLimit = newAttrFromIntProperty "limit"

-- | The size of the recently used resources list.
--
-- Allowed values: >= 'GMaxulong'
--
-- Default value: 0
--
--
-- * Available since Gtk+ version 2.10
--
recentManagerSize :: RecentManagerClass self => ReadAttr self Int
recentManagerSize = readAttrFromIntProperty "size"

--------------------
-- Signals

-- | Emitted when the current recently used resources manager changes its
-- contents.
--
--
-- * Available since Gtk+ version 2.10
--
recentManagerChanged :: RecentManagerClass self => Signal self (IO ())
recentManagerChanged = Signal (connect_NONE__NONE "changed")
#endif
