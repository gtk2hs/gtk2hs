{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget DisplayManager
--
--  Author : Andy Stewart
--
--  Created: 29 Mar 2010
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
-- Maintains a list of all open GdkDisplays
--
-- * Module available since Gdk version 2.2
--
module Graphics.UI.Gtk.Gdk.DisplayManager (

-- * Detail
--
-- | The purpose of the 'DisplayManager' singleton object is to offer
-- notification when displays appear or disappear or the default display
-- changes.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----DisplayManager
-- @

#if GTK_CHECK_VERSION(2,2,0)
-- * Types
  DisplayManager,
  DisplayManagerClass,
  castToDisplayManager,
  toDisplayManager,

-- * Methods
  displayManagerGet,
  displayManagerListDisplays,

-- * Attributes
  displayManagerDefaultDisplay,

-- * Signals
  displayManagerOpened,
#endif
  ) where

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GList
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gdk" prefix="gdk" #}

#if GTK_CHECK_VERSION(2,2,0)
--------------------
-- Methods

-- | Returns the global 'DisplayManager' singleton; 'parsePargs', 'init', or
-- 'initCheck' must have been called first.
--
displayManagerGet :: IO DisplayManager -- ^ returns the singleton 'DisplayManager' object.
displayManagerGet =
  constructNewGObject mkDisplayManager $
  {# call gdk_display_manager_get #}

-- | List all currently open displays.
--
displayManagerListDisplays :: DisplayManagerClass self => self
 -> IO [Display] -- ^ returns a newly allocated list of 'Display' objects.
displayManagerListDisplays self =
  {# call gdk_display_manager_list_displays #}
    (toDisplayManager self)
  >>= fromGSList
  >>= mapM (makeNewGObject mkDisplay . return)

--------------------
-- Attributes

-- | The default display.
--
displayManagerDefaultDisplay :: DisplayManagerClass self => Attr self Display
displayManagerDefaultDisplay = newAttrFromObjectProperty "default-display"
                                 {# call pure unsafe gdk_display_get_type #}

--------------------
-- Signals

-- | The 'displayManagerOpened' signal is emitted when a display is opened.
--
displayManagerOpened :: DisplayManagerClass self => Signal self (Display -> IO ())
displayManagerOpened = Signal (connect_OBJECT__NONE "display_opened")
#endif
