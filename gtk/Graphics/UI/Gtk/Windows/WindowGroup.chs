{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget WindowGroup
--
--  Author : Duncan Coutts
--
--  Created: 25 March 2005
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
-- Limit the effect of grabs
--
module Graphics.UI.Gtk.Windows.WindowGroup (

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----WindowGroup
-- @

-- * Types
  WindowGroup,
  WindowGroupClass,
  castToWindowGroup, gTypeWindowGroup,
  toWindowGroup,

-- * Constructors
  windowGroupNew,

-- * Methods
  windowGroupAddWindow,
  windowGroupRemoveWindow,
#if GTK_CHECK_VERSION(2,14,0)
  windowGroupListWindows,
#endif
  ) where

import System.Glib.FFI
import System.Glib.GList                (fromGList)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'WindowGroup' object. Grabs added with
-- 'Graphics.UI.Gtk.General.General.grabAdd' only affect windows within the
-- same 'WindowGroup'.
--
windowGroupNew :: IO WindowGroup
windowGroupNew =
  wrapNewGObject mkWindowGroup $
  {# call gtk_window_group_new #}

--------------------
-- Methods

-- | Adds a window to a 'WindowGroup'.
--
windowGroupAddWindow :: (WindowGroupClass self, WindowClass window) => self
 -> window -- ^ @window@ - the 'Window' to add
 -> IO ()
windowGroupAddWindow self window =
  {# call gtk_window_group_add_window #}
    (toWindowGroup self)
    (toWindow window)

-- | Removes a window from a 'WindowGroup'.
--
windowGroupRemoveWindow :: (WindowGroupClass self, WindowClass window) => self
 -> window -- ^ @window@ - the 'Window' to remove
 -> IO ()
windowGroupRemoveWindow self window =
  {# call gtk_window_group_remove_window #}
    (toWindowGroup self)
    (toWindow window)

#if GTK_CHECK_VERSION(2,14,0)
-- | Returns a list of the 'Window's that belong to @windowGroup@.
--
-- * Available since Gtk+ version 2.14
--
windowGroupListWindows :: WindowGroupClass self
  => self -- ^ @windowGroup@ - the window group
 -> IO [Window]  -- ^ returns the list of windows inside this group
windowGroupListWindows self = do
  glist <- {# call window_group_list_windows #} (toWindowGroup self)
  ptrList <- fromGList glist
  mapM (makeNewGObject mkWindow . return) ptrList
#endif
