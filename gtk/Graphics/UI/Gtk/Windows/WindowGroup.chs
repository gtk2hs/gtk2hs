-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget WindowGroup
--
--  Author : Duncan Coutts
--
--  Created: 25 March 2005
--
--  Version $Revision: 1.4 $ from $Date: 2005/11/26 16:00:22 $
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
  castToWindowGroup,
  toWindowGroup,

-- * Constructors
  windowGroupNew,

-- * Methods
  windowGroupAddWindow,
  windowGroupRemoveWindow,
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.GObject              (constructNewGObject)
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
  constructNewGObject mkWindowGroup $
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
