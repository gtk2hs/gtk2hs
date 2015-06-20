-- -*-haskell-*-
--  GIMP Toolkit (GTK) Window
--
--  Author : Andy Stewart
--
--  Created: 7 Oct 2009
--
--  Copyright (C) 2009 Andy Stewart
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
-- A widget which is not displayed
-- The 'Invisible' widget is used internally in GTK+, and is probably not very useful for application developers.
-- It is used for reliable pointer grabs and selection handling in the code for drag-and-drop.
--
module Graphics.UI.Gtk.Windows.Invisible (

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----Invisible
-- @

-- * Types
  Invisible,

-- * Constructors
  invisibleNew,
  invisibleNewForScreen,

-- * Methods
  invisibleSetScreen,
  invisibleGetScreen,
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
import Graphics.UI.Gtk.Gdk.Screen
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors
-- | Creates a new 'Invisible'.
--
invisibleNew :: IO Invisible
invisibleNew =
  makeNewObject mkInvisible $
  liftM (castPtr :: Ptr Widget -> Ptr Invisible) $
  {# call invisible_new #}

-- | Creates a new 'Invisible' object for a specified screen
--
-- * Available since Gdk version 2.2
--
invisibleNewForScreen ::
   Screen   -- ^ @screen@ - a 'Screen' which identifies on which the new 'Invisible' will be created.
 -> IO Invisible
invisibleNewForScreen screen =
  makeNewObject mkInvisible $
  liftM (castPtr :: Ptr Widget -> Ptr Invisible) $
  {# call invisible_new_for_screen #} screen

-- | Sets the 'Screen' where the 'Invisible' object will be displayed.
--
-- * Available since Gdk version 2.2
--
invisibleSetScreen :: Invisible
 -> Screen  -- ^ @screen@ - the 'Screen' to set
 -> IO ()
invisibleSetScreen invisible screen =
  {# call invisible_set_screen #} invisible screen

-- | Returns the 'Screen' object associated with invisible
--
-- * Available since Gdk version 2.2
--
invisibleGetScreen :: Invisible
 -> IO Screen
invisibleGetScreen invisible =
  makeNewGObject mkScreen $
  {# call invisible_get_screen #} invisible

