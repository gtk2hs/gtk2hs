-- -*-haskell-*-
--  GIMP Toolkit (GTK) Pixmap
--
--  Author : Armin Groesslinger
--
--  Created: 05 July 2005
--
--  Version $Revision: 1.1 $ from $Date: 2005/10/26 13:36:26 $
--
--  Copyright (C) 2005 Armin Groesslinger
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
-- TODO
--
-- add methods
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Pixmaps -- Offscreen drawables 
--
module Graphics.UI.Gtk.Gdk.Pixmap (
-- * Detail
-- Pixmaps are offscreen drawables. They can be drawn upon with the
-- standard drawing primitives, then copied to another drawable 
-- with 'drawDrawable'.

-- * Class Hierarchy
-- |
-- @
-- |   'GObject'
-- |   +----'Drawable'
-- |         +----Pixmap
-- @

-- * Types
  Pixmap, PixmapClass, 

-- * Constructors
  pixmapNew
  ) where

import Maybe
import System.Glib.FFI
import System.Glib.GObject           (makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.Gdk.Drawable  (Drawable, DrawableClass(..))

{# context lib="gdk" prefix="gdk" #}

-- methods

-- | Create a new pixmap.
-- If @drawable@ is @Nothing@, the depth of the pixmap is
-- taken from the @depth@ parameter, otherwise the pixmap
-- has the same depth as the 'Drawable' specified by @drawable@.
-- Therefore, at least one of @drawable@ and @depth@ must not be @Nothing@.
--
pixmapNew :: DrawableClass drawable
 => Maybe drawable -- ^ @drawable@ - drawable supplying default values for the pixmap
 -> Int       -- ^ @width@  - width of the pixmap
 -> Int       -- ^ @height@ - height of the pixmap
 -> Maybe Int -- ^ @depth@  - depth of the pixmap
 -> IO Pixmap
pixmapNew mbDrawable width height depth = 
    makeNewGObject mkPixmap $
    {# call unsafe pixmap_new #}
#if GTK_CHECK_VERSION(2,1,0) && !GTK_CHECK_VERSION(2,2,0)
      -- support for the broken Gtk+ 2.1.x version that Sun shipped
      (maybe (mkDrawWindow nullForeignPtr) (fromDrawable.toDrawable) mbDrawable)
#else
      (maybe (mkDrawable nullForeignPtr) toDrawable mbDrawable)
#endif
      (fromIntegral width) (fromIntegral height)
      (fromIntegral $ fromMaybe (negate 1) depth)

