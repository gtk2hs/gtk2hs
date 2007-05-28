-- -*-haskell-*-
--  GIMP Toolkit (GTK) Pixmap
--
--  Author : Armin Groesslinger
--
--  Created: 05 July 2005
--
--  Version $Revision: 1.3 $ from $Date: 2005/11/26 16:00:21 $
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

import Data.Maybe
import System.Glib.FFI
import System.Glib.GObject           (constructNewGObject)
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.Gdk.Drawable  (Drawable, DrawableClass(..))

{# context lib="gdk" prefix="gdk" #}

-- | Create a new pixmap.
--
-- If @drawable@ is @Nothing@, the depth of the pixmap is taken from the
-- @depth@ parameter, otherwise the pixmap has the same depth as the
-- 'Drawable' specified by @drawable@. Therefore, at least one of @drawable@
-- and @depth@ must not be @Nothing@.
--
-- * Note that in Gtk+ 2.0 the @drawable@ can only be a 'DrawWindow', not an
-- arbitary 'Drawable'.
--
#if GTK_CHECK_VERSION(2,2,0)
pixmapNew :: DrawableClass drawable
 => Maybe drawable -- ^ @drawable@ - drawable supplying default values for the
                   --pixmap
 -> Int            -- ^ @width@  - width of the pixmap
 -> Int            -- ^ @height@ - height of the pixmap
 -> Maybe Int      -- ^ @depth@  - depth of the pixmap
 -> IO Pixmap
pixmapNew mbDrawable width height depth =
    constructNewGObject mkPixmap $
    {# call unsafe pixmap_new #}
      (maybe (mkDrawable nullForeignPtr) toDrawable mbDrawable)
      (fromIntegral width) (fromIntegral height)
      (fromIntegral $ fromMaybe (negate 1) depth)
#else
pixmapNew ::
    Maybe DrawWindow -- ^ @drawable@ - drawable supplying default values for
                     -- the pixmap
 -> Int              -- ^ @width@  - width of the pixmap
 -> Int              -- ^ @height@ - height of the pixmap
 -> Maybe Int        -- ^ @depth@  - depth of the pixmap
 -> IO Pixmap
pixmapNew mbDrawWindow width height depth =
    constructNewGObject mkPixmap $
    {# call unsafe pixmap_new #}
      (maybe (mkDrawWindow nullForeignPtr) toDrawWindow mbDrawWindow)
      (fromIntegral width) (fromIntegral height)
      (fromIntegral $ fromMaybe (negate 1) depth)
#endif
