-- -*-haskell-*-
--  GIMP Toolkit (GTK) OpenGL Extension: GLPixmap
--
--  Author : Duncan Coutts
--
--  Created: 9 June 2005
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
-- OpenGL pixmap which is maintained off-screen
--
module Graphics.UI.Gtk.OpenGL.Pixmap (

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Drawable'
-- |         +----GLPixmap
-- @

-- * Types
  GLPixmap,
  GLPixmapClass,
  castToGLPixmap,

-- * Constructors
  glPixmapNew,

-- * Methods
  glPixmapGetPixmap,
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.GObject			(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.OpenGL.Types#}

{# context lib="gtkglext" prefix="gdk" #}

instance GLDrawableClass GLPixmap

--------------------
-- Constructors

-- | Creates an off-screen rendering area.
--
glPixmapNew ::
    GLConfig
 -> Pixmap      -- ^ @pixmap@ - the 'Pixmap' to be used as the rendering area.
 -> IO GLPixmap
glPixmapNew glconfig pixmap =
  makeNewGObject mkGLPixmap $
  {# call gdk_gl_pixmap_new #}
    (toGLConfig glconfig)
    (toPixmap pixmap)
    nullPtr

--------------------
-- Methods

-- | Returns the 'Pixmap' associated with a 'GLPixmap'.
--
glPixmapGetPixmap :: GLPixmap -> IO Pixmap
glPixmapGetPixmap self =
  makeNewGObject mkPixmap $
  {# call gdk_gl_pixmap_get_pixmap #}
    (toGLPixmap self)
