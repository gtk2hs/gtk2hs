-- -*-haskell-*-
--  GIMP Toolkit (GTK) OpenGL Extension
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
-- OpenGL extension for Gtk+
--
module Graphics.UI.Gtk.OpenGL (
  -- * Detail
  
  -- | A typical program will look like this

  -- * Simple OpenGL drawing area widget
  module Graphics.UI.Gtk.OpenGL.DrawingArea,
  -- * Initialisation and query functions
  module Graphics.UI.Gtk.OpenGL.General,
  -- * Lower level modules
  module Graphics.UI.Gtk.OpenGL.Config,
  module Graphics.UI.Gtk.OpenGL.Context,
  module Graphics.UI.Gtk.OpenGL.Drawable,
  module Graphics.UI.Gtk.OpenGL.Pixmap,
  module Graphics.UI.Gtk.OpenGL.Window,
  ) where

import Graphics.UI.Gtk.OpenGL.Config
import Graphics.UI.Gtk.OpenGL.Context
import Graphics.UI.Gtk.OpenGL.Drawable
import Graphics.UI.Gtk.OpenGL.Pixmap
import Graphics.UI.Gtk.OpenGL.Window
import Graphics.UI.Gtk.OpenGL.General
import Graphics.UI.Gtk.OpenGL.DrawingArea
