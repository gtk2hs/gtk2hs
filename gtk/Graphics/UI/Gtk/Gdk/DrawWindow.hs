--  -*-haskell-*-
--  GIMP Toolkit (GTK) DrawWindow
--
--  Author : Axel Simon
--  Created: 5 November 2002
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:18:36 $
--
--  Copyright (c) 2002 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
-- |
--
-- A primitive window.
--
--  * This abstract type represents an on-screen window. Since it is derived
--    from 'Drawable', all methods defined there can be used.
--
--  * Every widget usually has a 'DrawWindow' into which it draws its
--    content. 'DrawWindow's become useful when the user creates
--    custom widgets using the 'DrawingArea' skeleton.
--
-- TODO
--
--  * This abstract type corresponds to a @gdk_window@. There seems to be no
--    functions of interest that operate on @gdk_window@s.
--
module Graphics.UI.Gtk.Gdk.DrawWindow (
  DrawWindow,
  DrawWindowClass
  ) where

import Graphics.UI.Gtk.Types


