--  -*-haskell-*-
--  GIMP Toolkit (GTK) @entry DrawWindow@
--
--  Author : Axel Simon
--  Created: 5 November 2002
--
--  Version $Revision: 1.1 $ from $Date: 2002/12/03 13:45:43 $
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
-- @description@ --------------------------------------------------------------
--
--  A primitive window.
--
-- @documentation@ ------------------------------------------------------------
--
--  * This abstract type represents an on-screen window. Since it is derived
--    from @ref data Drawable@, all methods defined there can be used.
--
--  * Every widget usually has a @ref data DrawWindow@ into which it draws its
--    content. @ref data DrawWindow@s become useful when the user creates
--    custom widgets using the @ref data DrawingArea@ skeleton.
--
-- @todo@ ---------------------------------------------------------------------
--
--  * This abstract type corresponds to a gdk_window. There seems to be no
--    functions of interest that operate on gdk_windows.
--
module DrawWindow(
  DrawWindow,
  DrawWindowClass
  ) where

import Hierarchy


