-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget MenuBar
--
--  Author : Axel Simon
--
--  Created: 21 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/25 01:11:35 $
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- A subclass widget for 'MenuShell' which holds 'MenuItem' widgets
--
module Graphics.UI.Gtk.MenuComboToolbar.MenuBar (
-- * Description
-- 
-- | The 'MenuBar' is a subclass of 'MenuShell' which contains one to many
-- 'MenuItem'. The result is a standard menu bar which can hold many menu
-- items. 'MenuBar' allows for a shadow type to be set for aesthetic purposes.
-- The shadow types are defined in the 'menuBarSetShadowType' function.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'MenuShell'
-- |                           +----MenuBar
-- @

-- * Types
  MenuBar,
  MenuBarClass,
  castToMenuBar,

-- * Constructors
  menuBarNew
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a horizontal bar that contains menu items.
--
menuBarNew :: IO MenuBar
menuBarNew  = makeNewObject mkMenuBar $ 
  liftM castPtr {#call unsafe menu_bar_new#}
