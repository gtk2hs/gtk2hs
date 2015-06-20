{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget RecentChooserMenu
--
--  Author : Andy Stewart
--
--  Created: 27 Mar 2010
--
--  Copyright (C) 2010 Andy Stewart
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
-- Displays recently used files in a menu
--
-- * Module available since Gtk+ version 2.10
--
module Graphics.UI.Gtk.Recent.RecentChooserMenu (

-- * Detail
--
-- | 'RecentChooserMenu' is a widget suitable for displaying recently used
-- files inside a menu. It can be used to set a sub-menu of a 'MenuItem' using
-- 'menuItemSetSubmenu', or as the menu of a 'MenuToolButton'.
--
-- Note that 'RecentChooserMenu' does not have any methods of its own.
-- Instead, you should use the functions that work on a 'RecentChooser'.
--
-- Note also that 'RecentChooserMenu' does not support multiple filters, as
-- it has no way to let the user choose between them as the
-- 'RecentChooserWidget' and 'RecentChooserDialog' widgets do. Thus using
-- 'recentChooserAddFilter' on a 'RecentChooserMenu' widget will yield the same
-- effects as using 'recentChooserSetFilter', replacing any currently set
-- filter with the supplied filter; 'recentChooserRemoveFilter' will remove any
-- currently set 'RecentFilter' object and will unset the current filter;
-- 'recentChooserListFilters' will return a list containing a single
-- 'RecentFilter' object.
--
-- Recently used files are supported since Gtk+ 2.10.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'MenuShell'
-- |                           +----'Menu'
-- |                                 +----RecentChooserMenu
-- @

#if GTK_CHECK_VERSION(2,10,0)
-- * Types
  RecentChooserMenu,
  RecentChooserMenuClass,
  castToRecentChooserMenu,
  toRecentChooserMenu,

-- * Constructors
  recentChooserMenuNew,
  recentChooserMenuNewForManager,

-- * Attributes
  recentChooserMenuShowNumbers,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,10,0)

--------------------
-- Constructors

-- | Creates a new 'RecentChooserMenu' widget.
--
-- This kind of widget shows the list of recently used resources as a menu,
-- each item as a menu item. Each item inside the menu might have an icon,
-- representing its MIME type, and a number, for mnemonic access.
--
-- This widget implements the 'RecentChooser' interface.
--
-- This widget creates its own 'RecentManager' object. See the
-- 'recentChooserMenuNewForManager' function to know how to create a
-- 'RecentChooserMenu' widget bound to another 'RecentManager' object.
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserMenuNew :: IO RecentChooserMenu
recentChooserMenuNew =
  makeNewObject mkRecentChooserMenu $
  liftM (castPtr :: Ptr Widget -> Ptr RecentChooserMenu) $
  {# call gtk_recent_chooser_menu_new #}

-- | Creates a new 'RecentChooserMenu' widget using @manager@ as the
-- underlying recently used resources manager.
--
-- This is useful if you have implemented your own recent manager, or if you
-- have a customized instance of a 'RecentManager' object or if you wish to
-- share a common 'RecentManager' object among multiple 'RecentChooser'
-- widgets.
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserMenuNewForManager :: RecentManagerClass manager
                               => manager -- ^ @manager@ - a 'RecentManager'
                               -> IO RecentChooserMenu -- ^ returns a new 'RecentChooserMenu', bound to manager.
recentChooserMenuNewForManager manager =
  makeNewObject mkRecentChooserMenu $
  liftM (castPtr :: Ptr Widget -> Ptr RecentChooserMenu) $
  {# call gtk_recent_chooser_menu_new_for_manager #}
      (toRecentManager manager)

--------------------
-- Attributes

-- | Whether the first ten items in the menu should be prepended by a number acting as a unique mnemonic.
--
-- Default value: 'False'
--
-- * Available since Gtk+ version 2.10
--
recentChooserMenuShowNumbers :: RecentChooserMenuClass self => Attr self Bool
recentChooserMenuShowNumbers = newAttrFromBoolProperty "show-numbers"
#endif
