-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget OptionMenu
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
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
-- A widget used to choose from a list of valid choices.
--
module Graphics.UI.Gtk.MenuComboToolbar.OptionMenu (
-- * Description
-- 
-- | A 'OptionMenu' is a widget that allows the user to choose from a list of
-- valid choices. The 'OptionMenu' displays the selected choice. When activated
-- the 'OptionMenu' displays a popup 'Menu' which allows the user to make a new
-- choice.
--
-- Using a 'OptionMenu' is simple; build a 'Menu', by calling 'menuNew',
-- then appending menu items to it with 'menuShellAppend'. Set that menu on the
-- option menu with 'optionMenuSetMenu'. Set the selected menu item with
-- 'optionMenuSetHistory'; connect to the \"changed\" signal on the option
-- menu; in the \"changed\" signal, check the new selected menu item with
-- 'optionMenuGetHistory'.
--
-- As of Gtk+ 2.4, 'OptionMenu' has been deprecated in favor of 'ComboBox'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'Button'
-- |                                 +----OptionMenu
-- @

#ifndef DISABLE_DEPRECATED
-- * Types
  OptionMenu,
  OptionMenuClass,
  castToOptionMenu,

-- * Methods
  optionMenuNew,
  optionMenuGetMenu,
  optionMenuSetMenu,
  optionMenuRemoveMenu,
  optionMenuSetHistory,
  optionMenuGetHistory,

-- * Signals
  onOMChanged,
  afterOMChanged
#endif
  ) where
#ifndef DISABLE_DEPRECATED

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Create a new option menu.
--
optionMenuNew :: IO OptionMenu
optionMenuNew  = makeNewObject mkOptionMenu $ 
  liftM castPtr {#call unsafe option_menu_new#}

-- | Get the menu that should be associated with this
-- option menu.
--
optionMenuGetMenu :: OptionMenuClass om => om -> IO Menu
optionMenuGetMenu om = makeNewObject mkMenu $ liftM castPtr $
  throwIfNull "optionMenuGetMenu: no menu associated with this option menu." $
  {#call unsafe option_menu_get_menu#} (toOptionMenu om)

-- | Set a menu to associate with this option menu.
--
optionMenuSetMenu :: (OptionMenuClass om, MenuClass m) => om -> m -> IO ()
optionMenuSetMenu om m = {#call option_menu_set_menu#}
  (toOptionMenu om) (toWidget m)

-- | Remove the association the menu.
--
optionMenuRemoveMenu :: OptionMenuClass om => om -> IO ()
optionMenuRemoveMenu om = 
  {#call unsafe option_menu_remove_menu#} (toOptionMenu om)

-- | Set the state of the option menu. The options
-- are numbered from 0 up to n-1 for the nth item.
--
optionMenuSetHistory :: OptionMenuClass om => om -> Int -> IO ()
optionMenuSetHistory om item = {#call option_menu_set_history#}
  (toOptionMenu om) (fromIntegral item)

-- | Retrieve the index of the selected item.
--
optionMenuGetHistory :: OptionMenuClass om => om -> IO Int
optionMenuGetHistory om = liftM fromIntegral $
  {#call unsafe option_menu_get_history#} (toOptionMenu om)

--------------------
-- Signals

-- | This signal is called if the selected option has changed.
--
onOMChanged, afterOMChanged :: OptionMenuClass om => om -> IO () ->
                               IO (ConnectId om)
onOMChanged = connect_NONE__NONE "changed" False
afterOMChanged = connect_NONE__NONE "changed" True

#endif
