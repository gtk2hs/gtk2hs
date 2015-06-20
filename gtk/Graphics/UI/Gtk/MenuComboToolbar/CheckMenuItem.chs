{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget CheckMenuItem
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
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
-- A menu item with a check box
--
module Graphics.UI.Gtk.MenuComboToolbar.CheckMenuItem (
-- * Detail
--
-- | A 'CheckMenuItem' is a menu item that maintains the state of a boolean
-- value in addition to a 'MenuItem's usual role in activating application
-- code.
--
-- A check box indicating the state of the boolean value is displayed at the
-- left side of the 'MenuItem'. Activating the 'MenuItem' toggles the value.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'Item'
-- |                                 +----'MenuItem'
-- |                                       +----CheckMenuItem
-- |                                             +----'RadioMenuItem'
-- @

-- * Types
  CheckMenuItem,
  CheckMenuItemClass,
  castToCheckMenuItem, gTypeCheckMenuItem,
  toCheckMenuItem,

-- * Constructors
  checkMenuItemNew,
  checkMenuItemNewWithLabel,
  checkMenuItemNewWithMnemonic,

-- * Methods
  checkMenuItemSetActive,
  checkMenuItemGetActive,
  checkMenuItemEmitToggled,
  checkMenuItemSetInconsistent,
  checkMenuItemGetInconsistent,
#if GTK_CHECK_VERSION(2,4,0)
  checkMenuItemGetDrawAsRadio,
  checkMenuItemSetDrawAsRadio,
#endif

-- * Attributes
  checkMenuItemActive,
  checkMenuItemInconsistent,
#if GTK_CHECK_VERSION(2,4,0)
  checkMenuItemDrawAsRadio,
#endif

-- * Signals
  checkMenuItemToggled
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'CheckMenuItem'.
--
checkMenuItemNew :: IO CheckMenuItem
checkMenuItemNew =
  makeNewObject mkCheckMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr CheckMenuItem) $
  {# call unsafe check_menu_item_new #}

-- | Creates a new 'CheckMenuItem' with a label.
--
checkMenuItemNewWithLabel :: GlibString string
 => string           -- ^ @label@ - the string to use for the label.
 -> IO CheckMenuItem
checkMenuItemNewWithLabel label =
  makeNewObject mkCheckMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr CheckMenuItem) $
  withUTFString label $ \labelPtr ->
  {# call unsafe check_menu_item_new_with_label #}
    labelPtr

-- | Creates a new 'CheckMenuItem' containing a label. The label will be
-- created using 'Graphics.UI.Gtk.Display.Label.labelNewWithMnemonic', so
-- underscores in @label@ indicate the mnemonic for the menu item.
--
checkMenuItemNewWithMnemonic :: GlibString string
 => string           -- ^ @label@ - The text of the button, with an underscore
                     -- in front of the mnemonic character
 -> IO CheckMenuItem
checkMenuItemNewWithMnemonic label =
  makeNewObject mkCheckMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr CheckMenuItem) $
  withUTFString label $ \labelPtr ->
  {# call unsafe check_menu_item_new_with_mnemonic #}
    labelPtr

--------------------
-- Methods

-- | Sets the active state of the menu item's check box.
--
checkMenuItemSetActive :: CheckMenuItemClass self => self -> Bool -> IO ()
checkMenuItemSetActive self isActive =
  {# call check_menu_item_set_active #}
    (toCheckMenuItem self)
    (fromBool isActive)

-- | Returns whether the check menu item is active. See
-- 'checkMenuItemSetActive'.
--
checkMenuItemGetActive :: CheckMenuItemClass self => self -> IO Bool
checkMenuItemGetActive self =
  liftM toBool $
  {# call unsafe check_menu_item_get_active #}
    (toCheckMenuItem self)

-- | Emits the toggled signal.
--
checkMenuItemEmitToggled :: CheckMenuItemClass self => self -> IO ()
checkMenuItemEmitToggled self =
  {# call check_menu_item_toggled #}
    (toCheckMenuItem self)

-- | If the user has selected a range of elements (such as some text or
-- spreadsheet cells) that are affected by a boolean setting, and the current
-- values in that range are inconsistent, you may want to display the check in
-- an \"in between\" state. This function turns on \"in between\" display.
-- Normally you would turn off the inconsistent state again if the user
-- explicitly selects a setting. This has to be done manually,
-- 'checkMenuItemSetInconsistent' only affects visual appearance, it doesn't
-- affect the semantics of the widget.
--
checkMenuItemSetInconsistent :: CheckMenuItemClass self => self -> Bool -> IO ()
checkMenuItemSetInconsistent self setting =
  {# call check_menu_item_set_inconsistent #}
    (toCheckMenuItem self)
    (fromBool setting)

-- | Query if the menu check is drawn as inconsistent (inbetween). See
-- 'checkMenuItemSetInconsistent'.
--
checkMenuItemGetInconsistent :: CheckMenuItemClass self => self -> IO Bool
checkMenuItemGetInconsistent self =
  liftM toBool $
  {# call unsafe check_menu_item_get_inconsistent #}
    (toCheckMenuItem self)

#if GTK_CHECK_VERSION(2,4,0)
-- | Sets whether the menu item is drawn like a 'RadioMenuItem'.
--
-- * Available since Gtk+ version 2.4
--
checkMenuItemSetDrawAsRadio :: CheckMenuItemClass self => self -> Bool -> IO ()
checkMenuItemSetDrawAsRadio self drawAsRadio =
  {# call check_menu_item_set_draw_as_radio #}
    (toCheckMenuItem self)
    (fromBool drawAsRadio)

-- | Returns whether the menu item is drawn like a 'RadioMenuItem'.
--
-- * Available since Gtk+ version 2.4
--
checkMenuItemGetDrawAsRadio :: CheckMenuItemClass self => self -> IO Bool
checkMenuItemGetDrawAsRadio self =
  liftM toBool $
  {# call unsafe check_menu_item_get_draw_as_radio #}
    (toCheckMenuItem self)
#endif

--------------------
-- Attributes

-- | Whether the menu item is checked.
--
-- Default value: @False@
--
checkMenuItemActive :: CheckMenuItemClass self => Attr self Bool
checkMenuItemActive = newAttr
  checkMenuItemGetActive
  checkMenuItemSetActive

-- | Whether to display an \"inconsistent\" state.
--
-- Default value: @False@
--
checkMenuItemInconsistent :: CheckMenuItemClass self => Attr self Bool
checkMenuItemInconsistent = newAttr
  checkMenuItemGetInconsistent
  checkMenuItemSetInconsistent

#if GTK_CHECK_VERSION(2,4,0)
-- | Whether the menu item looks like a radio menu item.
--
-- Default value: @False@
--
checkMenuItemDrawAsRadio :: CheckMenuItemClass self => Attr self Bool
checkMenuItemDrawAsRadio = newAttr
  checkMenuItemGetDrawAsRadio
  checkMenuItemSetDrawAsRadio
#endif

-- | This signal is emitted when the state of the check box is changed.
--
checkMenuItemToggled :: CheckMenuItemClass self => Signal self (IO ())
checkMenuItemToggled = Signal (connect_NONE__NONE "toggled")
