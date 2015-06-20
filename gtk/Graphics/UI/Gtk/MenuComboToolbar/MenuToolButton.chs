{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget MenuToolButton
--
--  Author : Duncan Coutts
--
--  Created: 7 April 2005
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
-- A 'ToolItem' containing a button with an additional dropdown menu
--
-- * Module available since Gtk+ version 2.6
--
module Graphics.UI.Gtk.MenuComboToolbar.MenuToolButton (
-- * Detail
--
-- | A 'MenuToolButton' is a 'ToolItem' that contains a button and a small
-- additional button with an arrow. When clicked, the arrow button pops up a
-- dropdown menu.
--
-- Use 'menuToolButtonNew' to create a new 'MenuToolButton'. Use
-- 'menuToolButtonNewFromStock' to create a new 'MenuToolButton' containing a
-- stock item.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'ToolItem'
-- |                                 +----'ToolButton'
-- |                                       +----MenuToolButton
-- @

#if GTK_CHECK_VERSION(2,6,0)
-- * Types
  MenuToolButton,
  MenuToolButtonClass,
  castToMenuToolButton, gTypeMenuToolButton,
  toMenuToolButton,

-- * Constructors
  menuToolButtonNew,
  menuToolButtonNewFromStock,

-- * Methods
  menuToolButtonSetMenu,
  menuToolButtonGetMenu,
#if GTK_MAJOR_VERSION < 3
  menuToolButtonSetArrowTooltip,
#endif
#if GTK_CHECK_VERSION(2,12,0)
  menuToolButtonSetArrowTooltipText,
  menuToolButtonSetArrowTooltipMarkup,
#endif

-- * Attributes
  menuToolButtonMenu,

-- * Signals
  onShowMenu,
  afterShowMenu,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.StockItems

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,6,0)
--------------------
-- Constructors

-- | Creates a new 'MenuToolButton' using @iconWidget@ as icon and @label@ as
-- label.
--
menuToolButtonNew :: (WidgetClass iconWidget, GlibString string) =>
    Maybe iconWidget  -- ^ @iconWidget@ - a widget that will be used as icon
                      -- widget, or @Nothing@
 -> Maybe string      -- ^ @label@ - a string that will be used as label, or
                      -- @Nothing@
 -> IO MenuToolButton
menuToolButtonNew iconWidget label =
  makeNewObject mkMenuToolButton $
  liftM (castPtr :: Ptr ToolItem -> Ptr MenuToolButton) $
  maybeWith withUTFString label $ \labelPtr ->
  {# call gtk_menu_tool_button_new #}
    (maybe (Widget nullForeignPtr) toWidget iconWidget)
    labelPtr

-- | Creates a new 'MenuToolButton'. The new 'MenuToolButton' will contain an
-- icon and label from the stock item indicated by @stockId@.
--
menuToolButtonNewFromStock ::
    StockId           -- ^ @stockId@ - the name of a stock item
 -> IO MenuToolButton
menuToolButtonNewFromStock stockId =
  makeNewObject mkMenuToolButton $
  liftM (castPtr :: Ptr ToolItem -> Ptr MenuToolButton) $
  withUTFString stockId $ \stockIdPtr ->
  {# call gtk_menu_tool_button_new_from_stock #}
    stockIdPtr

--------------------
-- Methods

-- | Sets the 'Menu' that is popped up when the user clicks on the arrow. If
-- @menu@ is @Nothing@, the arrow button becomes insensitive.
--
menuToolButtonSetMenu :: (MenuToolButtonClass self, MenuClass menu) => self
 -> Maybe menu -- ^ @menu@ - the 'Menu' associated with 'MenuToolButton'
 -> IO ()
menuToolButtonSetMenu self menu =
  {# call gtk_menu_tool_button_set_menu #}
    (toMenuToolButton self)
    (maybe (Widget nullForeignPtr) toWidget menu)

-- | Gets the 'Menu' associated with 'MenuToolButton'.
--
menuToolButtonGetMenu :: MenuToolButtonClass self => self -> IO (Maybe Menu)
menuToolButtonGetMenu self =
  maybeNull (makeNewObject mkMenu) $
  liftM (castPtr :: Ptr Widget -> Ptr Menu) $
  {# call gtk_menu_tool_button_get_menu #}
    (toMenuToolButton self)

#if GTK_MAJOR_VERSION < 3
-- | Sets the 'Tooltips' object to be used for arrow button which pops up the
-- menu. See 'Graphics.UI.Gtk.MenuComboToolbar.ToolItem.toolItemSetTooltip'
-- for setting a tooltip on the whole 'MenuToolButton'.
--
menuToolButtonSetArrowTooltip :: (MenuToolButtonClass self, GlibString string) => self
 -> Tooltips -- ^ @tooltips@ - the 'Tooltips' object to be used
 -> string   -- ^ @tipText@ - text to be used as tooltip text for tool item
 -> string   -- ^ @tipPrivate@ - text to be used as private tooltip text
 -> IO ()
menuToolButtonSetArrowTooltip self tooltips tipText tipPrivate =
  withUTFString tipPrivate $ \tipPrivatePtr ->
  withUTFString tipText $ \tipTextPtr ->
  {# call gtk_menu_tool_button_set_arrow_tooltip #}
    (toMenuToolButton self)
    tooltips
    tipTextPtr
    tipPrivatePtr
#endif

#if GTK_CHECK_VERSION(2,12,0)
-- | Sets the tooltip text to be used as tooltip for the arrow button which
-- pops up the menu. See 'toolItemSetTooltip' for setting a tooltip on the
-- whole 'MenuToolButton'.
--
-- * Available since Gtk+ version 2.12
--
menuToolButtonSetArrowTooltipText :: (MenuToolButtonClass self, GlibString string) => self
 -> string -- ^ @text@ - text to be used as tooltip text for button's arrow
           -- button
 -> IO ()
menuToolButtonSetArrowTooltipText self text =
  withUTFString text $ \textPtr ->
  {# call gtk_menu_tool_button_set_arrow_tooltip_text #}
    (toMenuToolButton self)
    textPtr

-- | Sets the tooltip markup text to be used as tooltip for the arrow button
-- which pops up the menu. See 'toolItemSetTooltip' for setting a tooltip on
-- the whole 'MenuToolButton'.
--
-- * Available since Gtk+ version 2.12
--
menuToolButtonSetArrowTooltipMarkup :: (MenuToolButtonClass self, GlibString markup) => self
 -> markup -- ^ @markup@ - markup text to be used as tooltip text for button's
           -- arrow button
 -> IO ()
menuToolButtonSetArrowTooltipMarkup self markup =
  withUTFString markup $ \markupPtr ->
  {# call gtk_menu_tool_button_set_arrow_tooltip_markup #}
    (toMenuToolButton self)
    markupPtr
#endif

--------------------
-- Attributes

-- | The dropdown menu.
--
menuToolButtonMenu :: (MenuToolButtonClass self, MenuClass menu) => ReadWriteAttr self (Maybe Menu) (Maybe menu)
menuToolButtonMenu = newAttr
  menuToolButtonGetMenu
  menuToolButtonSetMenu

--------------------
-- Signals

-- |
--
onShowMenu, afterShowMenu :: MenuToolButtonClass self => self
 -> IO ()
 -> IO (ConnectId self)
onShowMenu = connect_NONE__NONE "show-menu" False
afterShowMenu = connect_NONE__NONE "show-menu" True
#endif
