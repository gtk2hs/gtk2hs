-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget MenuToolButton
--
--  Author : Duncan Coutts
--
--  Created: 7 April 2005
--
--  Version $Revision: 1.3 $ from $Date: 2005/08/25 01:16:15 $
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
  castToMenuToolButton,

-- * Constructors
  menuToolButtonNew,
  menuToolButtonNewFromStock,

-- * Methods
  menuToolButtonSetMenu,
  menuToolButtonGetMenu,
  menuToolButtonSetArrowTooltip,

-- * Attributes
  menuToolButtonMenu,

-- * Signals
  onShowMenu,
  afterShowMenu,
#endif
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,6,0)
--------------------
-- Constructors

-- | Creates a new 'MenuToolButton' using @iconWidget@ as icon and @label@ as
-- label.
--
menuToolButtonNew :: WidgetClass iconWidget => 
    Maybe iconWidget  -- ^ @iconWidget@ - a widget that will be used as icon
                      -- widget, or @Nothing@
 -> Maybe String      -- ^ @label@ - a string that will be used as label, or
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
    String            -- ^ @stockId@ - the name of a stock item
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

-- | Sets the 'Tooltips' object to be used for arrow button which pops up the
-- menu. See 'toolItemSetTooltip' for setting a tooltip on the whole
-- 'MenuToolButton'.
--
menuToolButtonSetArrowTooltip :: MenuToolButtonClass self => self
 -> Tooltips -- ^ @tooltips@ - the 'Tooltips' object to be used
 -> String   -- ^ @tipText@ - text to be used as tooltip text for tool item
 -> String   -- ^ @tipPrivate@ - text to be used as private tooltip text
 -> IO ()
menuToolButtonSetArrowTooltip self tooltips tipText tipPrivate =
  withUTFString tipPrivate $ \tipPrivatePtr ->
  withUTFString tipText $ \tipTextPtr ->
  {# call gtk_menu_tool_button_set_arrow_tooltip #}
    (toMenuToolButton self)
    tooltips
    tipTextPtr
    tipPrivatePtr

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
