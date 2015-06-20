{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ToolButton
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
-- A 'ToolItem' subclass that displays buttons
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.MenuComboToolbar.ToolButton (
-- * Detail
--
-- | 'ToolButton's are 'ToolItems' containing buttons.
--
-- Use 'toolButtonNew' to create a new 'ToolButton'. Use
-- 'toolButtonNewWithStock' to create a 'ToolButton' containing a stock item.
--
-- The label of a 'ToolButton' is determined by the properties
-- \"label_widget\", \"label\", and \"stock_id\". If \"label_widget\" is
-- not @Nothing@,
-- then that widget is used as the label. Otherwise, if \"label\" is
-- not @Nothing@,
-- that string is used as the label. Otherwise, if \"stock_id\" is not
-- @Nothing@, the label is
-- determined by the stock item. Otherwise, the button does not have a label.
--
-- The icon of a 'ToolButton' is determined by the properties
-- \"icon_widget\" and \"stock_id\". If \"icon_widget\" is not @Nothing@, then
-- that widget is used as the icon. Otherwise, if \"stock_id\" is not @Nothing@,
-- the icon is determined by the stock item. Otherwise, the button does not have
-- a label.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'ToolItem'
-- |                                 +----ToolButton
-- |                                       +----'MenuToolButton'
-- |                                       +----'ToggleToolButton'
-- @

#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  ToolButton,
  ToolButtonClass,
  castToToolButton, gTypeToolButton,
  toToolButton,

-- * Constructors
  toolButtonNew,
  toolButtonNewFromStock,

-- * Methods
  toolButtonSetLabel,
  toolButtonGetLabel,
  toolButtonSetUseUnderline,
  toolButtonGetUseUnderline,
  toolButtonSetStockId,
  toolButtonGetStockId,
  toolButtonSetIconWidget,
  toolButtonGetIconWidget,
  toolButtonSetLabelWidget,
  toolButtonGetLabelWidget,
#if GTK_CHECK_VERSION(2,8,0)
  toolButtonSetIconName,
  toolButtonGetIconName,
#endif

-- * Attributes
  toolButtonLabel,
  toolButtonUseUnderline,
  toolButtonLabelWidget,
  toolButtonStockId,
#if GTK_CHECK_VERSION(2,8,0)
  toolButtonIconName,
#endif
  toolButtonIconWidget,

-- * Signals
  onToolButtonClicked,
  afterToolButtonClicked,
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

#if GTK_CHECK_VERSION(2,4,0)
--------------------
-- Constructors

-- | Creates a new 'ToolButton' using @iconWidget@ as icon and @label@ as
-- label.
--
toolButtonNew :: (WidgetClass iconWidget, GlibString string) =>
    Maybe iconWidget -- ^ @iconWidget@ - a widget that will be used as icon
                     -- widget, or @Nothing@
 -> Maybe string     -- ^ @label@ - a string that will be used as label, or
                     -- @Nothing@
 -> IO ToolButton
toolButtonNew iconWidget label =
  makeNewObject mkToolButton $
  liftM (castPtr :: Ptr ToolItem -> Ptr ToolButton) $
  maybeWith withUTFString label $ \labelPtr ->
  {# call gtk_tool_button_new #}
    (maybe (Widget nullForeignPtr) toWidget iconWidget)
    labelPtr

-- | Creates a new 'ToolButton' containing the image and text from a stock
-- item.
--
-- It is an error if @stockId@ is not a name of a stock item.
--
toolButtonNewFromStock ::
    StockId       -- ^ @stockId@ - the name of the stock item
 -> IO ToolButton
toolButtonNewFromStock stockId =
  makeNewObject mkToolButton $
  liftM (castPtr :: Ptr ToolItem -> Ptr ToolButton) $
  withUTFString stockId $ \stockIdPtr ->
  {# call gtk_tool_button_new_from_stock #}
    stockIdPtr

--------------------
-- Methods

-- | Sets @label@ as the label used for the tool button. The \"label\"
-- property only has an effect if not overridden by a non-@Nothing@
-- \"label_widget\" property. If both the \"label_widget\" and \"label\"
-- properties are @Nothing@, the label is determined by the \"stock_id\"
-- property. If the \"stock_id\" property is also @Nothing@, @button@ will not
-- have a label.
--
toolButtonSetLabel :: (ToolButtonClass self, GlibString string) => self
 -> Maybe string -- ^ @label@ - a string that will be used as label, or
                 -- @Nothing@.
 -> IO ()
toolButtonSetLabel self label =
  maybeWith withUTFString label $ \labelPtr ->
  {# call gtk_tool_button_set_label #}
    (toToolButton self)
    labelPtr

-- | Returns the label used by the tool button, or @Nothing@ if the tool
-- button doesn't have a label. or uses a the label from a stock item.
--
toolButtonGetLabel :: (ToolButtonClass self, GlibString string) => self -> IO (Maybe string)
toolButtonGetLabel self =
  {# call gtk_tool_button_get_label #}
    (toToolButton self)
  >>= maybePeek peekUTFString

-- | If set, an underline in the label property indicates that the next
-- character should be used for the mnemonic accelerator key in the overflow
-- menu. For example, if the label property is \"_Open\" and @useUnderline@ is
-- @True@, the label on the tool button will be \"Open\" and the item on the
-- overflow menu will have an underlined \'O\'.
--
-- Labels shown on tool buttons never have mnemonics on them; this property
-- only affects the menu item on the overflow menu.
--
toolButtonSetUseUnderline :: ToolButtonClass self => self -> Bool -> IO ()
toolButtonSetUseUnderline self useUnderline =
  {# call gtk_tool_button_set_use_underline #}
    (toToolButton self)
    (fromBool useUnderline)

-- | Returns whether underscores in the label property are used as mnemonics
-- on menu items on the overflow menu. See 'toolButtonSetUseUnderline'.
--
toolButtonGetUseUnderline :: ToolButtonClass self => self -> IO Bool
toolButtonGetUseUnderline self =
  liftM toBool $
  {# call gtk_tool_button_get_use_underline #}
    (toToolButton self)

-- | Sets the name of the stock item. See 'toolButtonNewFromStock'. The
-- stock_id property only has an effect if not overridden by non-@Nothing@
-- \"label\" and \"icon_widget\" properties.
--
toolButtonSetStockId :: ToolButtonClass self => self
 -> Maybe StockId -- ^ @stockId@ - a name of a stock item, or @Nothing@
 -> IO ()
toolButtonSetStockId self stockId =
  maybeWith withUTFString stockId $ \stockIdPtr ->
  {# call gtk_tool_button_set_stock_id #}
    (toToolButton self)
    stockIdPtr

-- | Returns the name of the stock item. See 'toolButtonSetStockId'.
--
toolButtonGetStockId :: ToolButtonClass self => self -> IO (Maybe StockId)
toolButtonGetStockId self =
  {# call gtk_tool_button_get_stock_id #}
    (toToolButton self)
  >>= maybePeek peekUTFString

-- | Sets @icon@ as the widget used as icon on @button@. If @iconWidget@ is
-- @Nothing@ the icon is determined by the \"stock_id\" property. If the
-- \"stock_id\" property is also @Nothing@, the button will not have an icon.
--
toolButtonSetIconWidget :: (ToolButtonClass self, WidgetClass iconWidget) => self
 -> Maybe iconWidget -- ^ @iconWidget@ - the widget used as icon, or @Nothing@
 -> IO ()
toolButtonSetIconWidget self iconWidget =
  {# call gtk_tool_button_set_icon_widget #}
    (toToolButton self)
    (maybe (Widget nullForeignPtr) toWidget iconWidget)

-- | Return the widget used as icon widget on @button@. See
-- 'toolButtonSetIconWidget'.
--
toolButtonGetIconWidget :: ToolButtonClass self => self
 -> IO (Maybe Widget) -- ^ returns The widget used as icon on @button@, or
                      -- @Nothing@.
toolButtonGetIconWidget self =
  maybeNull (makeNewObject mkWidget) $
  {# call gtk_tool_button_get_icon_widget #}
    (toToolButton self)

-- | Sets @labelWidget@ as the widget that will be used as the label for
-- @button@. If @labelWidget@ is @Nothing@ the \"label\" property is used as
-- label. If \"label\" is also @Nothing@, the label in the stock item
-- determined by the \"stock_id\" property is used as label. If \"stock_id\" is
-- also @Nothing@, @button@ does not have a label.
--
toolButtonSetLabelWidget :: (ToolButtonClass self, WidgetClass labelWidget) => self
 -> Maybe labelWidget -- ^ @labelWidget@ - the widget used as label, or
                      -- @Nothing@
 -> IO ()
toolButtonSetLabelWidget self labelWidget =
  {# call gtk_tool_button_set_label_widget #}
    (toToolButton self)
    (maybe (Widget nullForeignPtr) toWidget labelWidget)

-- | Returns the widget used as label on @button@. See
-- 'toolButtonSetLabelWidget'.
--
toolButtonGetLabelWidget :: ToolButtonClass self => self
 -> IO (Maybe Widget) -- ^ returns The widget used as label on @button@, or
                      -- @Nothing@.
toolButtonGetLabelWidget self =
  maybeNull (makeNewObject mkWidget) $
  {# call gtk_tool_button_get_label_widget #}
    (toToolButton self)

#if GTK_CHECK_VERSION(2,8,0)
-- | Sets the icon for the tool button from a named themed icon. See the docs
-- for 'IconTheme' for more details. The \"icon_name\" property only has an
-- effect if not overridden by the \"label\", \"icon_widget\" and \"stock_id\"
-- properties.
--
-- * Available since Gtk+ version 2.8
--
toolButtonSetIconName :: (ToolButtonClass self, GlibString string) => self
 -> string -- ^ @iconName@ - the name of the themed icon
 -> IO ()
toolButtonSetIconName self iconName =
  withUTFString iconName $ \iconNamePtr ->
  {# call gtk_tool_button_set_icon_name #}
    (toToolButton self)
    iconNamePtr

-- | Returns the name of the themed icon for the tool button, see
-- 'toolButtonSetIconName'.
--
-- * Available since Gtk+ version 2.8
--
toolButtonGetIconName :: (ToolButtonClass self, GlibString string) => self
 -> IO string -- ^ returns the icon name or @\"\"@ if the tool button has no
              -- themed icon.
toolButtonGetIconName self =
  {# call gtk_tool_button_get_icon_name #}
    (toToolButton self)
  >>= \strPtr -> if strPtr == nullPtr
                then return ""
                else peekUTFString strPtr
#endif

--------------------
-- Attributes

-- | Text to show in the item.
--
-- Default value: @Nothing@
--
toolButtonLabel :: (ToolButtonClass self, GlibString string) => Attr self (Maybe string)
toolButtonLabel = newAttr
  toolButtonGetLabel
  toolButtonSetLabel

-- | If set, an underline in the label property indicates that the next
-- character should be used for the mnemonic accelerator key in the overflow
-- menu.
--
-- Default value: @False@
--
toolButtonUseUnderline :: ToolButtonClass self => Attr self Bool
toolButtonUseUnderline = newAttr
  toolButtonGetUseUnderline
  toolButtonSetUseUnderline

-- | Widget to use as the item label.
--
toolButtonLabelWidget :: (ToolButtonClass self, WidgetClass labelWidget) => ReadWriteAttr self (Maybe Widget) (Maybe labelWidget)
toolButtonLabelWidget = newAttr
  toolButtonGetLabelWidget
  toolButtonSetLabelWidget

-- | The stock icon displayed on the item.
--
-- Default value: @Nothing@
--
toolButtonStockId :: ToolButtonClass self => ReadWriteAttr self (Maybe StockId) (Maybe StockId)
toolButtonStockId = newAttr
  toolButtonGetStockId
  toolButtonSetStockId

#if GTK_CHECK_VERSION(2,8,0)
-- | The name of the themed icon displayed on the item. This property only has
-- an effect if not overridden by \"label\", \"icon_widget\" or \"stock_id\"
-- properties.
--
-- Default value: \"\"
--
toolButtonIconName :: (ToolButtonClass self, GlibString string) => Attr self string
toolButtonIconName = newAttr
  toolButtonGetIconName
  toolButtonSetIconName
#endif

-- | Icon widget to display in the item.
--
toolButtonIconWidget :: (ToolButtonClass self, WidgetClass iconWidget) => ReadWriteAttr self (Maybe Widget) (Maybe iconWidget)
toolButtonIconWidget = newAttr
  toolButtonGetIconWidget
  toolButtonSetIconWidget

--------------------
-- Signals

-- | This signal is emitted when the tool button is clicked with the mouse or
-- activated with the keyboard.
--
onToolButtonClicked, afterToolButtonClicked :: ToolButtonClass self => self
 -> IO ()
 -> IO (ConnectId self)
onToolButtonClicked = connect_NONE__NONE "clicked" False
afterToolButtonClicked = connect_NONE__NONE "clicked" True
#endif
