{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Toolbar
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
-- Create bars of buttons and other widgets
--
module Graphics.UI.Gtk.MenuComboToolbar.Toolbar (
-- * Detail
--
-- | This widget underwent a signficant overhaul in gtk 2.4 and the
-- recommended api changed substantially. The old interface is still supported
-- but it is not recommended.
--
-- * The following information applies to the new interface only.
--
-- A toolbar is created with a call to 'toolbarNew'.
--
-- A toolbar can contain instances of a subclass of 'ToolItem'. To add a
-- 'ToolItem' to the a toolbar, use 'toolbarInsert'. To remove an item from the
-- toolbar use 'containerRemove'. To add a button to the toolbar, add an
-- instance of 'ToolButton'.
--
-- Toolbar items can be visually grouped by adding instances of
-- 'SeparatorToolItem' to the toolbar. If a 'SeparatorToolItem' has the
-- \"expand\" property set to @True@ and the \"draw\" property set to @False@
-- the effect is to force all following items to the end of the toolbar.
--
-- Creating a context menu for the toolbar can be done using
-- 'onPopupContextMenu'.

#ifndef DISABLE_DEPRECATED
-- | * The following information applies to the old interface only.
--
-- 'Button's, 'RadioButton's and 'ToggleButton's can be added by refering to
-- stock images. Their size can be changed by calling 'toolbarSetIconSize'. In
-- contrast, normal widget cannot be added. Due to the bad interface of
-- "Toolbar" mnemonics of 'RadioButton's and 'ToggleButton's are not honored.
--
-- All the append, insert and prepend functions use an internal function to
-- do the actual work. In fact the interface is pretty skrewed up: To insert
-- icons by using stock items is definitely the best practice as all other
-- images cannot react to 'toolbarSetIconSize' and other theming actions. On
-- the other hand 'toolbarInsertStock' always generates simple 'Button's
-- but is the only function that is able to insert 'Mnemonic's on the label.
-- Our solution is to use 'StockItem's to specify all 'Images' of the
-- 'Buttons'. If the user inserts 'RadioButton's or 'ToggleButton's, the stock
-- image lookup is done manually. A mnemonic in the labels is sadly not
-- honored this way.
#endif

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----Toolbar
-- @

-- * Types
  Toolbar,
  ToolbarClass,
  castToToolbar, gTypeToolbar,
  toToolbar,
  Orientation(..),
  ToolbarStyle(..),

-- * Constructors
  toolbarNew,

-- * Methods
#if GTK_MAJOR_VERSION < 3
#ifndef DISABLE_DEPRECATED
  toolbarInsertNewButton,
  toolbarAppendNewButton,
  toolbarPrependNewButton,
  toolbarInsertNewToggleButton,
  toolbarAppendNewToggleButton,
  toolbarPrependNewToggleButton,
  toolbarInsertNewRadioButton,
  toolbarAppendNewRadioButton,
  toolbarPrependNewRadioButton,
  toolbarInsertNewWidget,
  toolbarAppendNewWidget,
  toolbarPrependNewWidget,
#endif
  toolbarSetOrientation,
  toolbarGetOrientation,
#endif
  toolbarSetStyle,
  toolbarGetStyle,
  toolbarUnsetStyle,
#if GTK_MAJOR_VERSION < 3
  toolbarSetTooltips,
  toolbarGetTooltips,
#endif
  IconSize(..),
#ifndef DISABLE_DEPRECATED
  toolbarSetIconSize,
#endif
  toolbarGetIconSize,
#if GTK_CHECK_VERSION(2,4,0)
  toolbarInsert,
  toolbarGetItemIndex,
  toolbarGetNItems,
  toolbarGetNthItem,
  toolbarGetDropIndex,
  toolbarSetDropHighlightItem,
  toolbarSetShowArrow,
  toolbarGetShowArrow,
  ReliefStyle(..),
  toolbarGetReliefStyle,
#endif

-- * Attributes
#if GTK_MAJOR_VERSION < 3
  toolbarOrientation,
#endif
#if GTK_CHECK_VERSION(2,4,0)
  toolbarShowArrow,
#endif
#if GTK_CHECK_VERSION(2,8,0)
#if GTK_MAJOR_VERSION < 3
  toolbarTooltips,
#endif
#endif
  toolbarStyle,

-- * Child Attributes
  toolbarChildExpand,
  toolbarChildHomogeneous,

-- * Signals
  onOrientationChanged,
  afterOrientationChanged,
  onStyleChanged,
  afterStyleChanged,
  onPopupContextMenu,
  afterPopupContextMenu,
  ) where

import Control.Monad    (liftM)
#if GTK_MAJOR_VERSION < 3
#ifndef DISABLE_DEPRECATED
import Data.Maybe       (fromJust)
import qualified Data.Text as T (filter)
import Graphics.UI.Gtk.General.StockItems
import Graphics.UI.Gtk.Display.Image    (imageNewFromStock)
import System.Glib.UTFString
#endif
#endif
import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.Abstract.ContainerChildProperties
import Graphics.UI.Gtk.General.Enums    (Orientation(..), ToolbarStyle(..),
                                         ReliefStyle(..))
import Graphics.UI.Gtk.General.Structs  (
#if GTK_MAJOR_VERSION < 3
#ifndef DISABLE_DEPRECATED
                                         toolbarChildToggleButton,
                                         toolbarChildRadioButton,
#endif
#endif
                                         IconSize(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new toolbar.
--
toolbarNew :: IO Toolbar
toolbarNew =
  makeNewObject mkToolbar $
  liftM (castPtr :: Ptr Widget -> Ptr Toolbar) $
  {# call unsafe toolbar_new #}

--------------------
-- Methods
#if GTK_MAJOR_VERSION < 3
#ifndef DISABLE_DEPRECATED
-- Make tooltips or not?
--
mkToolText :: GlibString string => Maybe (string,string) -> (CString -> CString -> IO a) -> IO a
mkToolText Nothing               fun = fun nullPtr nullPtr
mkToolText (Just (text,private)) fun = withUTFString text $ \txtPtr ->
  withUTFString private $ \prvPtr -> fun txtPtr prvPtr

-- | Insert a new 'Button' into the 'Toolbar'.
--
-- The new 'Button' is created at position @pos@, counting from 0.
--
-- The icon and label for the button is referenced by @stockId@
-- which must be a valid entry in the 'Toolbar's Style or the
-- default 'IconFactory'.
--
-- If you whish to have 'Tooltips' added to this button you can
-- specify @Just (tipText, tipPrivate)@ , otherwise specify @Nothing@.
--
-- The newly created 'Button' is returned. Use this button to
-- add an action function with @\"connectToClicked\"@.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- Removed in Gtk3.
toolbarInsertNewButton :: (ToolbarClass self, GlibString string) => self
 -> Int
 -> StockId
 -> Maybe (string,string)
 -> IO Button
toolbarInsertNewButton self pos stockId tooltips =
  withUTFString stockId $ \stockPtr ->
  mkToolText tooltips $ \textPtr privPtr ->
  makeNewObject mkButton $ liftM castPtr $
  {# call unsafe toolbar_insert_stock #}
    (toToolbar self)
    stockPtr
    textPtr
    privPtr
    nullFunPtr
    nullPtr
    (fromIntegral pos)

-- | Append a new 'Button' to the 'Toolbar'.
--
-- See 'toolbarInsertNewButton' for details.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- Removed in Gtk3.
toolbarAppendNewButton :: (ToolbarClass self, GlibString string) => self
 -> StockId
 -> Maybe (string, string)
 -> IO Button
toolbarAppendNewButton self = toolbarInsertNewButton self (-1)

-- | Prepend a new 'Button' to the 'Toolbar'.
--
-- See 'toolbarInsertNewButton' for details.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- Removed in Gtk3.
toolbarPrependNewButton :: (ToolbarClass self, GlibString string) => self
 -> StockId
 -> Maybe (string, string)
 -> IO Button
toolbarPrependNewButton self = toolbarInsertNewButton self 0

-- | Insert a new 'ToggleButton' into the 'Toolbar'.
--
-- See 'toolbarInsertNewButton' for details.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- Removed in Gtk3.
toolbarInsertNewToggleButton :: (ToolbarClass self, GlibString string) => self
 -> Int
 -> StockId
 -> Maybe (string, string)
 -> IO ToggleButton
toolbarInsertNewToggleButton self pos stockId tooltips = do
  mItem <- stockLookupItem stockId
  item <- case mItem of
    (Just item) -> return item
    Nothing     -> liftM fromJust $ stockLookupItem stockMissingImage
  let label = (T.filter (/= '_')) $ siLabel item
  size <- toolbarGetIconSize (toToolbar self)
  image <- imageNewFromStock stockId size
  makeNewObject mkToggleButton $ liftM castPtr $
    withUTFString label $ \lblPtr -> mkToolText tooltips $ \textPtr privPtr ->
    {#call unsafe toolbar_insert_element#} (toToolbar self)
    toolbarChildToggleButton (Widget nullForeignPtr) lblPtr
    textPtr privPtr (toWidget image) nullFunPtr nullPtr (fromIntegral pos)

-- | Append a new 'ToggleButton' to the 'Toolbar'.
--
-- See 'toolbarInsertNewButton' for details.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- Removed in Gtk3.
toolbarAppendNewToggleButton :: (ToolbarClass self, GlibString string) => self
 -> StockId
 -> Maybe (string, string)
 -> IO ToggleButton
toolbarAppendNewToggleButton self = toolbarInsertNewToggleButton self (-1)

-- | Prepend a new 'ToggleButton' to the 'Toolbar'.
--
-- See 'toolbarInsertNewButton' for details.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- Removed in Gtk3.
toolbarPrependNewToggleButton :: (ToolbarClass self, GlibString string) => self
 -> StockId
 -> Maybe (string, string)
 -> IO ToggleButton
toolbarPrependNewToggleButton self = toolbarInsertNewToggleButton self 0

-- | Insert a new 'RadioButton' into the 'Toolbar'.
--
-- See 'toolbarInsertNewButton' for details.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- The @parent@ argument must be set to another
-- 'RadioButton' in the group. If @Nothing@ is given,
-- a new group is generated (which is the desired behavious for the
-- first button of a group).
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- Removed in Gtk3.
toolbarInsertNewRadioButton :: (ToolbarClass self, RadioButtonClass rb, GlibString string) => self
 -> Int
 -> StockId
 -> Maybe (string,string)
 -> Maybe rb
 -> IO RadioButton
toolbarInsertNewRadioButton self pos stockId tooltips rb = do
  mItem <- stockLookupItem stockId
  item <- case mItem of
    (Just item) -> return item
    Nothing     -> liftM fromJust $ stockLookupItem stockMissingImage
  let label = (T.filter (/= '_')) $ siLabel item
  size <- toolbarGetIconSize (toToolbar self)
  image <- imageNewFromStock stockId size
  makeNewObject mkRadioButton $ liftM castPtr $
    withUTFString label $ \lblPtr -> mkToolText tooltips $ \textPtr privPtr ->
    {#call unsafe toolbar_insert_element#} (toToolbar self)
    toolbarChildRadioButton (maybe (Widget nullForeignPtr) toWidget rb)
      lblPtr  textPtr privPtr (toWidget image) nullFunPtr nullPtr
      (fromIntegral pos)

-- | Append a new 'RadioButton' to the 'Toolbar'.
--
-- See 'toolbarInsertNewButton' for details.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- Removed in Gtk3.
toolbarAppendNewRadioButton :: (ToolbarClass self, RadioButtonClass rb, GlibString string) => self
 -> StockId
 -> Maybe (string, string)
 -> Maybe rb
 -> IO RadioButton
toolbarAppendNewRadioButton self = toolbarInsertNewRadioButton self (-1)

-- | Prepend a new 'RadioButton' to the 'Toolbar'.
--
-- See 'toolbarInsertNewButton' for details.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- Removed in Gtk3.
toolbarPrependNewRadioButton :: (ToolbarClass self, RadioButtonClass rb, GlibString string) => self
 -> StockId
 -> Maybe (string, string)
 -> Maybe rb
 -> IO RadioButton
toolbarPrependNewRadioButton self = toolbarInsertNewRadioButton self 0

-- | Insert an arbitrary widget to the 'Toolbar'.
--
-- The 'Widget' should not be a button. Adding 'Button's
-- with the 'toolbarInsertButton',... functions with stock
-- objects is much better as it takes care of theme handling.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- Removed in Gtk3.
toolbarInsertNewWidget :: (ToolbarClass self, WidgetClass w, GlibString string) => self
 -> Int
 -> w
 -> Maybe (string,string)
 -> IO ()
toolbarInsertNewWidget self pos w tooltips =
  mkToolText tooltips $ \textPtr privPtr ->
  {# call unsafe toolbar_insert_widget #}
    (toToolbar self)
    (toWidget w)
    textPtr
    privPtr
    (fromIntegral pos)

-- | Append a new 'Widget' to the 'Toolbar'.
--
-- See 'toolbarInsertNewButton' for details.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- Removed in Gtk3.
toolbarAppendNewWidget :: (ToolbarClass self, WidgetClass w, GlibString string) => self
 -> w
 -> Maybe (string, string)
 -> IO ()
toolbarAppendNewWidget self = toolbarInsertNewWidget self (-1)

-- | Prepend a new 'Widget' to the 'Toolbar'.
--
-- See 'toolbarInsertNewButton' for details.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- Removed in Gtk3.
toolbarPrependNewWidget :: (ToolbarClass self, WidgetClass w, GlibString string) => self
 -> w
 -> Maybe (string, string)
 -> IO ()
toolbarPrependNewWidget self = toolbarInsertNewWidget self 0
#endif

-- | Sets whether a toolbar should appear horizontally or vertically.
--
-- Removed in Gtk3.
toolbarSetOrientation :: ToolbarClass self => self -> Orientation -> IO ()
toolbarSetOrientation self orientation =
  {# call toolbar_set_orientation #}
    (toToolbar self)
    ((fromIntegral . fromEnum) orientation)

-- | Retrieves the current orientation of the toolbar. See
-- 'toolbarSetOrientation'.
--
-- Removed in Gtk3.
toolbarGetOrientation :: ToolbarClass self => self -> IO Orientation
toolbarGetOrientation self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe toolbar_get_orientation #}
    (toToolbar self)
#endif
-- | Alters the view of the toolbar to display either icons only, text only, or
-- both.
--
toolbarSetStyle :: ToolbarClass self => self -> ToolbarStyle -> IO ()
toolbarSetStyle self style =
  {# call toolbar_set_style #}
    (toToolbar self)
    ((fromIntegral . fromEnum) style)

-- | Retrieves whether the toolbar has text, icons, or both. See
-- 'toolbarSetStyle'.
--
toolbarGetStyle :: ToolbarClass self => self -> IO ToolbarStyle
toolbarGetStyle self =
  liftM (toEnum . fromIntegral) $
  {# call toolbar_get_style #}
    (toToolbar self)

-- | Unsets a toolbar style set with 'toolbarSetStyle', so that user
-- preferences will be used to determine the toolbar style.
--
toolbarUnsetStyle :: ToolbarClass self => self -> IO ()
toolbarUnsetStyle self =
  {# call toolbar_unset_style #}
    (toToolbar self)

#if GTK_MAJOR_VERSION < 3
-- | Sets if the tooltips of a toolbar should be active or not.
--
-- Removed in Gtk3.
toolbarSetTooltips :: ToolbarClass self => self
 -> Bool  -- ^ @enable@ - set to @False@ to disable the tooltips, or @True@ to
          -- enable them.
 -> IO ()
toolbarSetTooltips self enable =
  {# call toolbar_set_tooltips #}
    (toToolbar self)
    (fromBool enable)

-- | Retrieves whether tooltips are enabled. See 'toolbarSetTooltips'.
--
-- Removed in Gtk3.
toolbarGetTooltips :: ToolbarClass self => self -> IO Bool
toolbarGetTooltips self =
  liftM toBool $
  {# call unsafe toolbar_get_tooltips #}
    (toToolbar self)
#endif

#ifndef DISABLE_DEPRECATED
-- | This function sets the size of stock icons in the toolbar. You can call
-- it both before you add the icons and after they\'ve been added. The size you
-- set will override user preferences for the default icon size.
--
-- It might be sensible to restrict oneself to 'IconSizeSmallToolbar' and
-- 'IconSizeLargeToolbar'.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
toolbarSetIconSize :: ToolbarClass self => self -> IconSize -> IO ()
toolbarSetIconSize self iconSize =
  {# call toolbar_set_icon_size #}
    (toToolbar self)
    ((fromIntegral . fromEnum) iconSize)
#endif

-- | Retrieves the icon size for the toolbar. See 'toolbarSetIconSize'.
--
toolbarGetIconSize :: ToolbarClass self => self -> IO IconSize
toolbarGetIconSize self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe toolbar_get_icon_size #}
    (toToolbar self)

#if GTK_CHECK_VERSION(2,4,0)
-- | Insert a 'ToolItem' into the toolbar at position @pos@. If @pos@ is 0 the
-- item is prepended to the start of the toolbar. If @pos@ is negative, the
-- item is appended to the end of the toolbar.
--
-- * Available since Gtk version 2.4
--
toolbarInsert :: (ToolbarClass self, ToolItemClass item) => self
 -> item  -- ^ @item@ - a 'ToolItem'
 -> Int   -- ^ @pos@ - the position of the new item
 -> IO ()
toolbarInsert self item pos =
  {# call toolbar_insert #}
    (toToolbar self)
    (toToolItem item)
    (fromIntegral pos)

-- | Returns the position of @item@ on the toolbar, starting from 0. It is an
-- error if @item@ is not a child of the toolbar.
--
-- * Available since Gtk version 2.4
--
toolbarGetItemIndex :: (ToolbarClass self, ToolItemClass item) => self
 -> item   -- ^ @item@ - a 'ToolItem' that is a child of @toolbar@
 -> IO Int -- ^ returns the position of item on the toolbar.
toolbarGetItemIndex self item =
  liftM fromIntegral $
  {# call unsafe toolbar_get_item_index #}
    (toToolbar self)
    (toToolItem item)

-- | Returns the number of items on the toolbar.
--
-- * Available since Gtk version 2.4
--
toolbarGetNItems :: ToolbarClass self => self -> IO Int
toolbarGetNItems self =
  liftM fromIntegral $
  {# call unsafe toolbar_get_n_items #}
    (toToolbar self)

-- | Returns the @n@\'th item on toolbar, or @Nothing@ if the toolbar does not
-- contain an @n@'th item.
--
-- * Available since Gtk+ version 2.4
--
toolbarGetNthItem :: ToolbarClass self => self
 -> Int                 -- ^ @n@ - A position on the toolbar
 -> IO (Maybe ToolItem) -- ^ returns The @n@'th 'ToolItem' on the toolbar, or
                        -- @Nothing@ if there isn't an @n@\'th item.
toolbarGetNthItem self n =
  maybeNull (makeNewObject mkToolItem) $
  {# call unsafe toolbar_get_nth_item #}
    (toToolbar self)
    (fromIntegral n)

-- | Returns the position corresponding to the indicated point on toolbar.
-- This is useful when dragging items to the toolbar: this function returns the
-- position a new item should be inserted.
--
-- * Available since Gtk version 2.4
--
toolbarGetDropIndex :: ToolbarClass self => self
 -> (Int, Int)  -- ^ @(x, y)@ - coordinate of a point on the toolbar. Note that
           -- @(x, y)@ are in toolbar coordinates, not window coordinates.
 -> IO Int -- ^ returns The position corresponding to the point @(x, y)@ on
           -- the toolbar.
toolbarGetDropIndex self (x,y) =
  liftM fromIntegral $
  {# call unsafe toolbar_get_drop_index #}
    (toToolbar self)
    (fromIntegral x)
    (fromIntegral y)

-- | Highlights the toolbar to give an idea of what it would look like if @item@
-- was added to toolbar at the position indicated by @index@. If @item@ is
-- @Nothing@, highlighting is turned off (and the index is ignored).
--
-- The @toolItem@ passed to this function must not be part of any widget
-- hierarchy. When an item is set as a drop highlight item it can not added to
-- any widget hierarchy or used as highlight item for another toolbar.
--
-- * Available since Gtk version 2.4
--
toolbarSetDropHighlightItem :: (ToolbarClass self, ToolItemClass toolItem) => self
 -> Maybe toolItem -- ^ @toolItem@ - a 'ToolItem', or @Nothing@ to turn of
                   -- highlighting
 -> Int            -- ^ @index@ - a position on the toolbar
 -> IO ()
toolbarSetDropHighlightItem self toolItem index =
  {# call toolbar_set_drop_highlight_item #}
    (toToolbar self)
    (maybe (ToolItem nullForeignPtr) toToolItem toolItem)
    (fromIntegral index)

-- | Sets whether to show an overflow menu when the toolbar doesn't have room
-- for all items on it. If @True@, items that there are not room are available
-- through an overflow menu.
--
-- * Available since Gtk version 2.4
--
toolbarSetShowArrow :: ToolbarClass self => self -> Bool -> IO ()
toolbarSetShowArrow self showArrow =
  {# call toolbar_set_show_arrow #}
    (toToolbar self)
    (fromBool showArrow)

-- | Returns whether the toolbar has an overflow menu. See
-- 'toolbarSetShowArrow'.
--
-- * Available since Gtk+ version 2.4
--
toolbarGetShowArrow :: ToolbarClass self => self -> IO Bool
toolbarGetShowArrow self =
  liftM toBool $
  {# call unsafe toolbar_get_show_arrow #}
    (toToolbar self)

-- | Returns the relief style of buttons on the toolbar. See 'buttonSetRelief'.
--
-- * Available since Gtk+ version 2.4
--
toolbarGetReliefStyle :: ToolbarClass self => self -> IO ReliefStyle
toolbarGetReliefStyle self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe toolbar_get_relief_style #}
    (toToolbar self)
#endif

--------------------
-- Attributes

#if GTK_MAJOR_VERSION < 3
-- | The orientation of the toolbar.
--
-- Default value: 'OrientationHorizontal'
--
-- Removed in Gtk3.
toolbarOrientation :: ToolbarClass self => Attr self Orientation
toolbarOrientation = newAttr
  toolbarGetOrientation
  toolbarSetOrientation
#endif

-- | How to draw the toolbar.
--
-- Default value: 'ToolbarIcons'
--
toolbarStyle :: ToolbarClass self => Attr self ToolbarStyle
toolbarStyle = newAttrFromEnumProperty "toolbar-style"
  {# call pure unsafe gtk_toolbar_style_get_type #}

#if GTK_CHECK_VERSION(2,4,0)
-- | If an arrow should be shown if the toolbar doesn't fit.
--
-- Default value: @True@
--
toolbarShowArrow :: ToolbarClass self => Attr self Bool
toolbarShowArrow = newAttr
  toolbarGetShowArrow
  toolbarSetShowArrow
#endif


#if GTK_MAJOR_VERSION < 3
-- | If the tooltips of the toolbar should be active or not.
--
-- Default value: @True@
--
-- Removed in Gtk3.
toolbarTooltips :: ToolbarClass self => Attr self Bool
toolbarTooltips = newAttr
  toolbarGetTooltips
  toolbarSetTooltips
#endif

--------------------
-- Child Attributes

-- | Whether the item should receive extra space when the toolbar grows.
--
-- Default value: @True@
--
toolbarChildExpand :: (ToolbarClass self, WidgetClass child) => child -> Attr self Bool
toolbarChildExpand = newAttrFromContainerChildBoolProperty "expand"

-- | Whether the item should be the same size as other homogeneous items.
--
-- Default value: @True@
--
toolbarChildHomogeneous :: (ToolbarClass self, WidgetClass child) => child -> Attr self Bool
toolbarChildHomogeneous = newAttrFromContainerChildBoolProperty "homogeneous"

--------------------
-- Signals

-- | Emitted when the orientation of the toolbar changes.
--
onOrientationChanged, afterOrientationChanged :: ToolbarClass self => self
 -> (Orientation -> IO ())
 -> IO (ConnectId self)
onOrientationChanged = connect_ENUM__NONE "orientation-changed" False
afterOrientationChanged = connect_ENUM__NONE "orientation-changed" True

-- | Emitted when the style of the toolbar changes.
--
onStyleChanged, afterStyleChanged :: ToolbarClass self => self
 -> (ToolbarStyle -> IO ())
 -> IO (ConnectId self)
onStyleChanged = connect_ENUM__NONE "style-changed" False
afterStyleChanged = connect_ENUM__NONE "style-changed" True

-- | Emitted when the user right-clicks the toolbar or uses the keybinding to
-- display a popup menu.
--
-- Application developers should handle this signal if they want to display
-- a context menu on the toolbar. The context-menu should appear at the
-- coordinates given by @x@ and @y@. The mouse button number is given by the
-- @button@ parameter. If the menu was popped up using the keybaord, @button@
-- is -1.
--
onPopupContextMenu, afterPopupContextMenu :: ToolbarClass self => self
 -> (Int -> Int -> Int -> IO Bool) -- ^ @(\x y button -> ...)@ - The handler
                                   -- should return True if the signal was
                                   -- handled, False if not.
 -> IO (ConnectId self)
onPopupContextMenu = connect_INT_INT_INT__BOOL "popup-context-menu" False
afterPopupContextMenu = connect_INT_INT_INT__BOOL "popup-context-menu" True
