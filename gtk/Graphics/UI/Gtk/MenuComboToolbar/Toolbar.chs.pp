-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Toolbar
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
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
-- Create bars of buttons and other widgets.
--
module Graphics.UI.Gtk.MenuComboToolbar.Toolbar (
-- * Description
-- 
-- | This widget underwent a signficant overhaul in gtk 2.4 and the
-- recommended api changed substantially. The old interface is still supported
-- but it is not recommended.
--
-- * The following information applies to the new interface only.
--
-- A toolbar is created with a call to 'toolbarNew'.
--
-- A toolbar can contain instances of a subclass of "ToolItem". To add a
-- "ToolItem" to the a toolbar, use 'toolbarInsert'. To remove an item from the
-- toolbar use 'containerRemove'. To add a button to the toolbar, add an
-- instance of "ToolButton".
--
-- Toolbar items can be visually grouped by adding instances of
-- "SeparatorToolItem" to the toolbar. If a "SeparatorToolItem" has the
-- \"expand\" property set to True and the \"draw\" property set to False
-- the effect is to force all following items to the end of the toolbar.
--
-- Creating a context menu for the toolbar can be done using
-- 'onPopupContextMenu'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----Toolbar
-- @

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

-- * Types
  Toolbar,
  ToolbarClass,
  castToToolbar,
  Orientation(..),
  ToolbarStyle(..),

-- * Constructors
  toolbarNew,

-- * Methods
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
  toolbarSetStyle,
  toolbarGetStyle,
  toolbarUnsetStyle,
  toolbarSetTooltips,
  toolbarGetTooltips,
  IconSize,
  iconSizeInvalid,
  iconSizeSmallToolbar,
  iconSizeLargeToolbar,
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
  onOrientationChanged,
  afterOrientationChanged,
  onStyleChanged,
  afterStyleChanged,
  onPopupContextMenu,
  afterPopupContextMenu
  ) where

import Monad	(liftM)
import Maybe	(fromJust, fromMaybe)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums	(Orientation(..), ToolbarStyle(..),
					 ReliefStyle(..))
import Graphics.UI.Gtk.General.Structs	(
#ifndef DISABLE_DEPRECATED
					 toolbarChildToggleButton,
					 toolbarChildRadioButton,
#endif
					 IconSize, iconSizeInvalid,
					 iconSizeSmallToolbar,
					 iconSizeLargeToolbar)
import Graphics.UI.Gtk.General.StockItems	(stockLookupItem, siLabel, stockMissingImage)
import Graphics.UI.Gtk.Display.Image	(imageNewFromStock)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new, empty toolbar.
--
toolbarNew :: IO Toolbar
toolbarNew  = makeNewObject mkToolbar $ liftM castPtr
  {#call unsafe toolbar_new#}

-- Make tooltips or not? 
--
mkToolText :: Maybe (String,String) -> (CString -> CString -> IO a) -> IO a
mkToolText Nothing               fun = fun nullPtr nullPtr
mkToolText (Just (text,private)) fun = withUTFString text $ \txtPtr -> 
  withUTFString private $ \prvPtr -> fun txtPtr prvPtr

--------------------
-- Methods

#ifndef DISABLE_DEPRECATED
-- | Insert a new 'Button' into the 'Toolbar'.
--
-- * The new 'Button' is created at position @pos@, counting
--   from 0.
--
-- * The icon and label for the button is referenced by @stockId@
--   which must be a valid entry in the 'Toolbar's Style or the
--   default 'IconFactory'.
--
-- * If you whish to have 'Tooltips' added to this button you can
--   specify @Just (tipText, tipPrivate)@ , otherwise specify
--   @Nothing@.
--
-- * The newly created 'Button' is returned. Use this button to 
--   add an action function with @\"connectToClicked\"@.
--
toolbarInsertNewButton :: ToolbarClass tb => tb -> Int -> String ->
                          Maybe (String,String) -> IO Button
toolbarInsertNewButton tb pos stockId tooltips = 
  withUTFString stockId $ \stockPtr ->
  mkToolText tooltips $ \textPtr privPtr ->
  makeNewObject mkButton $ liftM castPtr $ 
  {#call unsafe toolbar_insert_stock#} (toToolbar tb) stockPtr textPtr privPtr
    nullFunPtr nullPtr (fromIntegral pos)

-- | Append a new 'Button' to the 'Toolbar'.
--
-- * See 'toolbarInsertNewButton' for details.
--
toolbarAppendNewButton :: ToolbarClass tb => tb -> String ->
                          Maybe (String,String) -> IO Button
toolbarAppendNewButton tb = toolbarInsertNewButton tb (-1)

-- | Prepend a new 'Button' to the 'Toolbar'.
--
-- * See 'toolbarInsertNewButton' for details.
--
toolbarPrependNewButton :: ToolbarClass tb => tb -> String ->
                           Maybe (String,String) -> IO Button
toolbarPrependNewButton tb = toolbarInsertNewButton tb 0

-- | Insert a new 'ToggleButton' into the 'Toolbar'.
--
-- * See 'toolbarInsertNewButton' for details.
--
-- * Mnemonics in the label of the 'StockItem' are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
toolbarInsertNewToggleButton :: ToolbarClass tb => tb -> Int -> String ->
                                Maybe (String,String) -> IO ToggleButton
toolbarInsertNewToggleButton tb pos stockId tooltips = do
  mItem <- stockLookupItem stockId
  item <- case mItem of
    (Just item) -> return item
    Nothing	-> liftM fromJust $ stockLookupItem stockMissingImage
  let label = (filter (/= '_')) $ siLabel item
  size <- toolbarGetIconSize (toToolbar tb)
  image <- imageNewFromStock stockId size
  makeNewObject mkToggleButton $ liftM castPtr $
    withUTFString label $ \lblPtr -> mkToolText tooltips $ \textPtr privPtr ->
    {#call unsafe toolbar_insert_element#} (toToolbar tb) 
    toolbarChildToggleButton (mkWidget nullForeignPtr) lblPtr 
    textPtr privPtr (toWidget image) nullFunPtr nullPtr (fromIntegral pos)

-- | Append a new 'ToggleButton' to the 'Toolbar'.
--
-- * See 'toolbarInsertNewButton' for details.
--
-- * Mnemonics in the label of the 'StockItem' are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
toolbarAppendNewToggleButton :: ToolbarClass tb => tb -> String ->
                                Maybe (String,String) -> IO ToggleButton
toolbarAppendNewToggleButton tb = toolbarInsertNewToggleButton tb (-1)

-- | Prepend a new 'ToggleButton' to the 'Toolbar'.
--
-- * See 'toolbarInsertNewButton' for details.
--
-- * Mnemonics in the label of the 'StockItem' are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
toolbarPrependNewToggleButton :: ToolbarClass tb => tb -> String ->
                                 Maybe (String,String) -> IO ToggleButton
toolbarPrependNewToggleButton tb = toolbarInsertNewToggleButton tb 0

-- | Insert a new 'RadioButton' into the 'Toolbar'.
--
-- * See 'toolbarInsertNewButton' for details.
--
-- * Mnemonics in the label of the 'StockItem' are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
-- * The @parent@ argument must be set to another
--   'RadioButton' in the group. If @Nothing@ is given, 
--   a new group is generated (which is the desired behavious for the
--   first button of a group).
--
toolbarInsertNewRadioButton :: (ToolbarClass tb, RadioButtonClass rb) => tb ->
                               Int -> String -> Maybe (String,String) ->
                               Maybe rb -> IO RadioButton
toolbarInsertNewRadioButton tb pos stockId tooltips rb = do
  mItem <- stockLookupItem stockId
  item <- case mItem of
    (Just item) -> return item
    Nothing	-> liftM fromJust $ stockLookupItem stockMissingImage
  let label = (filter (/= '_')) $ siLabel item
  size <- toolbarGetIconSize (toToolbar tb)
  image <- imageNewFromStock stockId size
  makeNewObject mkRadioButton $ liftM castPtr $
    withUTFString label $ \lblPtr -> mkToolText tooltips $ \textPtr privPtr ->
    {#call unsafe toolbar_insert_element#} (toToolbar tb) 
    toolbarChildRadioButton (maybe (mkWidget nullForeignPtr) toWidget rb) 
      lblPtr  textPtr privPtr (toWidget image) nullFunPtr nullPtr
      (fromIntegral pos)

-- | Append a new 'RadioButton' to the 'Toolbar'.
--
-- * See 'toolbarInsertNewButton' for details.
--
-- * Mnemonics in the label of the 'StockItem' are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
toolbarAppendNewRadioButton :: (ToolbarClass tb, RadioButtonClass rb) => tb ->
                               String -> Maybe (String,String) -> Maybe rb ->
                               IO RadioButton
toolbarAppendNewRadioButton tb = toolbarInsertNewRadioButton tb (-1)

-- | Prepend a new 'RadioButton' to the 'Toolbar'.
--
-- * See 'toolbarInsertNewButton' for details.
--
-- * Mnemonics in the label of the 'StockItem' are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
toolbarPrependNewRadioButton :: (ToolbarClass tb, RadioButtonClass rb) => tb ->
                                String -> Maybe (String,String) -> Maybe rb ->
                                IO RadioButton
toolbarPrependNewRadioButton tb = toolbarInsertNewRadioButton tb 0


-- | Insert an arbitrary widget to the 'Toolbar'.
--
-- * The 'Widget' should not be a button. Adding 'Button's
--   with the 'toolbarInsertButton',... functions with stock
--   objects is much better as it takes care of theme handling.
--
toolbarInsertNewWidget :: (ToolbarClass tb, WidgetClass w) => tb -> Int -> w ->
                          Maybe (String,String) -> IO ()
toolbarInsertNewWidget tb pos w tooltips = 
  mkToolText tooltips $ \textPtr privPtr ->
  {#call unsafe toolbar_insert_widget#} (toToolbar tb) (toWidget w)
    textPtr privPtr (fromIntegral pos)

-- | Append a new 'Widget' to the 'Toolbar'.
--
-- * See 'toolbarInsertNewButton' for details.
--
-- * Mnemonics in the label of the 'StockItem' are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
toolbarAppendNewWidget :: (ToolbarClass tb, WidgetClass w) => tb -> w ->
                          Maybe (String,String) -> IO ()
toolbarAppendNewWidget tb = toolbarInsertNewWidget tb (-1)

-- | Prepend a new 'Widget' to the 'Toolbar'.
--
-- * See 'toolbarInsertNewButton' for details.
--
-- * Mnemonics in the label of the 'StockItem' are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
toolbarPrependNewWidget :: (ToolbarClass tb, WidgetClass w) => tb -> w ->
                           Maybe (String,String) -> IO ()
toolbarPrependNewWidget tb = toolbarInsertNewWidget tb 0
#endif


-- | Set the direction of the 'Toolbar'.
--
toolbarSetOrientation :: ToolbarClass tb => tb -> Orientation -> IO ()
toolbarSetOrientation tb orientation = {#call toolbar_set_orientation#}
  (toToolbar tb) ((fromIntegral.fromEnum) orientation)

-- | Get the direction of the 'Toolbar'.
--
toolbarGetOrientation :: ToolbarClass tb => tb -> IO Orientation
toolbarGetOrientation tb = liftM (toEnum.fromIntegral) $
  {#call unsafe toolbar_get_orientation#} (toToolbar tb)

-- | Alters the view of the toolbar to display either icons only, text only, or
-- both.
--
toolbarSetStyle :: ToolbarClass tb => tb -> ToolbarStyle -> IO ()
toolbarSetStyle tb style = {#call toolbar_set_style#}
  (toToolbar tb) ((fromIntegral.fromEnum) style)

-- | Retrieves whether the toolbar has text, icons, or both.
--
toolbarGetStyle :: ToolbarClass tb => tb -> IO ToolbarStyle
toolbarGetStyle tb = liftM (toEnum.fromIntegral) $
  {#call toolbar_get_style#} (toToolbar tb)

-- | Unsets a toolbar style set with 'toolbarSetStyle', so that user preferences
-- will be used to determine the toolbar style.
--
toolbarUnsetStyle :: ToolbarClass tb => tb -> IO ()
toolbarUnsetStyle tb =
  {#call toolbar_unset_style#} (toToolbar tb)

-- | Enable or disable the 'Tooltips'.
--
toolbarSetTooltips :: ToolbarClass tb => tb -> Bool -> IO ()
toolbarSetTooltips tb enable = {#call toolbar_set_tooltips#}
  (toToolbar tb) (fromBool enable)

-- | Enable or disable the 'Tooltips'.
--
toolbarGetTooltips :: ToolbarClass tb => tb -> IO Bool
toolbarGetTooltips tb =
  liftM toBool $ {#call unsafe toolbar_get_tooltips#} (toToolbar tb)

#ifndef DISABLE_DEPRECATED
-- | Set the size of the icons.
--
-- * It might be sensible to restrict oneself to 'IconSizeSmallToolbar' and
--   'IconSizeLargeToolbar'.
--
toolbarSetIconSize :: ToolbarClass tb => tb -> IconSize -> IO ()
toolbarSetIconSize tb is = {#call toolbar_set_icon_size#} (toToolbar tb)
  (fromIntegral is)
#endif

-- | Retrieve the current icon size that the 'Toolbar' shows.
--
toolbarGetIconSize :: ToolbarClass tb => tb -> IO IconSize
toolbarGetIconSize tb = liftM (toEnum.fromIntegral) $
  {#call unsafe toolbar_get_icon_size#} (toToolbar tb)

#if GTK_CHECK_VERSION(2,4,0)
-- | Insert a "ToolItem" into the toolbar at the given position.
--
-- * If the position is 0 the item is prepended to the start of the toolbar.
-- If the position is negative, the item is appended to the end of the toolbar.
--
toolbarInsert :: (ToolbarClass tb, ToolItemClass item) => tb
              -> item -> Int -> IO ()
toolbarInsert tb item pos =
  {#call toolbar_insert#} (toToolbar tb) (toToolItem item) (fromIntegral pos)

-- | Returns the position of item on the toolbar, starting from 0.
--
toolbarGetItemIndex :: (ToolbarClass tb, ToolItemClass item) => tb
                    -> item -> IO Int
toolbarGetItemIndex tb item = liftM fromIntegral $
  {#call unsafe toolbar_get_item_index#} (toToolbar tb) (toToolItem item)

-- | Returns the number of items on the toolbar.
--
toolbarGetNItems :: ToolbarClass tb => tb -> IO Int
toolbarGetNItems tb = liftM fromIntegral $
  {#call unsafe toolbar_get_n_items#} (toToolbar tb)

-- | Returns the n'th item on toolbar, or Nothing if the toolbar does not
-- contain an n'th item.
--
toolbarGetNthItem :: ToolbarClass tb => tb -> Int -> IO (Maybe ToolItem)
toolbarGetNthItem tb index = do
  toolItemPtr <- {#call unsafe toolbar_get_nth_item#} (toToolbar tb)
    (fromIntegral index)
  if toolItemPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkToolItem $ return toolItemPtr

-- | Returns the position corresponding to the indicated point on toolbar. This
-- is useful when dragging items to the toolbar: this function returns the
-- position a new item should be inserted.
--
-- * x and y are in toolbar coordinates.
--
toolbarGetDropIndex :: ToolbarClass tb => tb
                    -> (Int, Int)  -- ^ x,y coordinate of a point on the toolbar
                    -> IO Int
toolbarGetDropIndex tb (x,y) = liftM fromIntegral $
  {#call unsafe toolbar_get_drop_index#} (toToolbar tb)
    (fromIntegral x) (fromIntegral y)

-- | Highlights the toolbar to give an idea of what it would look like if item
-- was added to toolbar at the position indicated by the given index. If item is
-- Nothing, highlighting is turned off (and the index is ignored).
--
-- * Note: the ToolItem passed to this function must not be part of any widget
-- hierarchy. When an item is set as a drop highlight item it can not added to
-- any widget hierarchy or used as highlight item for another toolbar.
--
toolbarSetDropHighlightItem :: ToolbarClass tb => tb
                            -> Maybe ToolItem -- ^ A "ToolItem" or Nothing
                            -> Int            -- ^ A position on the toolbar
                            -> IO ()
toolbarSetDropHighlightItem tb item pos =
  {#call toolbar_set_drop_highlight_item#} (toToolbar tb)
    (fromMaybe (ToolItem nullForeignPtr) item) (fromIntegral pos)

-- | Sets whether to show an overflow menu when the toolbar doesn't have room
-- for all items on it.
--
toolbarSetShowArrow :: ToolbarClass tb => tb -> Bool -> IO ()
toolbarSetShowArrow tb showArrow =
  {#call toolbar_set_show_arrow#} (toToolbar tb) (fromBool showArrow)

-- | Returns whether the toolbar has an overflow menu.
--
toolbarGetShowArrow :: ToolbarClass tb => tb -> IO Bool
toolbarGetShowArrow tb = liftM toBool $
  {#call unsafe toolbar_get_show_arrow#} (toToolbar tb)

-- | Returns the relief style of buttons on the toolbar. See 'buttonSetRelief'.
--
toolbarGetReliefStyle :: ToolbarClass tb => tb -> IO ReliefStyle
toolbarGetReliefStyle tb = liftM (toEnum.fromIntegral) $
  {#call unsafe toolbar_get_relief_style#} (toToolbar tb)
#endif

--------------------
-- Signals

-- | Emitted when toolbarSetOrientation is called.
--
onOrientationChanged, afterOrientationChanged :: ToolbarClass tb => tb ->
                                                 (Orientation -> IO ()) ->
                                                 IO (ConnectId tb)
onOrientationChanged = connect_ENUM__NONE "orientation-changed" False
afterOrientationChanged = connect_ENUM__NONE "orientation-changed" True

-- | Emitted when toolbarSetStyle is called.
--
onStyleChanged, afterStyleChanged :: ToolbarClass tb => tb ->
                                     (ToolbarStyle -> IO ()) ->
                                     IO (ConnectId tb)
onStyleChanged = connect_ENUM__NONE "style-changed" False
afterStyleChanged = connect_ENUM__NONE "style-changed" True

-- | Emitted when the user right-clicks the toolbar or uses the keybinding to
-- display a popup menu.
--
-- * The handler should return True if the signal was handled, False if not.
--
onPopupContextMenu, afterPopupContextMenu :: ToolbarClass tb => tb ->
                                             (Int -> Int -> Int -> IO Bool) ->
                                             IO (ConnectId tb)
onPopupContextMenu = connect_INT_INT_INT__BOOL "popup-context-menu" False
afterPopupContextMenu = connect_INT_INT_INT__BOOL "popup-context-menu" True
