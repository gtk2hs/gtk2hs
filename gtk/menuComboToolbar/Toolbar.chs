-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Toolbar
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.7 $ from $Date: 2004/05/23 16:05:21 $
--
--  Copyright (c) 1999..2002 Axel Simon
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- |
--
-- Create bars of buttons and derived widget.
--
-- * 'Button's, 'RadioButton's and 'ToggleButton's can be added by 
--   refering to stock
--   images. Their size can be changed by calling 
--   'toolbarSetIconSize'. In
--   contrast, normal widget cannot be added. Due to the bad interface of
--   GtkToolbar Mnemonics of 'RadioButton's and 
--   'ToggleButton's are not
--   honored.
--
-- * All the append, insert and prepend functions use an internal function to
--   do the actual work. In fact the interface is pretty skrewed up: To insert
--   icons by using stock items is definitely the best practice as all other
--   images cannot react to 'toolbarSetIconSize'
--    and other theming actions. On
--   the other hand toolbar_insert_stock() always generates simple 
--   'Button's
--   but is the only function that is able to insert 'Mnemonic's 
--   on the label.
--   Our solution is to use 'StockItem's to specify all 
--   'Images' of the 
--   'Buttons'. 
--   If the user inserts 'RadioButton's or 'ToggleButton's, 
--   the
--   stock image lookup is done manually. A mnemonic in the labels is sadly
--   not honored this way.
--

module Toolbar(
  Toolbar,
  ToolbarClass,
  castToToolbar,
  Orientation(..),
  ToolbarStyle(..),
  toolbarNew,
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
  toolbarSetOrientation,
  toolbarSetStyle,
  toolbarSetTooltips,
  IconSize,
  iconSizeInvalid,
  iconSizeSmallToolbar,
  iconSizeLargeToolbar,
  toolbarSetIconSize,
  toolbarGetIconSize,
  onOrientationChanged,
  afterOrientationChanged,
  onStyleChanged,
  afterStyleChanged
  ) where

import Monad	(liftM)
import Maybe	(fromJust, fromMaybe)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(Orientation(..), ToolbarStyle(..))
import Structs	(toolbarGetSize', toolbarChildButton, toolbarChildToggleButton,
		 toolbarChildRadioButton, IconSize, 
		 iconSizeInvalid, iconSizeSmallToolbar, iconSizeLargeToolbar)
import StockItems(stockLookupItem, siLabel, stockMissingImage) 
import Image	(imageNewFromStock)

{# context lib="gtk" prefix="gtk" #}

-- methods

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

-- | Insert a new 'Button' into the
-- 'Toolbar'.
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

-- | Append a new 'Button' to the
-- 'Toolbar'.
--
-- * See 'toolbarInsertNewButton' for details.
--
toolbarAppendNewButton :: ToolbarClass tb => tb -> String ->
                          Maybe (String,String) -> IO Button
toolbarAppendNewButton tb = toolbarInsertNewButton tb (-1)

-- | Prepend a new 'Button' to the
-- 'Toolbar'.
--
-- * See 'toolbarInsertNewButton' for details.
--
toolbarPrependNewButton :: ToolbarClass tb => tb -> String ->
                           Maybe (String,String) -> IO Button
toolbarPrependNewButton tb = toolbarInsertNewButton tb 0

-- | Insert a new 'ToggleButton'
-- into the 'Toolbar'.
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
  size <- toolbarGetSize' (toToolbar tb)
  image <- imageNewFromStock stockId size
  makeNewObject mkToggleButton $ liftM castPtr $
    withUTFString label $ \lblPtr -> mkToolText tooltips $ \textPtr privPtr ->
    {#call unsafe toolbar_insert_element#} (toToolbar tb) 
    toolbarChildToggleButton (mkWidget nullForeignPtr) lblPtr 
    textPtr privPtr (toWidget image) nullFunPtr nullPtr (fromIntegral pos)

-- | Append a new 'ToggleButton'
-- to the 'Toolbar'.
--
-- * See 'toolbarInsertNewButton' for details.
--
-- * Mnemonics in the label of the 'StockItem' are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
toolbarAppendNewToggleButton :: ToolbarClass tb => tb -> String ->
                                Maybe (String,String) -> IO ToggleButton
toolbarAppendNewToggleButton tb = toolbarInsertNewToggleButton tb (-1)

-- | Prepend a new 'ToggleButton'
-- to the 'Toolbar'.
--
-- * See 'toolbarInsertNewButton' for details.
--
-- * Mnemonics in the label of the 'StockItem' are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
toolbarPrependNewToggleButton :: ToolbarClass tb => tb -> String ->
                                 Maybe (String,String) -> IO ToggleButton
toolbarPrependNewToggleButton tb = toolbarInsertNewToggleButton tb 0

-- | Insert a new 'RadioButton'
-- into the 'Toolbar'.
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
  size <- toolbarGetSize' (toToolbar tb)
  image <- imageNewFromStock stockId size
  makeNewObject mkRadioButton $ liftM castPtr $
    withUTFString label $ \lblPtr -> mkToolText tooltips $ \textPtr privPtr ->
    {#call unsafe toolbar_insert_element#} (toToolbar tb) 
    toolbarChildRadioButton (maybe (mkWidget nullForeignPtr) toWidget rb) 
      lblPtr  textPtr privPtr (toWidget image) nullFunPtr nullPtr
      (fromIntegral pos)

-- | Append a new 'RadioButton' to
-- the 'Toolbar'.
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

-- | Prepend a new 'RadioButton'
-- to the 'Toolbar'.
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


-- | Insert an arbitrary widget to the
-- 'Toolbar'.
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

-- | Append a new 'Widget' to the
-- 'Toolbar'.
--
-- * See 'toolbarInsertNewButton' for details.
--
-- * Mnemonics in the label of the 'StockItem' are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
toolbarAppendNewWidget :: (ToolbarClass tb, WidgetClass w) => tb -> w ->
                          Maybe (String,String) -> IO ()
toolbarAppendNewWidget tb = toolbarInsertNewWidget tb (-1)

-- | Prepend a new 'Widget' to the
-- 'Toolbar'.
--
-- * See 'toolbarInsertNewButton' for details.
--
-- * Mnemonics in the label of the 'StockItem' are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
toolbarPrependNewWidget :: (ToolbarClass tb, WidgetClass w) => tb -> w ->
                           Maybe (String,String) -> IO ()
toolbarPrependNewWidget tb = toolbarInsertNewWidget tb 0


-- | Set the direction of the 'Toolbar'.
--
toolbarSetOrientation :: ToolbarClass tb => tb -> Orientation -> IO ()
toolbarSetOrientation tb orientation = {#call toolbar_set_orientation#}
  (toToolbar tb) ((fromIntegral.fromEnum) orientation)

-- | Specify how the buttons are dispayed.
--
toolbarSetStyle :: ToolbarClass tb => tb -> ToolbarStyle -> IO ()
toolbarSetStyle tb style = {#call toolbar_set_style#}
  (toToolbar tb) ((fromIntegral.fromEnum) style)

-- | Enable or disable the 'Tooltips'.
--
toolbarSetTooltips :: ToolbarClass tb => tb -> Bool -> IO ()
toolbarSetTooltips tb enable = {#call unsafe toolbar_set_tooltips#}
  (toToolbar tb) (fromBool enable)

-- | Set the size of the icons.
--
-- * It might be sensible to restrict oneself to 
--   'IconSizeSmallToolbar' and
--   'IconSizeLargeToolbar'.
--
toolbarSetIconSize :: ToolbarClass tb => tb -> IconSize -> IO ()
toolbarSetIconSize tb is = {#call toolbar_set_icon_size#} (toToolbar tb)
  (fromIntegral is)

-- | Retrieve the current icon size that the
-- 'Toolbar' shows.
--
toolbarGetIconSize :: ToolbarClass tb => tb -> IO IconSize
toolbarGetIconSize tb = toolbarGetSize' (toToolbar tb)

-- signals

-- | Emitted when toolbarSetOrientation is
-- called.
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

