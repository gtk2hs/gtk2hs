-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Toolbar@
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.5 $ from $Date: 2003/03/24 23:56:39 $
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
-- @description@ --------------------------------------------------------------
--
-- * Toolbar: create bars of buttons and derived widget. @ref data Button@s, 
--   @ref data RadioButton@s and @ref data ToggleButton@s can be added by 
--   refering to stock
--   images. Their size can be changed by calling 
--   @ref method toolbarSetIconSize@. In
--   contrast, normal widget cannot be added. Due to the bad interface of
--   GtkToolbar Mnemonics of @ref data RadioButton@s and 
--   @ref data ToggleButton@s are not
--   honored.
--
-- @documentation@ ------------------------------------------------------------
--
-- * All the append, insert and prepend functions use an internal function to
--   do the actual work. In fact the interface is pretty skrewed up: To insert
--   icons by using stock items is definitely the best practice as all other
--   images cannot react to @ref method toolbarSetIconSize@
--    and other theming actions. On
--   the other hand toolbar_insert_stock() always generates simple 
--   @ref data Button@s
--   but is the only function that is able to insert @ref data Mnemonic@s 
--   on the label.
--   Our solution is to use @ref data StockItem@s to specify all 
--   @ref data Images@ of the 
--   @ref data Buttons@. 
--   If the user inserts @ref data RadioButton@s or @ref data ToggleButton@s, 
--   the
--   stock image lookup is done manually. A mnemonic in the labels is sadly
--   not honored this way.
--
-- @todo@ ---------------------------------------------------------------------

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
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(Orientation(..), ToolbarStyle(..))
import Structs	(toolbarGetSize', toolbarChildButton, toolbarChildToggleButton,
		 toolbarChildRadioButton, nullForeignPtr, IconSize, 
		 iconSizeInvalid, iconSizeSmallToolbar, iconSizeLargeToolbar)
import StockItems(stockLookupItem, siLabel, stockMissingImage) 
import Image	(imageNewFromStock)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor toolbarNew@ Create a new, empty toolbar.
--
toolbarNew :: IO Toolbar
toolbarNew  = makeNewObject mkToolbar $ liftM castPtr
  {#call unsafe toolbar_new#}

-- Make tooltips or not? 
--
mkToolText :: Maybe (String,String) -> (CString -> CString -> IO a) -> IO a
mkToolText Nothing               fun = fun nullPtr nullPtr
mkToolText (Just (text,private)) fun = withCString text $ \txtPtr -> 
  withCString private $ \prvPtr -> fun txtPtr prvPtr

-- @method toolbarInsertNewButton@ Insert a new @ref data Button@ into the
-- @ref data Toolbar@.
--
-- * The new @ref data Button@ is created at position @ref arg pos@, counting
--   from 0.
--
-- * The icon and label for the button is referenced by @ref arg stockId@
--   which must be a valid entry in the @ref data Toolbar@s Style or the
--   default @ref data IconFactory@.
--
-- * If you whish to have @ref data Tooltips@ added to this button you can
--   specify @literal Just (tipText, tipPrivate)@ , otherwise specify
--   @literal Nothing@.
--
-- * The newly created @ref data Button@ is returned. Use this button to 
--   add an action function with @ref signal connectToClicked@.
--
toolbarInsertNewButton :: ToolbarClass tb => tb -> Int -> String ->
                          Maybe (String,String) -> IO Button
toolbarInsertNewButton tb pos stockId tooltips = 
  withCString stockId $ \stockPtr ->
  mkToolText tooltips $ \textPtr privPtr ->
  makeNewObject mkButton $ liftM castPtr $ 
  {#call unsafe toolbar_insert_stock#} (toToolbar tb) stockPtr textPtr privPtr
    nullFunPtr nullPtr (fromIntegral pos)

-- @method toolbarAppendNewButton@ Append a new @ref data Button@ to the
-- @ref data Toolbar@.
--
-- * See @ref method toolbarInsertNewButton@ for details.
--
toolbarAppendNewButton :: ToolbarClass tb => tb -> String ->
                          Maybe (String,String) -> IO Button
toolbarAppendNewButton tb = toolbarInsertNewButton tb (-1)

-- @method toolbarPrependNewButton@ Prepend a new @ref data Button@ to the
-- @ref data Toolbar@.
--
-- * See @ref method toolbarInsertNewButton@ for details.
--
toolbarPrependNewButton :: ToolbarClass tb => tb -> String ->
                           Maybe (String,String) -> IO Button
toolbarPrependNewButton tb = toolbarInsertNewButton tb 0

-- @method toolbarInsertNewToggleButton@ Insert a new @ref data ToggleButton@
-- into the @ref data Toolbar@.
--
-- * See @ref method toolbarInsertNewButton@ for details.
--
-- * Mnemonics in the label of the @ref data StockItem@ are removed as they do
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
    withCString label $ \lblPtr -> mkToolText tooltips $ \textPtr privPtr ->
    {#call unsafe toolbar_insert_element#} (toToolbar tb) 
    toolbarChildToggleButton (mkWidget nullForeignPtr) lblPtr 
    textPtr privPtr (toWidget image) nullFunPtr nullPtr (fromIntegral pos)

-- @method toolbarAppendNewToggleButton@ Append a new @ref data ToggleButton@
-- to the @ref data Toolbar@.
--
-- * See @ref method toolbarInsertNewButton@ for details.
--
-- * Mnemonics in the label of the @ref data StockItem@ are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
toolbarAppendNewToggleButton :: ToolbarClass tb => tb -> String ->
                                Maybe (String,String) -> IO ToggleButton
toolbarAppendNewToggleButton tb = toolbarInsertNewToggleButton tb (-1)

-- @method toolbarPrependNewToggleButton@ Prepend a new @ref data ToggleButton@
-- to the @ref data Toolbar@.
--
-- * See @ref method toolbarInsertNewButton@ for details.
--
-- * Mnemonics in the label of the @ref data StockItem@ are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
toolbarPrependNewToggleButton :: ToolbarClass tb => tb -> String ->
                                 Maybe (String,String) -> IO ToggleButton
toolbarPrependNewToggleButton tb = toolbarInsertNewToggleButton tb 0

-- @method toolbarInsertNewRadioButton@ Insert a new @ref data RadioButton@
-- into the @ref data Toolbar@.
--
-- * See @ref method toolbarInsertNewButton@ for details.
--
-- * Mnemonics in the label of the @ref data StockItem@ are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
-- * The @ref arg parent@ argument must be set to another
--   @ref data RadioButton@ in the group. If @literal Nothing@ is given, 
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
    withCString label $ \lblPtr -> mkToolText tooltips $ \textPtr privPtr ->
    {#call unsafe toolbar_insert_element#} (toToolbar tb) 
    toolbarChildRadioButton (maybe (mkWidget nullForeignPtr) toWidget rb) 
      lblPtr  textPtr privPtr (toWidget image) nullFunPtr nullPtr
      (fromIntegral pos)

-- @method toolbarAppendNewRadioButton@ Append a new @ref data RadioButton@ to
-- the @ref data Toolbar@.
--
-- * See @ref method toolbarInsertNewButton@ for details.
--
-- * Mnemonics in the label of the @ref data StockItem@ are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
toolbarAppendNewRadioButton :: (ToolbarClass tb, RadioButtonClass rb) => tb ->
                               String -> Maybe (String,String) -> Maybe rb ->
                               IO RadioButton
toolbarAppendNewRadioButton tb = toolbarInsertNewRadioButton tb (-1)

-- @method toolbarPrependNewRadioButton@ Prepend a new @ref data RadioButton@
-- to the @ref data Toolbar@.
--
-- * See @ref method toolbarInsertNewButton@ for details.
--
-- * Mnemonics in the label of the @ref data StockItem@ are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
toolbarPrependNewRadioButton :: (ToolbarClass tb, RadioButtonClass rb) => tb ->
                                String -> Maybe (String,String) -> Maybe rb ->
                                IO RadioButton
toolbarPrependNewRadioButton tb = toolbarInsertNewRadioButton tb 0


-- @method toolbarInsertNewWidget@ Insert an arbitrary widget to the
-- @ref data Toolbar@.
--
-- * The @ref data Widget@ should not be a button. Adding @ref data Button@s
--   with the @ref method toolbarInsertButton@,... functions with stock
--   objects is much better as it takes care of theme handling.
--
toolbarInsertNewWidget :: (ToolbarClass tb, WidgetClass w) => tb -> Int -> w ->
                          Maybe (String,String) -> IO ()
toolbarInsertNewWidget tb pos w tooltips = 
  mkToolText tooltips $ \textPtr privPtr ->
  {#call unsafe toolbar_insert_widget#} (toToolbar tb) (toWidget w)
    textPtr privPtr (fromIntegral pos)

-- @method toolbarAppendNewWidget@ Append a new @ref data Widget@ to the
-- @ref data Toolbar@.
--
-- * See @ref method toolbarInsertNewButton@ for details.
--
-- * Mnemonics in the label of the @ref data StockItem@ are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
toolbarAppendNewWidget :: (ToolbarClass tb, WidgetClass w) => tb -> w ->
                          Maybe (String,String) -> IO ()
toolbarAppendNewWidget tb = toolbarInsertNewWidget tb (-1)

-- @method toolbarPrependNewWidget@ Prepend a new @ref data Widget@ to the
-- @ref data Toolbar@.
--
-- * See @ref method toolbarInsertNewButton@ for details.
--
-- * Mnemonics in the label of the @ref data StockItem@ are removed as they do
--   not work due to the bad interface definition of GtkToolbar.
--
toolbarPrependNewWidget :: (ToolbarClass tb, WidgetClass w) => tb -> w ->
                           Maybe (String,String) -> IO ()
toolbarPrependNewWidget tb = toolbarInsertNewWidget tb 0


-- @method toolbarSetOrientation@ Set the direction of the @ref data Toolbar@.
--
toolbarSetOrientation :: ToolbarClass tb => tb -> Orientation -> IO ()
toolbarSetOrientation tb orientation = {#call toolbar_set_orientation#}
  (toToolbar tb) ((fromIntegral.fromEnum) orientation)

-- @method toolbarSetStyle@ Specify how the buttons are dispayed.
--
toolbarSetStyle :: ToolbarClass tb => tb -> ToolbarStyle -> IO ()
toolbarSetStyle tb style = {#call toolbar_set_style#}
  (toToolbar tb) ((fromIntegral.fromEnum) style)

-- @method toolbarSetTooltips@ Enable or disable the @ref type Tooltips@.
--
toolbarSetTooltips :: ToolbarClass tb => tb -> Bool -> IO ()
toolbarSetTooltips tb enable = {#call unsafe toolbar_set_tooltips#}
  (toToolbar tb) (fromBool enable)

-- @method toolbarSetIconSize@ Set the size of the icons.
--
-- * It might be sensible to restrict oneself to 
--   @ref variant IconSizeSmallToolbar@ and
--   @ref variant IconSizeLargeToolbar@.
--
toolbarSetIconSize :: ToolbarClass tb => tb -> IconSize -> IO ()
toolbarSetIconSize tb is = {#call toolbar_set_icon_size#} (toToolbar tb)
  (fromIntegral is)

-- @method toolbarGetIconSize@ Retrieve the current icon size that the
-- @ref data Toolbar@ shows.
--
toolbarGetIconSize :: ToolbarClass tb => tb -> IO IconSize
toolbarGetIconSize tb = toolbarGetSize' (toToolbar tb)

-- signals

-- @signal connectToOrientationChanged@ Emitted when toolbarSetOrientation is
-- called.
--
onOrientationChanged, afterOrientationChanged :: ToolbarClass tb => tb ->
                                                 (Orientation -> IO ()) ->
                                                 IO (ConnectId tb)
onOrientationChanged = connect_ENUM__NONE "orientation-changed" False
afterOrientationChanged = connect_ENUM__NONE "orientation-changed" True

-- @signal connectToStyleChanged@ Emitted when toolbarSetStyle is called.
--
onStyleChanged, afterStyleChanged :: ToolbarClass tb => tb ->
                                     (ToolbarStyle -> IO ()) ->
                                     IO (ConnectId tb)
onStyleChanged = connect_ENUM__NONE "style-changed" False
afterStyleChanged = connect_ENUM__NONE "style-changed" True

