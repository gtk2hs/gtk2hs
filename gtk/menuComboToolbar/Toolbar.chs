-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Toolbar
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
--
--  Copyright (c) [1999.2001] Manuel Chakravarty, Axel Simon
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
--- DESCRIPTION ---------------------------------------------------------------
--
-- * Toolbar: create bars of buttons and derived widget. @Button@s, 
--   @RadioButton@s and @ToggleButton@s can be added by refering to stock
--   images. Their size can be changed by calling @toolbarSetIconSize. In
--   contrast, normal widget cannot be added. Due to the bad interface of
--   GtkToolbar Mnemonics of @RadioButton@s and @ToggleButton@s are not
--   honored.
--
--- DOCU ----------------------------------------------------------------------
--
-- * All the append, insert and prepend functions use an internal function to
--   do the actual work. In fact the interface is pretty skrewed up: To insert
--   icons by using stock items is definitely the best practice as all other
--   images cannot react to @toolbarSetIconSize and other theming actions. On
--   the other hand toolbar_insert_stock() always generates simple @Button@s
--   but is the only function that is able to insert @Mnemonic@s on the label.
--   Our solution is to use @StockItem@s to specify all @Images of the 
--   @Buttons. If the user inserts @RadioButton@s or @ToggleButton@s, the
--   stock image lookup is done manually. A mnemonic in the labels is sadly
--   not honored this way.
--
--- TODO ----------------------------------------------------------------------

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
  connectToOrientationChanged,
  connectToStyleChanged
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
import StockItems(stockLookup, siLabel, stockMissingImage) 
import Image	(imageNewFromStock)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Create a new, empty toolbar. (EXPORTED)
--
toolbarNew :: IO Toolbar
toolbarNew = makeNewObject mkToolbar $ liftM castPtr
  {#call unsafe toolbar_new#}

-- Make tooltips or not? 
--
mkToolText :: Maybe (String,String) -> (CString -> CString -> IO a) -> IO a
mkToolText Nothing               fun = fun nullPtr nullPtr
mkToolText (Just (text,private)) fun = withCString text $ \txtPtr -> 
  withCString private $ \prvPtr -> fun txtPtr prvPtr

-- Insert a new @Button into the @Toolbar. (EXPORTED)
--
-- * The new @Button is created at position @pos, counting from 0.
--
-- * The icon and label for the button is referenced by @stockId which must
--   be a valid entry in the @Toolbar@s Style or the default @IconFactory.
--
-- * If you whish to have @Tooltips added to this button you can specify
--   Just (tipText, tipPrivate), otherwise specify Nothing.
--
-- * The newly created @Button is returned. Use this button to add an action
--   function with @connectToClicked.
--
toolbarInsertNewButton :: ToolbarClass tb => 
  Int -> String -> Maybe (String,String) -> tb -> IO Button
toolbarInsertNewButton pos stockId tooltips tb = 
  withCString stockId $ \stockPtr ->
  mkToolText tooltips $ \textPtr privPtr ->
  makeNewObject mkButton $ liftM castPtr $ 
  {#call unsafe toolbar_insert_stock#} (toToolbar tb) stockPtr textPtr privPtr
    nullFunPtr nullPtr (fromIntegral pos)

-- Append a new @Button to the @Toolbar. (EXPORTED)
--
-- * see @toolbarInsertNewButton for details
--
toolbarAppendNewButton :: ToolbarClass tb => 
  String -> Maybe (String,String) -> tb -> IO Button
toolbarAppendNewButton = toolbarInsertNewButton (-1)

-- Prepend a new @Button to the @Toolbar. (EXPORTED)
--
-- * see @toolbarInsertNewButton for details
--
toolbarPrependNewButton :: ToolbarClass tb => 
  String -> Maybe (String,String) -> tb -> IO Button
toolbarPrependNewButton = toolbarInsertNewButton 0

-- Insert a new @ToggleButton into the @Toolbar. (EXPORTED)
--
-- * see @toolbarInsertNewButton for details
--
-- * Mnemonics in the label of the @StockItem are removed as they do not
--   work due to the bad interface definition of GtkToolbar.
--
toolbarInsertNewToggleButton :: ToolbarClass tb => 
  Int -> String -> Maybe (String,String) -> tb -> IO ToggleButton
toolbarInsertNewToggleButton pos stockId tooltips tb = do
  mItem <- stockLookup stockId
  item <- case mItem of
    (Just item) -> return item
    Nothing	-> liftM fromJust $ stockLookup stockMissingImage
  let label = (filter (/= '_')) $ siLabel item
  size <- toolbarGetSize' (toToolbar tb)
  image <- imageNewFromStock stockId size
  makeNewObject mkToggleButton $ liftM castPtr $
    withCString label $ \lblPtr -> mkToolText tooltips $ \textPtr privPtr ->
    {#call unsafe toolbar_insert_element#} (toToolbar tb) 
    toolbarChildToggleButton (mkWidget nullForeignPtr) lblPtr 
    textPtr privPtr (toWidget image) nullFunPtr nullPtr (fromIntegral pos)

-- Append a new @ToggleButton to the @Toolbar. (EXPORTED)
--
-- * see @toolbarInsertNewButton for details
--
-- * Mnemonics in the label of the @StockItem are removed as they do not
--   work due to the bad interface definition of GtkToolbar.
--
toolbarAppendNewToggleButton :: ToolbarClass tb => 
  String -> Maybe (String,String) -> tb -> IO ToggleButton
toolbarAppendNewToggleButton = toolbarInsertNewToggleButton (-1)

-- Prepend a new @ToggleButton to the @Toolbar. (EXPORTED)
--
-- * see @toolbarInsertNewButton for details
--
-- * Mnemonics in the label of the @StockItem are removed as they do not
--   work due to the bad interface definition of GtkToolbar.
--
toolbarPrependNewToggleButton :: ToolbarClass tb => 
  String -> Maybe (String,String) -> tb -> IO ToggleButton
toolbarPrependNewToggleButton = toolbarInsertNewToggleButton 0

-- Insert a new @RadioButton into the @Toolbar. (EXPORTED)
--
-- * see @toolbarInsertNewButton for details
--
-- * Mnemonics in the label of the @StockItem are removed as they do not
--   work due to the bad interface definition of GtkToolbar.
--
-- * The @parent argument must be set to another @RadioButton in the
--   group. If Nothing is given, a new group is generated (which is the
--   desired behavious for the first button of a group).
--
toolbarInsertNewRadioButton :: (ToolbarClass tb, RadioButtonClass rb) =>
  Int -> String -> Maybe (String,String) -> Maybe rb -> tb -> IO RadioButton
toolbarInsertNewRadioButton pos stockId tooltips rb tb = do
  mItem <- stockLookup stockId
  item <- case mItem of
    (Just item) -> return item
    Nothing	-> liftM fromJust $ stockLookup stockMissingImage
  let label = (filter (/= '_')) $ siLabel item
  size <- toolbarGetSize' (toToolbar tb)
  image <- imageNewFromStock stockId size
  makeNewObject mkRadioButton $ liftM castPtr $
    withCString label $ \lblPtr -> mkToolText tooltips $ \textPtr privPtr ->
    {#call unsafe toolbar_insert_element#} (toToolbar tb) 
    toolbarChildRadioButton (maybe (mkWidget nullForeignPtr) toWidget rb) lblPtr
    textPtr privPtr (toWidget image) nullFunPtr nullPtr (fromIntegral pos)

-- Append a new @RadioButton to the @Toolbar. (EXPORTED)
--
-- * see @toolbarInsertNewButton for details
--
-- * Mnemonics in the label of the @StockItem are removed as they do not
--   work due to the bad interface definition of GtkToolbar.
--
toolbarAppendNewRadioButton :: (ToolbarClass tb, RadioButtonClass rb) =>
  String -> Maybe (String,String) -> Maybe rb -> tb -> IO RadioButton
toolbarAppendNewRadioButton = toolbarInsertNewRadioButton (-1)

-- Prepend a new @RadioButton to the @Toolbar. (EXPORTED)
--
-- * see @toolbarInsertNewButton for details
--
-- * Mnemonics in the label of the @StockItem are removed as they do not
--   work due to the bad interface definition of GtkToolbar.
--
toolbarPrependNewRadioButton :: (ToolbarClass tb, RadioButtonClass rb) => 
  String -> Maybe (String,String) -> Maybe rb -> tb -> IO RadioButton
toolbarPrependNewRadioButton = toolbarInsertNewRadioButton 0


-- Insert an arbitrary widget to the @Toolbar. (EXPORTED)
--
-- * The @Widget should not be a button. Adding @Button@s with the
--   @toolbarInsertButton,... functions with stock objects is much
--   better as it takes care of theme handling.
--
toolbarInsertNewWidget :: (ToolbarClass tb, WidgetClass w) =>
  Int -> w -> Maybe (String,String) -> tb -> IO ()
toolbarInsertNewWidget pos w tooltips tb = 
  mkToolText tooltips $ \textPtr privPtr ->
  {#call unsafe toolbar_insert_widget#} (toToolbar tb) (toWidget w)
    textPtr privPtr (fromIntegral pos)

-- Append a new @Widget to the @Toolbar. (EXPORTED)
--
-- * see @toolbarInsertNewButton for details
--
-- * Mnemonics in the label of the @StockItem are removed as they do not
--   work due to the bad interface definition of GtkToolbar.
--
toolbarAppendNewWidget :: (ToolbarClass tb, WidgetClass w) =>
  w -> Maybe (String,String) -> tb -> IO ()
toolbarAppendNewWidget = toolbarInsertNewWidget (-1)

-- Prepend a new @Widget to the @Toolbar. (EXPORTED)
--
-- * see @toolbarInsertNewButton for details
--
-- * Mnemonics in the label of the @StockItem are removed as they do not
--   work due to the bad interface definition of GtkToolbar.
--
toolbarPrependNewWidget :: (ToolbarClass tb, WidgetClass w) =>
  w -> Maybe (String,String) -> tb -> IO ()
toolbarPrependNewWidget = toolbarInsertNewWidget 0


-- Set the direction of the @Toolbar. (EXPORTED)
--
toolbarSetOrientation :: ToolbarClass tb => Orientation -> tb -> IO ()
toolbarSetOrientation orientation tb = {#call toolbar_set_orientation#}
  (toToolbar tb) ((fromIntegral.fromEnum) orientation)

-- Specify how the buttons are dispayed. (EXPORTED)
--
toolbarSetStyle :: ToolbarClass tb => ToolbarStyle -> tb -> IO ()
toolbarSetStyle style tb = {#call toolbar_set_style#}
  (toToolbar tb) ((fromIntegral.fromEnum) style)

-- Enable or disable the @Tooltips. (EXPORTED)
--
toolbarSetTooltips :: ToolbarClass tb => Bool -> tb -> IO ()
toolbarSetTooltips enable tb = {#call unsafe toolbar_set_tooltips#}
  (toToolbar tb) (fromBool enable)

-- Set the size of the icons. (EXPORTED)
--
-- * It might be sensible to restrict oneself to IconSizeSmallToolbar and
--   IconSizeLargeToolbar.
--
toolbarSetIconSize :: ToolbarClass tb => IconSize -> tb -> IO ()
toolbarSetIconSize is tb = {#call toolbar_set_icon_size#} (toToolbar tb)
  (fromIntegral is)

-- Retrieve the current icon size that the @Toolbar shows. (EXPORTED)
--
toolbarGetIconSize :: ToolbarClass tb => tb -> IO IconSize
toolbarGetIconSize tb = toolbarGetSize' (toToolbar tb)

-- signals

-- Emitted when toolbarSetOrientation is called. (EXPORTED)
--
connectToOrientationChanged :: ToolbarClass tb => 
  (Orientation -> IO ()) -> ConnectAfter -> tb -> IO (ConnectId tb)
connectToOrientationChanged = connect_ENUM__NONE "orientation-changed"

-- Emitted when toolbarSetStyle is called. (EXPORTED)
--
connectToStyleChanged :: ToolbarClass tb =>
  (ToolbarStyle -> IO ()) -> ConnectAfter -> tb -> IO (ConnectId tb)
connectToStyleChanged = connect_ENUM__NONE "style-changed"

