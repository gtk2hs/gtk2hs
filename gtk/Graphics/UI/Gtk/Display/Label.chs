-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Label
--
--  Author : Manuel M. T. Chakravarty,
--	     Axel Simon
--          
--  Created: 2 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:14:30 $
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
-- | A label is a piece of static text in a user interface.
--
module Graphics.UI.Gtk.Display.Label (
  Label,
  LabelClass,
  castToLabel,
  labelNew,
  labelNewWithMnemonic,
  labelSetText,
  labelSetLabel,
  labelSetTextWithMnemonic,
  labelSetMarkup,
  labelSetMarkupWithMnemonic,
  labelSetMnemonicWidget,
  labelGetMnemonicWidget,
  KeyVal,
  labelGetMnemonicKeyval,
  labelSetUseMarkup,
  labelGetUseMarkup,
  labelSetUseUnderline,
  labelGetUseUnderline,
  labelGetText,
  labelGetLabel,
--  labelSetAttributes,
  labelSetPattern,
  Justification(..),
  labelSetJustify,
  labelGetJustify,
  labelGetLayout,
  labelSetLineWrap,
  labelGetLineWrap,
  labelSetSelectable,
  labelGetSelectable,
  labelSelectRegion,
  labelGetSelectionBounds,
  labelGetLayoutOffsets
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject   (makeNewGObject)
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums	(Justification(..))
import Graphics.UI.Gtk.Pango.Markup
{# context lib="gtk" prefix="gtk" #}


-- methods

-- | Create a new label widget.
--
labelNew :: Maybe String -> IO Label
labelNew str = makeNewObject mkLabel $ liftM castPtr $
  case str of
    Nothing    -> {#call label_new#} nullPtr
    (Just str) -> withUTFString str {#call label_new#}

-- | Set the text the label widget shows. 
--
labelSetText :: LabelClass l => l -> String -> IO ()
labelSetText l str =
  withUTFString str $ {#call label_set_text#} (toLabel l)

-- | The label is interpreted as including embedded underlines and\/or Pango
-- markup depending on the markup and underline properties.
--
labelSetLabel :: LabelClass l => l -> String -> IO ()
labelSetLabel l str =
  withUTFString str $ {#call label_set_label#} (toLabel l)

{-
-- | Set the text attributes.
--
-- labelSetAttributes :: LabelClass l => PangoAttrList -> IO ()
-}

-- | Set the label to a markup string.
--
labelSetMarkup :: LabelClass l => l -> Markup -> IO ()
labelSetMarkup l str =
  withUTFString str $ {#call label_set_markup#} (toLabel l)

-- | Set the label to a markup string and interpret keyboard accelerators.
--
labelSetMarkupWithMnemonic :: LabelClass l => l -> Markup -> IO ()
labelSetMarkupWithMnemonic l str =
  withUTFString str $ {#call label_set_markup_with_mnemonic#} (toLabel l)

-- | Underline parts of the text, odd indices of the list represent underlined
-- parts.
--
labelSetPattern :: LabelClass l => l -> [Int] -> IO ()
labelSetPattern l list =
  withUTFString str $ {#call label_set_pattern#} (toLabel l)
  where
    str = concat $ zipWith replicate list (cycle ['_',' '])

-- | Set the justification of the label.
--
labelSetJustify :: LabelClass l => l -> Justification -> IO ()
labelSetJustify l j = 
  {#call label_set_justify#} (toLabel l) ((fromIntegral.fromEnum) j)

-- | Get the justification of the label.
--
labelGetJustify :: LabelClass l => l -> IO Justification
labelGetJustify l =
  liftM (toEnum.fromIntegral) $ {#call unsafe label_get_justify#} (toLabel l)

-- | Gets the "PangoLayout" used to display the label.
--
labelGetLayout :: LabelClass l => l -> IO PangoLayout
labelGetLayout l =
  makeNewGObject mkPangoLayout $ {#call unsafe label_get_layout#} (toLabel l)

-- | Set wether lines should be wrapped (@True@) or truncated (@False@).
--
labelSetLineWrap :: LabelClass l => l -> Bool -> IO ()
labelSetLineWrap l w = {#call label_set_line_wrap#} (toLabel l) (fromBool w)

-- | Returns whether lines in the label are automatically wrapped.
--
labelGetLineWrap :: LabelClass l => l -> IO Bool
labelGetLineWrap l = liftM toBool $
  {#call unsafe label_get_line_wrap#} (toLabel l)

-- | Get starting cooridinates of text rendering.
--
labelGetLayoutOffsets :: LabelClass l => l -> IO (Int,Int)
labelGetLayoutOffsets l =
  alloca (\xPtr ->
    alloca (\yPtr -> do
      {#call unsafe label_get_layout_offsets#} (toLabel l) xPtr yPtr
      x <- peek xPtr
      y <- peek yPtr
      return (fromIntegral x,fromIntegral y)
    )
  )

-- | KeyVal is a synonym for a hot key number.
--
type KeyVal = {#type guint#}

-- | Get the keyval for the underlined character in the label.
--
labelGetMnemonicKeyval :: LabelClass l => l -> IO KeyVal
labelGetMnemonicKeyval l = 
  {#call unsafe label_get_mnemonic_keyval#} (toLabel l)

-- | Get whether the text selectable.
--
labelGetSelectable :: LabelClass l => l -> IO Bool
labelGetSelectable l = liftM toBool $ 
  {#call unsafe label_get_selectable#} (toLabel l)

-- | Sets whether the text of the label contains markup in Pango's text markup
-- language.
--
labelSetUseMarkup :: LabelClass l => l -> Bool -> IO ()
labelSetUseMarkup l useMarkup =
  {#call label_set_use_markup#} (toLabel l) (fromBool useMarkup)

-- | Returns whether the label's text is interpreted as marked up with the
-- Pango text markup language.
--
labelGetUseMarkup :: LabelClass l => l -> IO Bool
labelGetUseMarkup l = liftM toBool $
  {#call unsafe label_get_use_markup#} (toLabel l)

-- | If @True@, an underline in the text indicates the next character should
-- be used for the mnemonic accelerator key.
--
labelSetUseUnderline :: LabelClass l => l -> Bool -> IO ()
labelSetUseUnderline l useUnderline =
  {#call label_set_use_underline#} (toLabel l) (fromBool useUnderline)

-- | Returns whether an embedded underline in the label indicates a mnemonic.
--
labelGetUseUnderline :: LabelClass l => l -> IO Bool
labelGetUseUnderline l = liftM toBool $
  {#call unsafe label_get_use_underline#} (toLabel l)

-- | Get the text stored in the label. This does not include any embedded
-- underlines indicating mnemonics or Pango markup.
--
labelGetText :: LabelClass l => l -> IO String
labelGetText l = {#call unsafe label_get_text#} (toLabel l) >>= peekUTFString

-- | Get the text from a label widget including any embedded underlines
-- indicating mnemonics and Pango markup.
--
labelGetLabel :: LabelClass l => l -> IO String
labelGetLabel l = {#call unsafe label_get_label#} (toLabel l) >>= peekUTFString

-- | Create a new label widget with accelerator key.
--
-- * Each underscore in @str@ is converted into an underlined character in the
--   label. Entering this character will activate the label widget or any other
--   widget set with 'labelSetMnemonicWidget'.
--
labelNewWithMnemonic :: String -> IO Label
labelNewWithMnemonic str = makeNewObject mkLabel $ liftM castPtr $
  withUTFString str {#call label_new_with_mnemonic#}

-- | Select a region in the label.
--
labelSelectRegion :: LabelClass l => l -> Int -> Int -> IO ()
labelSelectRegion l start end = {#call label_select_region#} (toLabel l) 
  (fromIntegral start) (fromIntegral end)

-- | Gets the selected range of characters in the label, if any. If there is
-- a range selected the result is the start and end of the selection as
-- character offsets.
--
labelGetSelectionBounds :: LabelClass l => l -> IO (Maybe (Int, Int))
labelGetSelectionBounds l =
  alloca $ \startPtr -> alloca $ \endPtr -> do
  isSelection <-
    {#call unsafe label_get_selection_bounds#} (toLabel l) startPtr endPtr
  if toBool isSelection
    then do start <- peek startPtr
            end <- peek endPtr
	    return $ Just $ (fromIntegral start, fromIntegral end)
    else return Nothing

-- | Set an explicit widget for which to emit the \"mnemonic_activate\" signal
-- if an underlined character is pressed.
--
labelSetMnemonicWidget :: (LabelClass l, WidgetClass w) => l -> w -> IO ()
labelSetMnemonicWidget l w = 
  {#call unsafe label_set_mnemonic_widget#} (toLabel l) (toWidget w)

-- | Retrieves the target of the mnemonic (keyboard shortcut) of this label,
-- or Nothing if none has been set and the default algorithm will be used.
--
labelGetMnemonicWidget :: LabelClass l => l -> IO (Maybe Widget)
labelGetMnemonicWidget l = do
  widgetPtr <- {#call unsafe label_get_mnemonic_widget#} (toLabel l)
  if widgetPtr == nullPtr
    then return Nothing
    else liftM Just $ makeNewObject mkWidget (return widgetPtr)

-- | Make a label text selectable.
--
labelSetSelectable :: LabelClass l => l -> Bool -> IO ()
labelSetSelectable l s =
  {#call unsafe label_set_selectable#} (toLabel l) (fromBool s)

-- | Set the label to a markup string and interpret keyboard accelerators.
--
labelSetTextWithMnemonic :: LabelClass l => l -> String -> IO ()
labelSetTextWithMnemonic l str =
  withUTFString str $ {#call label_set_text_with_mnemonic#} (toLabel l)

