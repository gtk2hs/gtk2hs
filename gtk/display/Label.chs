-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Label
--
--  Author : Manuel M. T. Chakravarty,
--	     Axel Simon
--          
--  Created: 2 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:19 $
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
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module Label(
  Label,
  LabelClass,
  castToLabel,
  labelNew,
  labelNewWithMnemonic,
  labelSetText,
  labelSetTextWithMnemonic,
  module Markup,
  labelSetMarkup,
  labelSetMarkupWithMnemonic,
  labelSetMnemonicWidget,
  KeyVal,
  labelGetMnemonicKeyval,
  labelGetText,
--  labelSetAttributes,
  labelSetPattern,
  Justification(..),
  labelSetJustify,
  labelSetLineWrap,
  labelSetSelectable,
  labelGetSelectable,
  labelSelectRegion,
  labelGetLayoutOffsets
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(Justification(..))
import Markup
{# context lib="gtk" prefix="gtk" #}


-- methods

-- create a new label widget (EXPORTED)
--
labelNew :: (Maybe String) -> IO Label
labelNew str = makeNewObject mkLabel $ liftM castPtr $
  case str of
    Nothing    -> {#call label_new#} nullPtr
    (Just str) -> withCString str {#call label_new#}

-- set the text the label widget shows (EXPORTED)
--
labelSetText :: LabelClass l => String -> l -> IO ()
labelSetText str l =
  withCString str $ {#call label_set_text#} (toLabel l)

-- set the text attribute (EXPORTED)
--
-- labelSetAttributes :: LabelClass l => PangoAttrList -> IO ()

-- set the label to a markup string (EXPORTED)
--
labelSetMarkup :: LabelClass l => Markup -> l -> IO ()
labelSetMarkup str l =
  withCString str $ {#call label_set_markup#} (toLabel l)

-- set the label to a markup string and interpret keyboard 
-- accelerators (EXPORTED)
--
labelSetMarkupWithMnemonic :: LabelClass l => Markup -> l -> IO ()
labelSetMarkupWithMnemonic str l =
  withCString str $ {#call label_set_markup_with_mnemonic#} (toLabel l)

-- underline parts of the text, odd indices of the list represent
-- underlined parts (EXPORTED)
labelSetPattern :: LabelClass l => [Int] -> l -> IO ()
labelSetPattern list l =
  withCString str $ {#call label_set_pattern#} (toLabel l)
  where
    str = concat $ zipWith replicate list (cycle ['_',' '])

-- set the justification of the label (EXPORTED)
--
labelSetJustify :: LabelClass l => Justification -> l -> IO ()
labelSetJustify j l = 
  {#call label_set_justify#} (toLabel l) ((fromIntegral.fromEnum) j)

-- set wether lines should be wrapped or truncated (EXPORTED)
--
labelSetLineWrap :: LabelClass l => Bool -> l -> IO ()
labelSetLineWrap w l = {#call label_set_line_wrap#} (toLabel l) (fromBool w)

-- get starting cooridinates of text rendering (EXPORTED)
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

-- KeyVal is a synonym for a hot key number. (EXPORTED)
type KeyVal = {#type guint#}

-- get the keyval for the underlined character in the label (EXPORTED)
--
labelGetMnemonicKeyval :: LabelClass l => l -> IO KeyVal
labelGetMnemonicKeyval l = 
  {#call unsafe label_get_mnemonic_keyval#} (toLabel l)

-- is the text selectable? (EXPORTED)
--
labelGetSelectable :: LabelClass l => l -> IO Bool
labelGetSelectable l = liftM toBool $ 
  {#call unsafe label_get_selectable#} (toLabel l)

-- get the text stored in the label (EXPORTED)
--
labelGetText :: LabelClass l => l -> IO String
labelGetText l = {#call unsafe label_get_text#} (toLabel l) >>= peekCString


-- create a new label widget with accelerator (EXPORTED)
--
-- * Each underscore in @str is converted into an underlined character
--   in the label. Entering this character will activate the label
--   widget or any other widget set with labelSetMnemonicWidget.
labelNewWithMnemonic :: String -> IO Label
labelNewWithMnemonic str = makeNewObject mkLabel $ liftM castPtr $
  withCString str {#call label_new_with_mnemonic#}

-- select a region in label (EXPORTED)
--
labelSelectRegion :: LabelClass l => Int -> Int -> l -> IO ()
labelSelectRegion start end l = {#call label_select_region#} (toLabel l) 
  (fromIntegral start) (fromIntegral end)


-- set an explicit widget for which to emit the mnemonic_activate signal
-- if an underlined character is pressed (EXPORTED)
--
labelSetMnemonicWidget :: (LabelClass l, WidgetClass w) => w -> l -> IO ()
labelSetMnemonicWidget w l = 
  {#call unsafe label_set_mnemonic_widget#} (toLabel l) (toWidget w)

-- make a label text selectable (EXPORTED)
--
labelSetSelectable :: LabelClass l => Bool -> l -> IO ()
labelSetSelectable s l =
  {#call unsafe label_set_selectable#} (toLabel l) (fromBool s)

-- set the label to a markup string and interpret keyboard 
-- accelerators (EXPORTED)
--
labelSetTextWithMnemonic :: LabelClass l => String -> l -> IO ()
labelSetTextWithMnemonic str l =
  withCString str $ {#call label_set_text_with_mnemonic#} (toLabel l)

