-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Label@
--
--  Author : Manuel M. T. Chakravarty,
--	     Axel Simon
--          
--  Created: 2 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2002/08/05 16:41:34 $
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
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

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

-- @constructor labelNew@ create a new label widget
--
labelNew :: Maybe String -> IO Label
labelNew str = makeNewObject mkLabel $ liftM castPtr $
  case str of
    Nothing    -> {#call label_new#} nullPtr
    (Just str) -> withCString str {#call label_new#}

-- @method labelSetText@ set the text the label widget shows
--
labelSetText :: LabelClass l => l -> String -> IO ()
labelSetText l str =
  withCString str $ {#call label_set_text#} (toLabel l)

-- @method labelSetAttributes@ Set the text attributes.
--
-- labelSetAttributes :: LabelClass l => PangoAttrList -> IO ()


-- @method labelSetMarkup@ set the label to a markup string
--
labelSetMarkup :: LabelClass l => l -> Markup -> IO ()
labelSetMarkup l str =
  withCString str $ {#call label_set_markup#} (toLabel l)

-- @method labelSetMarkupWithMnemonic@ set the label to a markup string and
-- interpret keyboard accelerators
--
labelSetMarkupWithMnemonic :: LabelClass l => l -> Markup -> IO ()
labelSetMarkupWithMnemonic l str =
  withCString str $ {#call label_set_markup_with_mnemonic#} (toLabel l)

-- @method labelSetPattern@ underline parts of the text, odd indices of the
-- list represent underlined parts
--
labelSetPattern :: LabelClass l => l -> [Int] -> IO ()
labelSetPattern l list =
  withCString str $ {#call label_set_pattern#} (toLabel l)
  where
    str = concat $ zipWith replicate list (cycle ['_',' '])

-- @method labelSetJustify@ set the justification of the label
--
labelSetJustify :: LabelClass l => l -> Justification -> IO ()
labelSetJustify l j = 
  {#call label_set_justify#} (toLabel l) ((fromIntegral.fromEnum) j)

-- @method labelSetLineWrap@ set wether lines should be wrapped or truncated
--
labelSetLineWrap :: LabelClass l => l -> Bool -> IO ()
labelSetLineWrap l w = {#call label_set_line_wrap#} (toLabel l) (fromBool w)

-- @method labelGetLayoutOffsets@ get starting cooridinates of text rendering
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

-- @type KeyVal@ KeyVal is a synonym for a hot key number.
--
type KeyVal = {#type guint#}

-- @method labelGetMnemonicKeyval@ get the keyval for the underlined character
-- in the label
--
labelGetMnemonicKeyval :: LabelClass l => l -> IO KeyVal
labelGetMnemonicKeyval l = 
  {#call unsafe label_get_mnemonic_keyval#} (toLabel l)

-- @method labelGetSelectable@ is the text selectable?
--
labelGetSelectable :: LabelClass l => l -> IO Bool
labelGetSelectable l = liftM toBool $ 
  {#call unsafe label_get_selectable#} (toLabel l)

-- @method labelGetText@ get the text stored in the label
--
labelGetText :: LabelClass l => l -> IO String
labelGetText l = {#call unsafe label_get_text#} (toLabel l) >>= peekCString


-- @method labelNewWithMnemonic@ create a new label widget with accelerator
--
-- * Each underscore in @ref arg str@ is converted into an underlined
--   character in the label. Entering this character will activate the label
--   widget or any other widget set with labelSetMnemonicWidget.
--
labelNewWithMnemonic :: String -> IO Label
labelNewWithMnemonic str = makeNewObject mkLabel $ liftM castPtr $
  withCString str {#call label_new_with_mnemonic#}

-- @method labelSelectRegion@ select a region in label
--
labelSelectRegion :: LabelClass l => l -> Int -> Int -> IO ()
labelSelectRegion l start end = {#call label_select_region#} (toLabel l) 
  (fromIntegral start) (fromIntegral end)


-- @method labelSetMnemonicWidget@ set an explicit widget for which to emit
-- the mnemonic_activate signal if an underlined character is pressed
--
labelSetMnemonicWidget :: (LabelClass l, WidgetClass w) => l -> w -> IO ()
labelSetMnemonicWidget l w = 
  {#call unsafe label_set_mnemonic_widget#} (toLabel l) (toWidget w)

-- @method labelSetSelectable@ make a label text selectable
--
labelSetSelectable :: LabelClass l => l -> Bool -> IO ()
labelSetSelectable l s =
  {#call unsafe label_set_selectable#} (toLabel l) (fromBool s)

-- @method labelSetTextWithMnemonic@ set the label to a markup string and
-- interpret keyboard accelerators
--
labelSetTextWithMnemonic :: LabelClass l => l -> String -> IO ()
labelSetTextWithMnemonic l str =
  withCString str $ {#call label_set_text_with_mnemonic#} (toLabel l)

