-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Combo
--
--  Author : Axel Simon
--
--  Created: 2 June 2001
--
--  Version $Revision: 1.4 $ from $Date: 2005/02/25 22:53:41 $
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
-- TODO
--
-- The combo_set_item_string function is not bound as we do not handle
--   arbitrary widgets yet.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A Combo is a text entry field with a dropdown list.
--
module Graphics.UI.Gtk.MenuComboToolbar.Combo (
-- * Description
-- 
-- | The 'Combo' widget consists of a single-line text entry field and a
-- drop-down list. The drop-down list is displayed when the user clicks on a
-- small arrow button to the right of the entry field.
--
-- List elements
-- can contain arbitrary widgets, but if an element is not a plain label, then
-- you must use the 'listSetItemString' function. This sets the string which
-- will be placed in the text entry field when the item is selected.
--
-- By default, the user can step through the items in the list using the
-- arrow (cursor) keys, though this behaviour can be turned off with
-- 'comboSetUseArrows'.
--
-- As of Gtk+ 2.4, 'Combo' has been deprecated in favor of 'ComboBox'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Box'
-- |                           +----'HBox'
-- |                                 +----Combo
-- @
#ifndef DISABLE_DEPRECATED
-- * Types
  Combo,
  ComboClass,
  castToCombo,
  comboNew,

-- * Methods
  comboSetPopdownStrings,
  comboSetValueInList,
  comboSetUseArrows,
  comboSetUseArrowsAlways,
  comboSetCaseSensitive,
  comboDisableActivate
#endif
  ) where
#ifndef DISABLE_DEPRECATED

import Monad	(liftM, mapM_)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object		(makeNewObject)
import Graphics.UI.Gtk.Abstract.Widget		(widgetShow)
import Graphics.UI.Gtk.Abstract.Container	(containerAdd)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Structs		(comboGetList)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- Create a new Combo text entry field.
--
comboNew :: IO Combo
comboNew = makeNewObject mkCombo $ liftM castPtr $ {#call unsafe combo_new#}

-- | Insert a set of Strings into the
-- 'Combo' drop down list.
--
comboSetPopdownStrings :: ComboClass c => c -> [String] -> IO ()
comboSetPopdownStrings c strs = do
  list <- comboGetList (toCombo c)
  {#call list_clear_items#} list  0 (-1)
  mapM_ (\str -> do
    li <- makeNewObject mkWidget $ liftM castPtr $ 
      withUTFString str {#call unsafe list_item_new_with_label#}
    widgetShow li
    containerAdd list li)
    strs

-- | Specify whether the user may enter texts that
-- are not in the list of alternatives and if empty entries are allowed.
--
comboSetValueInList :: ComboClass c => c -> Bool -> Bool -> IO ()
comboSetValueInList c val okIfEmpty = {#call unsafe combo_set_value_in_list#}
  (toCombo c) (fromBool val) (fromBool okIfEmpty)

-- | Specify if the user may use the cursor keys to
-- navigate the list.
--
comboSetUseArrows :: ComboClass c => c -> Bool -> IO ()
comboSetUseArrows c val = {#call unsafe combo_set_use_arrows#} (toCombo c)
  (fromBool val)

-- | Specify if the content entered by the user
-- will be replaced by a predefined alternative as soon as the user uses the
-- cursor keys.
--
comboSetUseArrowsAlways :: ComboClass c => c -> Bool -> IO ()
comboSetUseArrowsAlways c val = {#call unsafe combo_set_use_arrows_always#}
  (toCombo c) (fromBool val)

-- | Specify whether the entered text is case
-- sensitive when it comes to matching the users input with the predefined
-- alternatives.
--
comboSetCaseSensitive :: ComboClass c => c -> Bool -> IO ()
comboSetCaseSensitive c val = {#call unsafe combo_set_case_sensitive#}
  (toCombo c) (fromBool val)

-- | Stops the GtkCombo widget from showing the
-- popup list when the Entry emits the \"activate\" signal, i.e. when the Return
-- key is pressed. This may be useful if, for example, if you want the Return
-- key to close a dialog instead.
--
comboDisableActivate :: ComboClass c => c -> IO ()
comboDisableActivate  = {#call unsafe combo_disable_activate#}.toCombo

#endif
