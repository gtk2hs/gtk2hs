-- -*-haskell-*-
--  GIMP Toolkit (GTK) entry Widget ComboBoxEntry
--
--  Author : Duncan Coutts
--  Created: 25 April 2004
--
--  Copyright (c) 2004 Duncan Coutts
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
-- |
--
-- A text entry field with a dropdown list
--
-- * Added in gtk 2.4
--
module ComboBoxEntry (
#if GTK_CHECK_VERSION(2,4,0)
  ComboBoxEntryClass,
  ComboBoxEntry,
  comboBoxEntryNew,
  comboBoxEntryNewWithModel,
  comboBoxEntryNewText,
  comboBoxEntrySetTextColumn,
  comboBoxEntryGetTextColumn,
#endif
) where

#if GTK_CHECK_VERSION(2,4,0)
import Monad	(liftM)
import FFI

import Object	(makeNewObject)
import GObject (makeNewGObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix ="gtk" #}


comboBoxEntryNew :: IO ComboBoxEntry
comboBoxEntryNew =
  makeNewObject mkComboBoxEntry $ liftM castPtr $
  {# call gtk_combo_box_entry_new #}

comboBoxEntryNewWithModel :: TreeModel -> Int -> IO ComboBoxEntry
comboBoxEntryNewWithModel model textColumn =
  makeNewObject mkComboBoxEntry $ liftM castPtr $
  {# call gtk_combo_box_entry_new_with_model #} model (fromIntegral textColumn)

comboBoxEntryNewText :: IO ComboBoxEntry
comboBoxEntryNewText =
  makeNewObject mkComboBoxEntry $ liftM castPtr $
  {# call gtk_combo_box_entry_new_text #}

comboBoxEntrySetTextColumn :: ComboBoxEntryClass combo => combo -> Int -> IO ()
comboBoxEntrySetTextColumn combo textColumn =
  {# call gtk_combo_box_entry_set_text_column #} (toComboBoxEntry combo)
    (fromIntegral textColumn)

comboBoxEntryGetTextColumn :: ComboBoxEntryClass combo => combo -> IO Int
comboBoxEntryGetTextColumn combo =
  liftM fromIntegral $ 
  {# call gtk_combo_box_entry_get_text_column #} (toComboBoxEntry combo)

#endif
