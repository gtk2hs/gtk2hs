-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ComboBoxEntry
--
--  Author : Duncan Coutts
--
--  Created: 25 April 2004
--
--  Version $Revision: 1.5 $ from $Date: 2005/03/13 19:34:35 $
--
--  Copyright (C) 2004-2005 Duncan Coutts
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
-- A text entry field with a dropdown list
--
-- * Added in gtk 2.4
--
module Graphics.UI.Gtk.MenuComboToolbar.ComboBoxEntry (
-- * Description
-- 
-- | * Module available since Gtk version 2.4

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'ComboBox'
-- |                                 +----ComboBoxEntry
-- @

#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  ComboBoxEntry,
  ComboBoxEntryClass,
  castToComboBoxEntry,

-- * Constructors
  comboBoxEntryNew,
  comboBoxEntryNewWithModel,
  comboBoxEntryNewText,

-- * Methods
  comboBoxEntrySetTextColumn,
  comboBoxEntryGetTextColumn,

-- * Properties
  comboBoxEntryTextColumn
#endif
  ) where

#if GTK_CHECK_VERSION(2,4,0)
import Monad	(liftM)

import System.Glib.FFI
import System.Glib.Attributes		(Attr(..))
import System.Glib.GObject		(makeNewGObject)
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

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

--------------------
-- Methods

comboBoxEntrySetTextColumn :: ComboBoxEntryClass combo => combo -> Int -> IO ()
comboBoxEntrySetTextColumn combo textColumn =
  {# call gtk_combo_box_entry_set_text_column #} (toComboBoxEntry combo)
    (fromIntegral textColumn)

comboBoxEntryGetTextColumn :: ComboBoxEntryClass combo => combo -> IO Int
comboBoxEntryGetTextColumn combo =
  liftM fromIntegral $ 
  {# call gtk_combo_box_entry_get_text_column #} (toComboBoxEntry combo)

--------------------
-- Properties

-- | A column in the data source model to get the strings from.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
comboBoxEntryTextColumn :: Attr ComboBoxEntry Int
comboBoxEntryTextColumn = Attr 
  comboBoxEntryGetTextColumn
  comboBoxEntrySetTextColumn
#endif
