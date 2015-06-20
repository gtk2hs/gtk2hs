{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Combo
--
--  Author : Axel Simon
--
--  Created: 2 June 2001
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
-- A text entry field with a dropdown list
--
-- * Warning: this module is deprecated and should not be used in
-- newly-written code.
--
-- This module is empty in Gtk3 as Combo has been removed.
module Graphics.UI.Gtk.MenuComboToolbar.Combo (
-- * Detail
--
-- | The 'Combo' widget consists of a single-line text entry field and a
-- drop-down list. The drop-down list is displayed when the user clicks on a
-- small arrow button to the right of the entry field.
--
-- List elements
-- can contain arbitrary widgets, but if an element is not a plain label, then
-- you must use the 'comboSetItemString' function. This sets the string which
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

#if GTK_MAJOR_VERSION < 3
#ifndef DISABLE_DEPRECATED
-- * Types
  Combo,
  ComboClass,
  castToCombo, gTypeCombo,
  toCombo,

-- * Constructors
  comboNew,

-- * Methods
  comboSetPopdownStrings,
  comboSetValueInList,
  comboSetUseArrows,
  comboSetUseArrowsAlways,
  comboSetCaseSensitive,
  comboDisableActivate,

-- * Attributes
  comboEnableArrowKeys,
  comboEnableArrowsAlways,
  comboCaseSensitive,
  comboAllowEmpty,
  comboValueInList,
#endif
#endif
  ) where

#if GTK_MAJOR_VERSION < 3
import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object          (makeNewObject)
import Graphics.UI.Gtk.Abstract.Widget          (widgetShow)
import Graphics.UI.Gtk.Abstract.Container       (containerAdd)
{#import Graphics.UI.Gtk.Types#}

#ifndef DISABLE_DEPRECATED
import Graphics.UI.Gtk.General.Structs          (comboGetList)
#endif

{# context lib="gtk" prefix="gtk" #}

#ifndef DISABLE_DEPRECATED
--------------------
-- Constructors

-- Create a new Combo text entry field.
--
comboNew :: IO Combo
comboNew =
  makeNewObject mkCombo $
  liftM castPtr $
  {# call unsafe combo_new #}

--------------------
-- Methods

-- | Insert a set of Strings into the
-- 'Combo' drop down list.
--
comboSetPopdownStrings :: (ComboClass self, GlibString string) => self -> [string] -> IO ()
comboSetPopdownStrings self strs = do
  list <- comboGetList (toCombo self)
  {#call list_clear_items#} list  0 (-1)
  mapM_ (\str -> do
    li <- makeNewObject mkWidget $ liftM castPtr $
      withUTFString str {#call unsafe list_item_new_with_label#}
    widgetShow li
    containerAdd list li)
    strs

-- | Specifies whether the value entered in the text entry field must match
-- one of the values in the list. If this is set then the user will not be able
-- to perform any other action until a valid value has been entered.
--
-- If an empty field is acceptable, the @okIfEmpty@ parameter should be
-- @True@.
--
comboSetValueInList :: ComboClass self => self
 -> Bool  -- ^ @val@ - @True@ if the value entered must match one of the
          -- values in the list.
 -> Bool  -- ^ @okIfEmpty@ - @True@ if an empty value is considered valid.
 -> IO ()
comboSetValueInList self val okIfEmpty =
  {# call unsafe combo_set_value_in_list #}
    (toCombo self)
    (fromBool val)
    (fromBool okIfEmpty)

-- | Specifies if the arrow (cursor) keys can be used to step through the
-- items in the list. This is on by default.
--
comboSetUseArrows :: ComboClass self => self -> Bool -> IO ()
comboSetUseArrows self val =
  {# call unsafe combo_set_use_arrows #}
    (toCombo self)
    (fromBool val)

-- | Obsolete function, does nothing.
--
comboSetUseArrowsAlways :: ComboClass self => self -> Bool -> IO ()
comboSetUseArrowsAlways self val =
  {# call unsafe combo_set_use_arrows_always #}
    (toCombo self)
    (fromBool val)

-- | Specifies whether the text entered into the 'Entry' field and the text in
-- the list items is case sensitive.
--
-- This may be useful, for example, when you have called
-- 'comboSetValueInList' to limit the values entered, but you are not worried
-- about differences in case.
--
comboSetCaseSensitive :: ComboClass self => self -> Bool -> IO ()
comboSetCaseSensitive self val =
  {# call unsafe combo_set_case_sensitive #}
    (toCombo self)
    (fromBool val)

-- | Stops the 'Combo' widget from showing the popup list when the 'Entry'
-- emits the \"activate\" signal, i.e. when the Return key is pressed. This may
-- be useful if, for example, you want the Return key to close a dialog
-- instead.
--
comboDisableActivate :: ComboClass self => self -> IO ()
comboDisableActivate self =
  {# call unsafe combo_disable_activate #}
    (toCombo self)

--------------------
-- Attributes

-- | Whether the arrow keys move through the list of items.
--
-- Default value: @True@
--
comboEnableArrowKeys :: ComboClass self => Attr self Bool
comboEnableArrowKeys = newAttrFromBoolProperty "enable-arrow-keys"

-- | Obsolete property, ignored.
--
-- Default value: @True@
--
comboEnableArrowsAlways :: ComboClass self => Attr self Bool
comboEnableArrowsAlways = newAttrFromBoolProperty "enable-arrows-always"

-- | Whether list item matching is case sensitive.
--
-- Default value: @False@
--
comboCaseSensitive :: ComboClass self => Attr self Bool
comboCaseSensitive = newAttrFromBoolProperty "case-sensitive"

-- | Whether an empty value may be entered in this field.
--
-- Default value: @True@
--
comboAllowEmpty :: ComboClass self => Attr self Bool
comboAllowEmpty = newAttrFromBoolProperty "allow-empty"

-- | Whether entered values must already be present in the list.
--
-- Default value: @False@
--
comboValueInList :: ComboClass self => Attr self Bool
comboValueInList = newAttrFromBoolProperty "value-in-list"
#endif
#endif
