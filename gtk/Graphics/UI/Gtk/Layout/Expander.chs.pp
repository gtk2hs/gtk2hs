-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Expander
--
--  Author : Duncan Coutts
--
--  Created: 24 April 2004
--
--  Version $Revision: 1.4 $ from $Date: 2005/02/25 22:53:41 $
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
-- A container which can hide its child
--
module Graphics.UI.Gtk.Layout.Expander (
-- * Description
-- 
-- | A 'Expander' allows the user to hide or show its child by clicking on an
-- expander triangle similar to the triangles used in a 'TreeView'.
--
-- Normally you use an expander as you would use any other descendant of
-- 'Bin'; you create the child widget and use 'containerAdd' to add it to the
-- expander. When the expander is toggled, it will take care of showing and
-- hiding the child automatically.
--
-- * Module available since Gtk version 2.4

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----Expander
-- @

#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  Expander,
  ExpanderClass,
  castToExpander,

-- * Constructors
  expanderNew,
  expanderNewWithMnemonic,

-- * Methods
  expanderSetExpanded,
  expanderGetExpanded,
  expanderSetSpacing,
  expanderGetSpacing,
  expanderSetLabel,
  expanderGetLabel,
  expanderSetUseUnderline,
  expanderGetUseUnderline,
  expanderSetUseMarkup,
  expanderGetUseMarkup,
  expanderSetLabelWidget,
  expanderGetLabelWidget,

-- * Signals
  onActivate,
  afterActivate
#endif
  ) where

#if GTK_CHECK_VERSION(2,4,0)

import Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.Signals

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

expanderNew :: String -> IO Expander
expanderNew label = 
 makeNewObject mkExpander $ liftM castPtr $
 withUTFString label $ \strPtr ->
 {# call gtk_expander_new #} strPtr

expanderNewWithMnemonic :: String -> IO Expander
expanderNewWithMnemonic label =
 makeNewObject mkExpander $ liftM castPtr $
 withUTFString label $ \strPtr -> 
 {# call gtk_expander_new_with_mnemonic #} strPtr

--------------------
-- Methods

expanderSetExpanded :: Expander -> Bool -> IO ()
expanderSetExpanded expander expanded = 
 {# call gtk_expander_set_expanded #} expander (fromBool expanded)

expanderGetExpanded :: Expander -> IO Bool
expanderGetExpanded expander = liftM toBool $
 {# call gtk_expander_get_expanded #} expander

expanderSetSpacing :: Expander -> Int -> IO ()
expanderSetSpacing expander spacing = 
 {# call gtk_expander_set_spacing #} expander (fromIntegral spacing)

expanderGetSpacing :: Expander -> IO Int
expanderGetSpacing expander = liftM fromIntegral $
 {# call gtk_expander_get_spacing #} expander

expanderSetLabel :: Expander -> String -> IO ()
expanderSetLabel expander label =
 withUTFString label $ \strPtr ->
 {# call gtk_expander_set_label #} expander strPtr

expanderGetLabel :: Expander -> IO String
expanderGetLabel expander = do
 strPtr <- {# call gtk_expander_get_label #} expander
 peekUTFString strPtr

expanderSetUseUnderline :: Expander -> Bool -> IO ()
expanderSetUseUnderline expander useUnderline = 
 {# call gtk_expander_set_use_underline #} expander (fromBool useUnderline)

expanderGetUseUnderline :: Expander -> IO Bool
expanderGetUseUnderline expander = liftM toBool $
 {# call gtk_expander_get_use_underline #} expander

expanderSetUseMarkup :: Expander -> Bool -> IO ()
expanderSetUseMarkup expander useMarkup = 
 {# call gtk_expander_set_use_markup #} expander (fromBool useMarkup)

expanderGetUseMarkup :: Expander -> IO Bool
expanderGetUseMarkup expander = liftM toBool $
 {# call gtk_expander_get_use_markup #} expander

expanderSetLabelWidget :: WidgetClass widget => Expander -> widget -> IO ()
expanderSetLabelWidget expander widget = 
 {# call gtk_expander_set_label_widget #} expander (toWidget widget)

expanderGetLabelWidget :: Expander -> IO Widget
expanderGetLabelWidget expander = 
 makeNewObject mkWidget $
 {# call gtk_expander_get_label_widget #} expander

--------------------
-- Signals

onActivate :: Expander -> IO () -> IO (ConnectId Expander)
afterActivate :: Expander -> IO () -> IO (ConnectId Expander)
onActivate = connect_NONE__NONE "activate" False
afterActivate = connect_NONE__NONE "activate" True

#endif
