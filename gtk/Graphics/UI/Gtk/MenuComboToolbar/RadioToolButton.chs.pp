-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget RadioToolButton
--
--  Author : Duncan Coutts
--
--  Created: 7 April 2005
--
--  Version $Revision: 1.1 $ from $Date: 2005/04/12 19:52:15 $
--
--  Copyright (C) 2005 Duncan Coutts
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
-- A toolbar item that contains a radio button
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.MenuComboToolbar.RadioToolButton (
-- * Detail
-- 
-- | A 'RadioToolButton' is a 'ToolItem' that contains a radio button, that
-- is, a button that is part of a group of toggle buttons where only one button
-- can be active at a time.
--
-- Use 'radioToolButtonNew' to create a new 'RadioToolButton'. use
-- 'radioToolButtonNewFromWidget' to create a new 'RadioToolButton' that is
-- part of the same group as an existing 'RadioToolButton'. Use
-- 'radioToolButtonNewFromStock' or 'radioToolButtonNewFromWidgetWithStock' to
-- create a new 'RadioToolButton' containing a stock item.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'ToolItem'
-- |                                 +----'ToolButton'
-- |                                       +----'ToggleToolButton'
-- |                                             +----RadioToolButton
-- @

#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  RadioToolButton,
  RadioToolButtonClass,
  castToRadioToolButton,

-- * Constructors
  radioToolButtonNew,
  radioToolButtonNewFromStock,
  radioToolButtonNewFromWidget,
  radioToolButtonNewWithStockFromWidget,

-- * Methods
  radioToolButtonGetGroup,
  radioToolButtonSetGroup,

-- * Properties
  radioToolButtonGroup,
#endif
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.Attributes		(Attr(..))
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,4,0)
--------------------
-- Constructors

-- | Creates a new 'RadioToolButton', adding it to @group@.
--
radioToolButtonNew :: 
    [RadioToolButton]  -- ^ @group@ - An existing radio button group, or @[]@
                       -- if you are creating a new group
 -> IO RadioToolButton
radioToolButtonNew group =
  withForeignPtrs (map unRadioToolButton group) $ \radioToolButtonFPtrs ->
  toGSList radioToolButtonFPtrs >>= \groupGSList ->
  makeNewObject mkRadioToolButton $
  liftM (castPtr :: Ptr ToolItem -> Ptr RadioToolButton) $
  {# call gtk_radio_tool_button_new #}
    groupGSList

-- | Creates a new 'RadioToolButton', adding it to @group@. The new
-- 'RadioToolButton' will contain an icon and label from the stock item
-- indicated by @stockId@.
--
radioToolButtonNewFromStock :: 
    [RadioToolButton]  -- ^ @group@ - an existing radio button group, or @[]@ 
                       -- if you are creating a new group
 -> String             -- ^ @stockId@ - the name of a stock item
 -> IO RadioToolButton
radioToolButtonNewFromStock group stockId =
  withForeignPtrs (map unRadioToolButton group) $ \radioToolButtonFPtrs ->
  toGSList radioToolButtonFPtrs >>= \groupGSList ->
  makeNewObject mkRadioToolButton $
  liftM (castPtr :: Ptr ToolItem -> Ptr RadioToolButton) $
  withUTFString stockId $ \stockIdPtr ->
  {# call gtk_radio_tool_button_new_from_stock #}
    groupGSList
    stockIdPtr

-- | Creates a new 'RadioToolButton' adding it to the same group as @gruup@
--
radioToolButtonNewFromWidget :: RadioToolButtonClass group => 
    group              -- ^ @group@ - An existing 'RadioToolButton'
 -> IO RadioToolButton
radioToolButtonNewFromWidget group =
  makeNewObject mkRadioToolButton $
  liftM (castPtr :: Ptr ToolItem -> Ptr RadioToolButton) $
  {# call gtk_radio_tool_button_new_from_widget #}
    (toRadioToolButton group)

-- | Creates a new 'RadioToolButton' adding it to the same group as @group@.
-- The new 'RadioToolButton' will contain an icon and label from the stock item
-- indicated by @stockId@.
--
radioToolButtonNewWithStockFromWidget :: RadioToolButtonClass group => 
    group              -- ^ @group@ - An existing 'RadioToolButton'.
 -> String             -- ^ @stockId@ - the name of a stock item
 -> IO RadioToolButton
radioToolButtonNewWithStockFromWidget group stockId =
  makeNewObject mkRadioToolButton $
  liftM (castPtr :: Ptr ToolItem -> Ptr RadioToolButton) $
  withUTFString stockId $ \stockIdPtr ->
  {# call gtk_radio_tool_button_new_with_stock_from_widget #}
    (toRadioToolButton group)
    stockIdPtr

--------------------
-- Methods

-- | Returns the radio button group @button@ belongs to.
--
radioToolButtonGetGroup :: RadioToolButtonClass self => self
 -> IO [RadioToolButton] -- ^ returns the group the button belongs to.
radioToolButtonGetGroup self =
  {# call gtk_radio_tool_button_get_group #}
    (toRadioToolButton self)
  >>= readGSList
  >>= mapM (\elemPtr -> makeNewObject mkRadioToolButton (return elemPtr))

-- | Adds @button@ to @group@, removing it from the group it belonged to
-- before.
--
radioToolButtonSetGroup :: RadioToolButtonClass self => self
 -> [RadioToolButton] -- ^ @group@ - an existing radio button group
 -> IO ()
radioToolButtonSetGroup self group =
  withForeignPtrs (map unRadioToolButton group) $ \radioToolButtonFPtrs ->
  toGSList radioToolButtonFPtrs >>= \groupGSList ->
  {# call gtk_radio_tool_button_set_group #}
    (toRadioToolButton self)
    groupGSList

--------------------
-- Properties

-- | Sets a new group for a radio tool button.
--
radioToolButtonGroup :: RadioToolButtonClass self => Attr self [RadioToolButton]
radioToolButtonGroup = Attr 
  radioToolButtonGetGroup
  radioToolButtonSetGroup
#endif
