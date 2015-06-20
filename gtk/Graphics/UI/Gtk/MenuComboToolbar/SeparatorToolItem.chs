{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SeparatorToolItem
--
--  Author : Duncan Coutts
--
--  Created: 7 April 2005
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
-- A toolbar item that separates groups of other toolbar items
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.MenuComboToolbar.SeparatorToolItem (
-- * Detail
--
-- | A 'SeparatorToolItem' is a 'ToolItem' that separates groups of other
-- 'ToolItem's. Depending on the theme, a 'SeparatorToolItem' will often look
-- like a vertical line on horizontally docked toolbars.
--
-- If the property \"expand\" is @True@ and the property \"draw\" is
-- @False@, a 'SeparatorToolItem' will act as a \"spring\" that forces other
-- items to the ends of the toolbar.
--
-- Use 'separatorToolItemNew' to create a new 'SeparatorToolItem'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'ToolItem'
-- |                                 +----SeparatorToolItem
-- @

#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  SeparatorToolItem,
  SeparatorToolItemClass,
  castToSeparatorToolItem, gTypeSeparatorToolItem,
  toSeparatorToolItem,

-- * Constructors
  separatorToolItemNew,

-- * Methods
  separatorToolItemSetDraw,
  separatorToolItemGetDraw,

-- * Attributes
  separatorToolItemDraw,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,4,0)
--------------------
-- Constructors

-- | Create a new 'SeparatorToolItem'
--
separatorToolItemNew :: IO SeparatorToolItem
separatorToolItemNew =
  makeNewObject mkSeparatorToolItem $
  liftM (castPtr :: Ptr ToolItem -> Ptr SeparatorToolItem) $
  {# call gtk_separator_tool_item_new #}

--------------------
-- Methods

-- | Whether the separator tool item is drawn as a vertical line, or just
-- blank. Setting this @False@ along with
-- 'Graphics.UI.Gtk.MenuComboToolbar.ToolItem.toolItemSetExpand' is useful to
-- create an item that forces following items to the end of the toolbar.
--
separatorToolItemSetDraw :: SeparatorToolItemClass self => self -> Bool -> IO ()
separatorToolItemSetDraw self draw =
  {# call gtk_separator_tool_item_set_draw #}
    (toSeparatorToolItem self)
    (fromBool draw)

-- | Returns whether the separator tool item is drawn as a line, or just blank.
-- See 'separatorToolItemSetDraw'.
--
separatorToolItemGetDraw :: SeparatorToolItemClass self => self -> IO Bool
separatorToolItemGetDraw self =
  liftM toBool $
  {# call gtk_separator_tool_item_get_draw #}
    (toSeparatorToolItem self)

--------------------
-- Attributes

-- | Whether the separator is drawn, or just blank.
--
-- Default value: @True@
--
separatorToolItemDraw :: SeparatorToolItemClass self => Attr self Bool
separatorToolItemDraw = newAttr
  separatorToolItemGetDraw
  separatorToolItemSetDraw
#endif
