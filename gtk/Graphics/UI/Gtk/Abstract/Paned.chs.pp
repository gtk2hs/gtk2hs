-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Paned
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/25 01:11:31 $
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
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- This abstract widget provides a division line with a handle that can be
-- used by the user to divide the given space between two widgets. The two
-- concrete implementations are HPaned and VPaned.
--
module Graphics.UI.Gtk.Abstract.Paned (
-- * Description
-- 
-- | 'Paned' is the base class for widgets with two panes, arranged either
-- horizontally ('HPaned') or vertically ('VPaned'). Child widgets are added to
-- the panes of the widget with 'panedPack1' and 'panedPack2'. The division
-- beween the two children is set by default from the size requests of the
-- children, but it can be adjusted by the user.
--
-- A paned widget draws a separator between the two child widgets and a
-- small handle that the user can drag to adjust the division. It does not draw
-- any relief around the children or around the separator. (The space in which
-- the separator is called the gutter.) Often, it is useful to put each child
-- inside a 'Frame' with the shadow type set to 'ShadowIn' so that the gutter
-- appears as a ridge.
--
-- Each child has two options that can be set, @resize@ and @shrink@. If
-- @resize@ is true, then when the 'Paned' is resized, that child will expand
-- or shrink along with the paned widget. If @shrink@ is true, then when that
-- child can be made smaller than its requisition by the user. Setting @shrink@
-- to @False@ allows the application to set a minimum size. If @resize@ is
-- false for both children, then this is treated as if @resize@ is true for
-- both children.
--
-- The application can set the position of the slider as if it were set by
-- the user, by calling 'panedSetPosition'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----Paned
-- |                           +----'HPaned'
-- |                           +----'VPaned'
-- @

-- * Types
  Paned,
  PanedClass,
  castToPaned,

-- * Methods
  panedAdd1,
  panedAdd2,
  panedPack1,
  panedPack2,
  panedSetPosition,
  panedGetPosition
#if GTK_CHECK_VERSION(2,4,0)
 ,panedGetChild1,
  panedGetChild2
#endif
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Add a widget to the first (top or left) area.
--
-- * The widget does not expand if 'Paned' expands. It does not shrink either.
--
panedAdd1 :: (PanedClass p, WidgetClass w) => p -> w -> IO ()
panedAdd1 p w = {#call paned_add1#} (toPaned p) (toWidget w)

-- | Add a widget to the second (bottom or right) area.
--
-- * The widget does not expand if 'Paned' expands. But it does shrink.
--
panedAdd2 :: (PanedClass p, WidgetClass w) => p -> w -> IO ()
panedAdd2 p w = {#call paned_add2#} (toPaned p) (toWidget w)

-- | Add a widget to the first area and specify its resizing behaviour.
--
panedPack1 :: (PanedClass p, WidgetClass w) => p -> w -> Bool -> Bool -> IO ()
panedPack1 p w expand shrink = {#call paned_pack1#} 
  (toPaned p) (toWidget w) (fromBool expand) (fromBool shrink)

-- | Add a widget to the second area and specify its resizing behaviour.
--
panedPack2 :: (PanedClass p, WidgetClass w) => p -> w -> Bool -> Bool -> IO ()
panedPack2 p w expand shrink = {#call paned_pack2#} 
  (toPaned p) (toWidget w) (fromBool expand) (fromBool shrink)

-- | Set the gutter to the specified @position@ (in pixels).
--
panedSetPosition :: PanedClass p => p -> Int -> IO ()
panedSetPosition p position = 
  {#call paned_set_position#} (toPaned p) (fromIntegral position)

-- | Get the gutter position (in pixels).
--
panedGetPosition :: PanedClass p => p -> IO Int
panedGetPosition p = liftM fromIntegral $
  {#call unsafe paned_get_position#} (toPaned p)

#if GTK_CHECK_VERSION(2,4,0)
-- | Obtains the first child of the paned widget.
--
panedGetChild1 :: PanedClass p => p -> IO Widget
panedGetChild1 p =
  makeNewObject mkWidget $ {#call unsafe paned_get_child1#} (toPaned p)

-- | Obtains the second child of the paned widget.
--
panedGetChild2 :: PanedClass p => p -> IO Widget
panedGetChild2 p =
  makeNewObject mkWidget $ {#call unsafe paned_get_child2#} (toPaned p)
#endif
