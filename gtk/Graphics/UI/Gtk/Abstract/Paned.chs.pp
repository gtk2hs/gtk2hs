-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Paned
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
--
--  Version $Revision: 1.7 $ from $Date: 2005/04/02 19:02:22 $
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
-- Base class for widgets with two adjustable panes
--
module Graphics.UI.Gtk.Abstract.Paned (
-- * Detail
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
  panedGetPosition,
#if GTK_CHECK_VERSION(2,4,0)
  panedGetChild1,
  panedGetChild2,
#endif

-- * Properties
  panedPosition
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.Attributes		(Attr(..))
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Adds a child to the top or left pane with default parameters. This is
-- equivalent to @'panedPack1' paned child False True@.
--
panedAdd1 :: (PanedClass self, WidgetClass child) => self
 -> child -- ^ @child@ - the child to add
 -> IO ()
panedAdd1 self child =
  {# call paned_add1 #}
    (toPaned self)
    (toWidget child)

-- | Adds a child to the bottom or right pane with default parameters. This is
-- equivalent to @'panedPack2' paned child True True@.
--
panedAdd2 :: (PanedClass self, WidgetClass child) => self
 -> child -- ^ @child@ - the child to add
 -> IO ()
panedAdd2 self child =
  {# call paned_add2 #}
    (toPaned self)
    (toWidget child)

-- | Adds a child to the top or left pane.
--
panedPack1 :: (PanedClass self, WidgetClass child) => self
 -> child -- ^ @child@ - the child to add
 -> Bool  -- ^ @resize@ - should this child expand when the paned widget is
          -- resized.
 -> Bool  -- ^ @shrink@ - can this child be made smaller than its requsition.
 -> IO ()
panedPack1 self child resize shrink =
  {# call paned_pack1 #}
    (toPaned self)
    (toWidget child)
    (fromBool resize)
    (fromBool shrink)

-- | Adds a child to the bottom or right pane.
--
panedPack2 :: (PanedClass self, WidgetClass child) => self
 -> child -- ^ @child@ - the child to add
 -> Bool  -- ^ @resize@ - should this child expand when the paned widget is
          -- resized.
 -> Bool  -- ^ @shrink@ - can this child be made smaller than its requsition.
 -> IO ()
panedPack2 self child resize shrink =
  {# call paned_pack2 #}
    (toPaned self)
    (toWidget child)
    (fromBool resize)
    (fromBool shrink)

-- | Sets the position of the divider between the two panes.
--
panedSetPosition :: PanedClass self => self
 -> Int   -- ^ @position@ - pixel position of divider, a negative value means
          -- that the position is unset.
 -> IO ()
panedSetPosition self position =
  {# call paned_set_position #}
    (toPaned self)
    (fromIntegral position)

-- | Obtains the position of the divider between the two panes.
--
panedGetPosition :: PanedClass self => self
 -> IO Int -- ^ returns position of the divider
panedGetPosition self =
  liftM fromIntegral $
  {# call unsafe paned_get_position #}
    (toPaned self)

#if GTK_CHECK_VERSION(2,4,0)
-- | Obtains the first child of the paned widget.
--
-- * Available since Gtk version 2.4
--
panedGetChild1 :: PanedClass self => self
 -> IO Widget -- ^ returns first child
panedGetChild1 self =
  makeNewObject mkWidget $
  {# call unsafe paned_get_child1 #}
    (toPaned self)

-- | Obtains the second child of the paned widget.
--
-- * Available since Gtk version 2.4
--
panedGetChild2 :: PanedClass self => self
 -> IO Widget -- ^ returns second child
panedGetChild2 self =
  makeNewObject mkWidget $
  {# call unsafe paned_get_child2 #}
    (toPaned self)
#endif

--------------------
-- Properties

-- | Position of paned separator in pixels (0 means all the way to the
-- left\/top).
--
-- Allowed values: >= 0
--
-- Default value: 0
--
panedPosition :: PanedClass self => Attr self Int
panedPosition = Attr 
  panedGetPosition
  panedSetPosition
