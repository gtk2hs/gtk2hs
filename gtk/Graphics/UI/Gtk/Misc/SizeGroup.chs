{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SizeGroup
--
--  Author : Duncan Coutts
--
--  Created: 2 August 2004
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
-- Grouping widgets so they request the same size
--
module Graphics.UI.Gtk.Misc.SizeGroup (
-- * Detail
--
-- | 'SizeGroup' provides a mechanism for grouping a number of widgets
-- together so they all request the same amount of space. This is typically
-- useful when you want a column of widgets to have the same size, but you
-- can't use a 'Table' widget.
--
-- In detail, the size requested for each widget in a 'SizeGroup' is the
-- maximum of the sizes that would have been requested for each widget in the
-- size group if they were not in the size group. The mode of the size group
-- (see 'sizeGroupSetMode') determines whether this applies to the horizontal
-- size, the vertical size, or both sizes.
--
-- Note that size groups only affect the amount of space requested, not the
-- size that the widgets finally receive. If you want the widgets in a
-- 'SizeGroup' to actually be the same size, you need to pack them in such a
-- way that they get the size they request and not more. For example, if you
-- are packing your widgets into a table, you would not include the
-- 'Graphics.UI.Gtk.Layout.Table.Fill' flag.
--
-- Widgets can be part of multiple size groups; Gtk+ will compute the
-- horizontal size of a widget from the horizontal requisition of all widgets
-- that can be reached from the widget by a chain of size groups of type
-- 'SizeGroupHorizontal' or 'SizeGroupBoth', and the vertical size from the
-- vertical requisition of all widgets that can be reached from the widget by a
-- chain of size groups of type 'SizeGroupVertical' or 'SizeGroupBoth'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----SizeGroup
-- @

-- * Types
  SizeGroup,
  SizeGroupClass,
  castToSizeGroup, gTypeSizeGroup,
  toSizeGroup,

-- * Constructors
  sizeGroupNew,

-- * Methods
  SizeGroupMode(..),
  sizeGroupSetMode,
  sizeGroupGetMode,
  sizeGroupAddWidget,
  sizeGroupRemoveWidget,
#if GTK_CHECK_VERSION(2,8,0)
  sizeGroupSetIgnoreHidden,
  sizeGroupGetIgnoreHidden,
#endif

-- * Attributes
  sizeGroupMode,
#if GTK_CHECK_VERSION(2,8,0)
  sizeGroupIgnoreHidden,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

{# enum SizeGroupMode {underscoreToCase} #}

--------------------
-- Constructors

-- | Create a new 'SizeGroup'.
--
sizeGroupNew ::
    SizeGroupMode -- ^ @mode@ - the mode for the new size group.
 -> IO SizeGroup
sizeGroupNew mode =
  wrapNewGObject mkSizeGroup $
  {# call unsafe size_group_new #}
    ((fromIntegral . fromEnum) mode)

--------------------
-- Methods

-- | Adds a widget to a 'SizeGroup'. In the future, the requisition of the
-- widget will be determined as the maximum of its requisition and the
-- requisition of the other widgets in the size group. Whether this applies
-- horizontally, vertically, or in both directions depends on the mode of the
-- size group. See 'sizeGroupSetMode'.
--
sizeGroupAddWidget :: (SizeGroupClass self, WidgetClass widget) => self
 -> widget -- ^ @widget@ - the 'Widget' to add
 -> IO ()
sizeGroupAddWidget self widget =
  {# call size_group_add_widget #}
    (toSizeGroup self)
    (toWidget widget)

-- | Gets the current mode of the size group. See 'sizeGroupSetMode'.
--
sizeGroupGetMode :: SizeGroupClass self => self
 -> IO SizeGroupMode -- ^ returns the current mode of the size group.
sizeGroupGetMode self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe size_group_get_mode #}
    (toSizeGroup self)

-- | Removes a widget from a 'SizeGroup'.
--
sizeGroupRemoveWidget :: (SizeGroupClass self, WidgetClass widget) => self
 -> widget -- ^ @widget@ - the 'Widget' to remove
 -> IO ()
sizeGroupRemoveWidget self widget =
  {# call size_group_remove_widget #}
    (toSizeGroup self)
    (toWidget widget)

-- | Sets the 'SizeGroupMode' of the size group. The mode of the size group
-- determines whether the widgets in the size group should all have the same
-- horizontal requisition 'SizeGroupHorizontal' all have the same vertical
-- requisition 'SizeGroupVertical', or should all have the same requisition
-- in both directions 'SizeGroupBoth'.
--
sizeGroupSetMode :: SizeGroupClass self => self
 -> SizeGroupMode -- ^ @mode@ - the mode to set for the size group.
 -> IO ()
sizeGroupSetMode self mode =
  {# call size_group_set_mode #}
    (toSizeGroup self)
    ((fromIntegral . fromEnum) mode)

#if GTK_CHECK_VERSION(2,8,0)
-- | Sets whether invisible widgets should be ignored when calculating the
-- size.
--
-- * Available since Gtk+ version 2.8
--
sizeGroupSetIgnoreHidden :: SizeGroupClass self => self
 -> Bool  -- ^ @ignoreHidden@ - whether hidden widgets should be ignored when
          -- calculating the size
 -> IO ()
sizeGroupSetIgnoreHidden self ignoreHidden =
  {# call gtk_size_group_set_ignore_hidden #}
    (toSizeGroup self)
    (fromBool ignoreHidden)

-- | Returns if invisible widgets are ignored when calculating the size.
--
-- * Available since Gtk+ version 2.8
--
sizeGroupGetIgnoreHidden :: SizeGroupClass self => self
 -> IO Bool -- ^ returns @True@ if invisible widgets are ignored.
sizeGroupGetIgnoreHidden self =
  liftM toBool $
  {# call gtk_size_group_get_ignore_hidden #}
    (toSizeGroup self)
#endif

--------------------
-- Attributes

-- | The directions in which the size group affects the requested sizes of its
-- component widgets.
--
-- Default value: 'SizeGroupHorizontal'
--
sizeGroupMode :: SizeGroupClass self => Attr self SizeGroupMode
sizeGroupMode = newAttr
  sizeGroupGetMode
  sizeGroupSetMode

#if GTK_CHECK_VERSION(2,8,0)
-- | If @True@, hidden widgets are ignored when determining the size of the
-- group.
--
-- Default value: @False@
--
sizeGroupIgnoreHidden :: SizeGroupClass self => Attr self Bool
sizeGroupIgnoreHidden = newAttr
  sizeGroupGetIgnoreHidden
  sizeGroupSetIgnoreHidden
#endif
