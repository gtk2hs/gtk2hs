{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ToolPalette
--
--  Author : Andy Stewart
--
--  Created: 08 Sep 2010
--
--  Copyright (C) 2010 Andy Stewart
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
-- A tool palette with categories
--
-- * Module available since Gtk+ version 2.20
--
-- TODO:
--
--       gtk_tool_palette_add_drag_dest
--       gtk_tool_palette_get_drag_item
--       gtk_tool_palette_get_drag_target_group
--       gtk_tool_palette_get_drag_target_item
--       gtk_tool_palette_get_drop_group
--       gtk_tool_palette_get_drop_item
--
module Graphics.UI.Gtk.MenuComboToolbar.ToolPalette (

-- * Detail
-- | A 'ToolPalette' allows you to add 'ToolItems' to a palette-like container with different
-- categories and drag and drop support.
--
-- A 'ToolPalette' is created with a call to 'toolPaletteNew'.
--
-- 'ToolItems' cannot be added directly to a 'ToolPalette' - instead they are added to a
-- 'ToolItemGroup' which can than be added to a 'ToolPalette'. To add a 'ToolItemGroup' to a
-- 'ToolPalette', use 'containerAdd'.
--
-- The easiest way to use drag and drop with 'ToolPalette' is to call 'toolPaletteAddDragDest'
-- with the desired drag source palette and the desired drag target widget. Then
-- 'toolPaletteGetDragItem' can be used to get the dragged item in the 'dragDataReceived'
-- signal handler of the drag target.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'ToolPalette'
-- @

#if GTK_CHECK_VERSION(2,20,0)
-- * Types
  ToolPalette,
  ToolPaletteClass,
  castToToolPalette,
  toToolPalette,

-- * Enums
  ToolPaletteDragTargets (..),

-- * Constructors
  toolPaletteNew,

-- * Methods
  toolPaletteUnsetIconSize,
  toolPaletteUnsetStyle,
  toolPaletteGetHAdjustment,
  toolPaletteGetVAdjustment,

-- * Attributes
  toolPaletteSetGroupPosition,
  toolPaletteGetGroupPosition,
  toolPaletteIconSize,
  toolPaletteIconSizeSet,
  toolPaletteToolbarStyle,

-- * Child Attributes
  toolPaletteChildExclusive,
  toolPaletteChildExpand,

-- * Signals
  toolPaletteSetScrollAdjustments,
#endif
) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.General.Structs (IconSize (..))
import Graphics.UI.Gtk.General.Enums (ToolbarStyle (..))
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,20,0)

-- | Flags used to specify the supported drag targets.
{# enum ToolPaletteDragTargets {underscoreToCase} deriving (Eq) #}

-- | Creates a new tool palette.
--
-- * Available since Gtk+ version 2.20
--
toolPaletteNew :: IO ToolPalette
toolPaletteNew =
  makeNewObject mkToolPalette $
  liftM (castPtr :: Ptr Widget -> Ptr ToolPalette) $
  {#call gtk_tool_palette_new #}

-- | Unsets the tool palette icon size set with 'toolPaletteSetIconSize', so that user
-- preferences will be used to determine the icon size.
--
-- * Available since Gtk+ version 2.20
--
toolPaletteUnsetIconSize :: ToolPaletteClass self => self -> IO ()
toolPaletteUnsetIconSize palette =
  {#call gtk_tool_palette_unset_icon_size #}
    (toToolPalette palette)

-- | Unsets a toolbar style set with 'toolPaletteSetStyle', so that user preferences will be used
-- to determine the toolbar style.
--
-- * Available since Gtk+ version 2.20
--
toolPaletteUnsetStyle :: ToolPaletteClass self => self -> IO ()
toolPaletteUnsetStyle palette =
  {#call gtk_tool_palette_unset_style #}
     (toToolPalette palette)

-- | Gets the horizontal adjustment of the tool palette.
--
-- * Available since Gtk+ version 2.20
--
toolPaletteGetHAdjustment :: ToolPaletteClass self => self
                          -> IO Adjustment
toolPaletteGetHAdjustment palette =
  makeNewObject mkAdjustment $
  {# call gtk_tool_palette_get_hadjustment #}
     (toToolPalette palette)

-- | Gets the vertical adjustment of the tool palette.
--
-- * Available since Gtk+ version 2.20
--
toolPaletteGetVAdjustment :: ToolPaletteClass self => self
                          -> IO Adjustment
toolPaletteGetVAdjustment palette =
  makeNewObject mkAdjustment $
  {# call gtk_tool_palette_get_vadjustment #}
     (toToolPalette palette)

-- | Gets the position of group in palette as index. See 'toolPaletteSetGroupPosition'.
--
-- * Available since Gtk+ version 2.20
--
toolPaletteGetGroupPosition :: (ToolPaletteClass palette, ToolItemGroupClass group)
                              => palette
                              -> group
                              -> IO Int -- ^ returns the index of group or -1 if group is not a child of palette
toolPaletteGetGroupPosition palette group =
  liftM fromIntegral $
  {#call gtk_tool_palette_get_group_position #}
    (toToolPalette palette)
    (toToolItemGroup group)

-- | Sets the position of the group as an index of the tool palette. If position is 0 the group will
-- become the first child, if position is -1 it will become the last child.
--
-- * Available since Gtk+ version 2.20
--
toolPaletteSetGroupPosition :: (ToolPaletteClass palette, ToolItemGroupClass group)
                              => palette
                              -> group
                              -> Int
                              -> IO ()
toolPaletteSetGroupPosition palette group position =
  {#call gtk_tool_palette_set_group_position #}
     (toToolPalette palette)
     (toToolItemGroup group)
     (fromIntegral position)

-- | The size of the icons in a tool palette is normally determined by the 'toolbarIconSize'
-- setting. When this property is set, it overrides the setting.
--
-- This should only be used for special-purpose tool palettes, normal application tool palettes should
-- respect the user preferences for the size of icons.
--
-- Default value: 'IconSizeSmallToolbar'
--
-- * Available since Gtk+ version 2.20
--
toolPaletteIconSize :: ToolPaletteClass self => Attr self IconSize
toolPaletteIconSize =
    newAttrFromEnumProperty "icon-size"
        {# call pure unsafe gtk_icon_size_get_type #}

-- | Is 'True' if the 'iconSize' property has been set.
--
-- Default value: 'False'
--
-- * Available since Gtk+ version 2.20
--
toolPaletteIconSizeSet :: ToolPaletteClass self => Attr self Bool
toolPaletteIconSizeSet =
  newAttrFromBoolProperty "icon-size-set"

-- | The style of items in the tool palette.
--
-- Default value: 'ToolbarIcons'
--
-- * Available since Gtk+ version 2.20
--
toolPaletteToolbarStyle :: ToolPaletteClass self => Attr self ToolbarStyle
toolPaletteToolbarStyle =
  newAttrFromEnumProperty "toolbar-style"
    {# call pure unsafe gtk_toolbar_style_get_type #}

-- | Whether the item group should be the only one that is expanded at a given time.
--
-- Default value: 'False'
--
-- * Available since Gtk+ version 2.20
--
toolPaletteChildExclusive :: ToolPaletteClass self => Attr self Bool
toolPaletteChildExclusive =
  newAttrFromBoolProperty "exclusive"

-- | Whether the item group should receive extra space when the palette grows. at a given time.
--
-- Default value: 'False'
--
-- * Available since Gtk+ version 2.20
--
toolPaletteChildExpand :: ToolPaletteClass self => Attr self Bool
toolPaletteChildExpand =
  newAttrFromBoolProperty "expand"

-- | Set the scroll adjustments for the viewport. Usually scrolled containers like 'ScrolledWindow' will
-- emit this signal to connect two instances of 'Scrollbar' to the scroll directions of the
-- 'Toolpalette'.
--
-- * Available since Gtk+ version 2.20
--
toolPaletteSetScrollAdjustments :: ToolPaletteClass self => Signal self (Adjustment -> Adjustment -> IO ())
toolPaletteSetScrollAdjustments = Signal (connect_OBJECT_OBJECT__NONE "set-scroll-adjustments")
#endif
