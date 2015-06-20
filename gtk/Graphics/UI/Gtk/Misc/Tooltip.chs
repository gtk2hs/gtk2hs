{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Tooltip
--
--  Author : Andy Stewart
--
--  Created: 24 Mar 2010
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
-- Add tips to your widgets
--
-- * Module available since Gtk+ version 2.12
--
module Graphics.UI.Gtk.Misc.Tooltip (

-- * Detail
--
-- | 'Tooltip' belongs to the new tooltips API that was introduced in Gtk+
-- 2.12 and which deprecates the old 'Tooltips' API.
--
-- Basic tooltips can be realized simply by using 'widgetTooltipText' or
-- 'widgetTooltipMarkup' without any explicit tooltip object.
--
-- When you need a tooltip with a little more fancy contents, like adding an
-- image, or you want the tooltip to have different contents per 'TreeView' row
-- or cell, you will have to do a little more work:
--
--   * Set the 'hasTooltip' property to 'True', this will make GTK+ monitor the widget for motion and
--     related events which are needed to determine when and where to show a tooltip.
--
--   * Connect to the 'queryTooltip' signal. This signal will be emitted when a tooltip is supposed to
--     be shown. One of the arguments passed to the signal handler is a 'Tooltip' object. This is the
--     object that we are about to display as a tooltip, and can be manipulated in your callback using
--     functions like 'tooltipSetIcon'. There are functions for setting the tooltip's markup,
--     setting an image from a stock icon, or even putting in a custom widget.
--
--   * Return 'True' from your query-tooltip handler. This causes the tooltip to be show. If you return
--    'False', it will not be shown.
--
-- In the probably rare case where you want to have even more control over the tooltip that is about to
-- be shown, you can set your own 'Window' which will be used as tooltip window. This works as
-- follows:
--
--   * Set 'hasTooltip' and connect to 'queryTooltip' as before.
--
--   * Use 'widgetSetTooltipWindow' to set a 'Window' created by you as tooltip window.
--
--   * In the 'queryTooltip' callback you can access your window using 'widgetGetTooltipWindow'
--     and manipulate as you wish. The semantics of the return value are exactly as before, return 'True'
--     to show the window, 'False' to not show it.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----Tooltip
-- @

#if GTK_CHECK_VERSION(2,12,0)
-- * Types
  Tooltip,
  TooltipClass,
  castToTooltip,
  toTooltip,

-- * Methods
  tooltipSetMarkup,
  tooltipSetText,
  tooltipSetIcon,
  tooltipSetIconFromStock,
#if GTK_CHECK_VERSION(2,14,0)
  tooltipSetIconFromIconName,
#endif
  tooltipSetCustom,
  tooltipTriggerTooltipQuery,
  tooltipSetTipArea,
#ifdef HAVE_GIO
#if GTK_CHECK_VERSION(2,20,0)
  tooltipSetIconFromGIcon,
#endif
#endif
#endif
  ) where

#if GTK_CHECK_VERSION(2,12,0)

import Data.Maybe (fromMaybe)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.General.Structs  (IconSize(..), Rectangle)
{#import Graphics.UI.Gtk.Types#}
#ifdef HAVE_GIO
{#import System.GIO.Types#}
#endif

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Sets the text of the tooltip to be @markup@, which is marked up with the
-- Pango text markup language. If @markup@ is 'Nothing', the label will be hidden.
--
tooltipSetMarkup :: (TooltipClass self, GlibString markup) => self
 -> Maybe markup -- ^ @markup@ - a markup string (see Pango markup format) or 'Nothing'
 -> IO ()
tooltipSetMarkup self markup =
  maybeWith withUTFString markup $ \markupPtr ->
  {# call gtk_tooltip_set_markup #}
    (toTooltip self)
    markupPtr

-- | Sets the text of the tooltip to be @text@. If @text@ is 'Nothing'
-- the label will be hidden. See also 'tooltipSetMarkup'.
--
tooltipSetText :: (TooltipClass self, GlibString string) => self
 -> Maybe string -- ^ @text@ - a text string or 'Nothing'
 -> IO ()
tooltipSetText self text =
  maybeWith withUTFString text $ \textPtr ->
  {# call gtk_tooltip_set_text #}
    (toTooltip self)
    textPtr

-- | Sets the icon of the tooltip (which is in front of the text) to be
-- @pixbuf@. If @pixbuf@ is 'Nothing' the image will be hidden.
--
tooltipSetIcon :: TooltipClass self => self
  -> Maybe Pixbuf  -- ^ @pixbuf@ - a 'Pixbuf' or 'Nothing'
  -> IO ()
tooltipSetIcon self pixbuf =
  {#call tooltip_set_icon#}
    (toTooltip self)
    (fromMaybe (Pixbuf nullForeignPtr) pixbuf)

-- | Sets the icon of the tooltip (which is in front of the text) to be the
-- stock item indicated by @stockId@ with the size indicated by @size@. If
-- @stockId@ is 'Nothing' the image will be hidden.
--
tooltipSetIconFromStock :: (TooltipClass self, GlibString string) => self
  -> Maybe string -- ^ @id@ a stock id, or 'Nothing'
  -> IconSize -- ^ @size@ a stock icon size
  -> IO ()
tooltipSetIconFromStock self id size =
  maybeWith withUTFString id $ \ idPtr ->
  {#call tooltip_set_icon_from_stock#}
    (toTooltip self)
    idPtr
    ((fromIntegral . fromEnum) size)

#if GTK_CHECK_VERSION(2,14,0)
-- | Sets the icon of the tooltip (which is in front of the text) to be the
-- icon indicated by @iconName@ with the size indicated by @size@. If
-- @iconName@ is 'Nothing' the image will be hidden.
--
-- * Available since Gtk+ version 2.14
--
tooltipSetIconFromIconName :: (TooltipClass self, GlibString string) => self
  -> Maybe string -- ^ @iconName@ an icon name, or 'Nothing'
  -> IconSize  -- ^ @size@ a stock icon size
  -> IO ()
tooltipSetIconFromIconName self iconName size =
  maybeWith withUTFString iconName $ \ iconPtr ->
  {#call tooltip_set_icon_from_icon_name#}
    (toTooltip self)
    iconPtr
    ((fromIntegral . fromEnum) size)
#endif

-- | Replaces the widget packed into the tooltip with @customWidget@.
-- @customWidget@ does not get destroyed when the tooltip goes away. By default
-- a box with a 'Image' and 'Label' is embedded in the tooltip, which can be
-- configured using 'tooltipSetMarkup' and 'tooltipSetIcon'.
--
tooltipSetCustom :: (TooltipClass self, WidgetClass widget) => self
  -> Maybe widget  -- ^ @customWidget@ a 'Widget', or 'Nothing' to unset the old custom widget.
  -> IO ()
tooltipSetCustom self customWidget =
  {#call tooltip_set_custom#}
    (toTooltip self)
    (maybe (Widget nullForeignPtr) toWidget customWidget)

-- | Triggers a new tooltip query on @display@, in order to update the current
-- visible tooltip, or to show\/hide the current tooltip. This function is
-- useful to call when, for example, the state of the widget changed by a key
-- press.
--
tooltipTriggerTooltipQuery ::
    Display -- ^ @display@ - a 'Display'
 -> IO ()
tooltipTriggerTooltipQuery display =
  {# call gtk_tooltip_trigger_tooltip_query #}
    display

-- | Sets the area of the widget, where the contents of this tooltip apply, to
-- be @rect@ (in widget coordinates). This is especially useful for properly
-- setting tooltips on 'TreeView' rows and cells, 'IconView'
--
-- For setting tooltips on 'TreeView', please refer to the convenience
-- functions for this: 'treeViewSetTooltipRow' and 'treeViewSetTooltipCell'.
--
tooltipSetTipArea :: TooltipClass self => self -> Rectangle -> IO ()
tooltipSetTipArea self rect =
  with rect $ \ rectPtr ->
  {#call tooltip_set_tip_area#}
    (toTooltip self)
    (castPtr rectPtr)
#endif

#ifdef HAVE_GIO
#if GTK_CHECK_VERSION(2,20,0)
-- | Sets the icon of the tooltip (which is in front of the text) to be the icon indicated by gicon with
-- the size indicated by size. If gicon is 'Nothing', the image will be hidden.
tooltipSetIconFromGIcon :: TooltipClass self => self
                        -> Maybe Icon  -- ^ @gicon@   a GIcon representing the icon, or 'Nothing'. allow-none.
                        -> IconSize
                        -> IO ()
tooltipSetIconFromGIcon tooltip icon size =
  {#call gtk_tooltip_set_icon_from_gicon #}
    (toTooltip tooltip)
    (fromMaybe (Icon nullForeignPtr) icon)
    ((fromIntegral . fromEnum) size)
#endif
#endif
