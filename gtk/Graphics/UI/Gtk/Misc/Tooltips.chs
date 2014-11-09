{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Tooltips
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
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
-- Add tips to your widgets
--
module Graphics.UI.Gtk.Misc.Tooltips (
-- * Detail
--
-- | Tooltips are the messages that appear next to a widget when the mouse
-- pointer is held over it for a short amount of time. They are especially
-- helpful for adding more verbose descriptions of things such as buttons in a
-- toolbar.
--
-- An individual tooltip belongs to a group of tooltips. A group is created
-- with a call to 'tooltipsNew'. Every tooltip in the group can then be turned
-- off with a call to 'tooltipsDisable' and enabled with 'tooltipsEnable'.
--
#ifndef DISABLE_DEPRECATED
-- The length of time the user must keep the mouse over a widget before the
-- tip is shown, can be altered with 'tooltipsSetDelay'. This is set on a \'per
-- group of tooltips\' basis.
--
#endif
-- To assign a tip to a particular 'Widget', 'tooltipsSetTip' is used.
--
-- To associate 'Tooltips' to a widget it is has to have its own 'DrawWindow'.
-- Otherwise the widget must be set into an 'EventBox'.
--
-- The default appearance of all tooltips in a program is determined by the
-- current Gtk+ theme that the user has selected.
--
-- Information about the tooltip (if any) associated with an arbitrary
-- widget can be retrieved using 'tooltipsDataGet'.
--
-- * This module is deprecated. It is empty in Gtk3.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----Tooltips
-- @
#if GTK_MAJOR_VERSION < 3
-- * Types
  Tooltips,
  TooltipsClass,
  castToTooltips, gTypeTooltips,
  toTooltips,

-- * Constructors
  tooltipsNew,

-- * Methods
  tooltipsEnable,
  tooltipsDisable,
#ifndef DISABLE_DEPRECATED
  tooltipsSetDelay,
#endif
  tooltipsSetTip,
  tooltipsDataGet
#endif
  ) where

#if GTK_MAJOR_VERSION < 3
import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new goup of 'Tooltips'.
--
tooltipsNew :: IO Tooltips
tooltipsNew =
  makeNewObject mkTooltips $
  {# call unsafe tooltips_new #}

--------------------
-- Methods

-- | Allows the user to see your tooltips as they navigate your application.
--
tooltipsEnable :: TooltipsClass self => self -> IO ()
tooltipsEnable self =
  {# call unsafe tooltips_enable #}
    (toTooltips self)

-- | Causes all tooltips in @tooltips@ to become inactive. Any widgets that
-- have tips associated with that group will no longer display their tips until
-- they are enabled again with 'tooltipsEnable'.
--
tooltipsDisable :: TooltipsClass self => self -> IO ()
tooltipsDisable self =
  {# call unsafe tooltips_disable #}
    (toTooltips self)

#ifndef DISABLE_DEPRECATED
-- | Sets the time between the user moving the mouse over a widget and the
-- widget's tooltip appearing.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
tooltipsSetDelay :: TooltipsClass self => self
 -> Int   -- ^ @delay@ - the delay in milliseconds
 -> IO ()
tooltipsSetDelay self delay =
  {# call unsafe tooltips_set_delay #}
    (toTooltips self)
    (fromIntegral delay)
#endif

-- | Adds a tooltip containing the message @tipText@ to the specified
-- 'Widget'.
--
tooltipsSetTip :: (TooltipsClass self, WidgetClass widget, GlibString string) => self
 -> widget -- ^ @widget@ - the 'Widget' you wish to associate the tip with.
 -> string -- ^ @tipText@ - a string containing the tip itself.
 -> string -- ^ @tipPrivate@ - a string of any further information that may be
           -- useful if the user gets stuck.
 -> IO ()
tooltipsSetTip self widget tipText tipPrivate =
  withUTFString tipPrivate $ \tipPrivatePtr ->
  withUTFString tipText $ \tipTextPtr ->
  {# call unsafe tooltips_set_tip #}
    (toTooltips self)
    (toWidget widget)
    tipTextPtr
    tipPrivatePtr

-- | Retrieves any 'Tooltips' previously associated with the given widget.
--
tooltipsDataGet :: (WidgetClass w, GlibString string) => w -> IO (Maybe (Tooltips, string, string))
tooltipsDataGet w = do
  tipDataPtr <- {#call unsafe tooltips_data_get#} (toWidget w)
  if tipDataPtr == nullPtr
    then return Nothing
    else do --next line is a hack, tooltips struct member is at offset 0
           tooltips <- makeNewObject mkTooltips (return $ castPtr tipDataPtr)
           tipText  <- {#get TooltipsData->tip_text#} tipDataPtr
                   >>= peekUTFString
           tipPrivate <- {#get TooltipsData->tip_private#} tipDataPtr
                     >>= peekUTFString
           return $ Just $ (tooltips, tipText, tipPrivate)
#endif
