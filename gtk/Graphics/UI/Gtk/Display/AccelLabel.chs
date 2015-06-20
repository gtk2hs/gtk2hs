{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget AccelLabel
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
-- A label which displays an accelerator key on the right of the text
--
module Graphics.UI.Gtk.Display.AccelLabel (
-- * Detail
--
-- | The 'AccelLabel' widget is a subclass of 'Label' that also displays an
-- accelerator key on the right of the label text, e.g. \'Ctl+S\'. It is
-- commonly used in menus to show the keyboard short-cuts for commands.
--
-- The accelerator key to display is not set explicitly. Instead, the
-- 'AccelLabel' displays the accelerators which have been added to a particular
-- widget. This widget is set by calling 'accelLabelSetAccelWidget'.
--
-- For example, a 'MenuItem' widget may have an accelerator added to emit
-- the \"activate\" signal when the \'Ctl+S\' key combination is pressed. A
-- 'AccelLabel' is created and added to the 'MenuItem', and
-- 'accelLabelSetAccelWidget' is called with the 'MenuItem' as the second
-- argument. The 'AccelLabel' will now display \'Ctl+S\' after its label.
--
-- Note that creating a 'MenuItem' with
-- 'Graphics.UI.Gtk.MenuComboToolbar.MenuItem.menuItemNewWithLabel' (or one of
-- the similar functions for 'CheckMenuItem' and 'RadioMenuItem') automatically
-- adds a 'AccelLabel' to the 'MenuItem' and calls 'accelLabelSetAccelWidget'
-- to set it up for you.
--
-- An 'AccelLabel' will only display accelerators which have
-- 'Graphics.UI.Gtk.Abstract.Widget.AccelVisible'
-- set (see 'Graphics.UI.Gtk.Abstract.Widget.AccelFlags').
-- A 'AccelLabel' can display multiple accelerators and
-- even signal names, though it is almost always used to display just one
-- accelerator key.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Misc'
-- |                     +----'Label'
-- |                           +----AccelLabel
-- @

-- * Types
  AccelLabel,
  AccelLabelClass,
  castToAccelLabel, gTypeAccelLabel,
  toAccelLabel,

-- * Constructors
  accelLabelNew,

-- * Methods
  accelLabelSetAccelWidget,
  accelLabelGetAccelWidget,

-- * Attributes
  accelLabelAccelWidget,
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'AccelLabel'.
--
accelLabelNew :: GlibString string
 => string        -- ^ @string@ - the label string.
 -> IO AccelLabel
accelLabelNew string =
  makeNewObject mkAccelLabel $
  liftM (castPtr :: Ptr Widget -> Ptr AccelLabel) $
  withUTFString string $ \stringPtr ->
  {# call unsafe accel_label_new #}
    stringPtr

--------------------
-- Methods

-- | Sets the widget to be monitored by this accelerator label.
--
accelLabelSetAccelWidget :: (AccelLabelClass self, WidgetClass accelWidget) => self
 -> accelWidget -- ^ @accelWidget@ - the widget to be monitored.
 -> IO ()
accelLabelSetAccelWidget self accelWidget =
  {# call accel_label_set_accel_widget #}
    (toAccelLabel self)
    (toWidget accelWidget)

-- | Fetches the widget monitored by this accelerator label. See
-- 'accelLabelSetAccelWidget'.
--
accelLabelGetAccelWidget :: AccelLabelClass self => self
 -> IO (Maybe Widget) -- ^ returns the object monitored by the accelerator
                      -- label, or @Nothing@.
accelLabelGetAccelWidget self =
  maybeNull (makeNewObject mkWidget) $
  {# call unsafe accel_label_get_accel_widget #}
    (toAccelLabel self)

--------------------
-- Attributes

-- | The widget to be monitored for accelerator changes.
--
accelLabelAccelWidget :: (AccelLabelClass self, WidgetClass accelWidget) => ReadWriteAttr self (Maybe Widget) accelWidget
accelLabelAccelWidget = newAttr
  accelLabelGetAccelWidget
  accelLabelSetAccelWidget
