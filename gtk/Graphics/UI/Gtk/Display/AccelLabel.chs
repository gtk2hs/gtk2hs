-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget AccelLabel
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/25 01:11:32 $
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
-- A label which displays an accelerator key on the right of the text.
--
module Graphics.UI.Gtk.Display.AccelLabel (
-- * Description
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
-- Note that creating a 'MenuItem' with 'menuItemNewWithLabel' (or one of
-- the similar functions for 'CheckMenuItem' and 'RadioMenuItem') automatically
-- adds a 'AccelLabel' to the 'MenuItem' and calls 'accelLabelSetAccelWidget'
-- to set it up for you.
--
-- A 'AccelLabel' will only display accelerators which have 'AccelVisible'
-- set (see 'AccelFlags'). A 'AccelLabel' can display multiple accelerators and
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
  castToAccelLabel,

-- * Constructors
  accelLabelNew,

-- * Methods
  accelLabelSetAccelWidget,
  accelLabelGetAccelWidget
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new label with an accelerator key.
--
accelLabelNew :: String -> IO AccelLabel
accelLabelNew str = withUTFString str $ \strPtr -> makeNewObject mkAccelLabel $ 
  liftM castPtr $ {#call unsafe accel_label_new#} strPtr

--------------------
-- Methods

-- | Set the key name from the activation
-- signal of another widget.
--
accelLabelSetAccelWidget :: (AccelLabelClass acl, WidgetClass w) => acl -> w ->
                            IO ()
accelLabelSetAccelWidget acl w = {#call accel_label_set_accel_widget#}
  (toAccelLabel acl) (toWidget w)

-- | Fetches the widget monitored by this accelerator label, or Nothing if it
-- has not bee set.
--
accelLabelGetAccelWidget :: AccelLabelClass acl => acl -> IO (Maybe Widget)
accelLabelGetAccelWidget acl = do
  wPtr <- {#call unsafe accel_label_get_accel_widget#} (toAccelLabel acl)
  if wPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkWidget (return wPtr)
