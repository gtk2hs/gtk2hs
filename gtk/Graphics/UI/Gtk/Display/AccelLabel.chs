-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget AccelLabel
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:14:30 $
--
--  Copyright (c) 1999..2002 Axel Simon
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- |
--
-- This widget is a special version of 'Label'. It displays an 
-- accelerator key next to the Label.
--
-- * The key name is not explicitly set but taken from the key that
--   is associated with the activation of another widget.
--

module Graphics.UI.Gtk.Display.AccelLabel (
  AccelLabel,
  AccelLabelClass,
  castToAccelLabel,
  accelLabelNew,
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

-- methods

-- | Create a new label with an accelerator key.
--
accelLabelNew :: String -> IO AccelLabel
accelLabelNew str = withUTFString str $ \strPtr -> makeNewObject mkAccelLabel $ 
  liftM castPtr $ {#call unsafe accel_label_new#} strPtr

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
