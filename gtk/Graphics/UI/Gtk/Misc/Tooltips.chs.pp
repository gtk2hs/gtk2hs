-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Tooltips
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:27:48 $
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
-- Tooltips are the messages that appear next to a widget when the mouse
-- pointer is held over it for a short amount of time. They are especially
-- helpful for adding more verbose descriptions of things such as buttons
-- in a toolbar. 
--
-- An individual tooltip belongs to a group of tooltips. A group is created
-- with a call to 'tooltipsNew'. Every tooltip in the group can 
-- then be turned off with a call to 'tooltipsDisable' and enabled with 
-- 'tooltipsEnable'.
--
#ifndef DISABLE_DEPRECATED
-- The length of time the user must keep the mouse over a widget before the tip
-- is shown, can be altered with 'tooltipsSetDelay'. This is set on a 'per group
-- of tooltips' basis.
--
#endif
-- To assign a tip to a particular widget, 'tooltipsSetTip' is used.
--
-- To associate 'Tooltips' to a widget it is has to have its own 'DrawWindow'.
-- Otherwise the widget must be set into an 'EventBox'.
--
module Graphics.UI.Gtk.Misc.Tooltips (
  Tooltips,
  TooltipsClass,
  castToTooltips,
  tooltipsNew,
  tooltipsEnable,
  tooltipsDisable,
#ifndef DISABLE_DEPRECATED
  tooltipsSetDelay,
#endif
  tooltipsSetTip,
  tooltipsDataGet
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new goup of 'Tooltips'.
--
tooltipsNew :: IO Tooltips
tooltipsNew  = makeNewObject mkTooltips $ 
  liftM castPtr {#call unsafe tooltips_new#}

-- | Display the help the 'Tooltips' group
-- provides.
--
tooltipsEnable :: TooltipsClass t => t -> IO ()
tooltipsEnable t = {#call unsafe tooltips_enable#} (toTooltips t)

-- | Disable 'Tooltips' group.
--
-- * Causes all tooltips in tooltips to become inactive. Any widgets that have
--   tips associated with that group will no longer display their tips until
--   they are enabled again with 'tooltipsEnable'.
--
tooltipsDisable :: TooltipsClass t => t -> IO ()
tooltipsDisable t = {#call unsafe tooltips_disable#} (toTooltips t)

#ifndef DISABLE_DEPRECATED
-- | Sets the time between the user moving the mouse
-- over a widget and the widget's tooltip appearing.
--
-- * The @time@ parameter is in ms.
--
tooltipsSetDelay :: TooltipsClass t => t -> Int -> IO ()
tooltipsSetDelay t time = {#call unsafe tooltips_set_delay#}
  (toTooltips t) (fromIntegral time)
#endif

-- | Adds a tooltip containing the message tipText to
-- the specified GtkWidget.
--
-- * The @tipPrivate@ parameter is meant to give a thorough
--   explaination. This might someday be accessible to a questionmark cursor
--   (like MS Windows).
--
tooltipsSetTip :: (TooltipsClass t, WidgetClass w) => t -> w -> String ->
                  String -> IO ()
tooltipsSetTip t w tipText tipPrivate = 
  withUTFString tipPrivate $ \priPtr ->
  withUTFString tipText $ \txtPtr ->
  {#call unsafe tooltips_set_tip#} (toTooltips t) (toWidget w) txtPtr priPtr

{#pointer * TooltipsData#}

-- | Retrieves any 'Tooltips' previously associated with the given widget.
--
tooltipsDataGet :: WidgetClass w => w -> IO (Maybe (Tooltips, String, String))
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

