-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Tooltips@
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2002/08/05 16:41:34 $
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
-- @description@ --------------------------------------------------------------
--
-- * Tooltips are the messages that appear next to a widget when the mouse
--   pointer is held over it for a short amount of time. They are especially
--   helpful for adding more verbose descriptions of things such as buttons
--   in a toolbar. 
--   An individual tooltip belongs to a group of tooltips. A group is created
--   with a call to @ref method tooltipsNew@. Every tooltip in the group can 
--   then be 
--   turned off with a call to @ref method tooltipsDisable@ and enabled with 
--   @ref method tooltipsEnable@. The length of time the user must keep the
--   mouse over a 
--   widget before the tip is shown, can be altered with 
--   @ref method tooltipsSetDelay@.
--   This is set on a 'per group of tooltips' basis.
--   To assign a tip to a particular widget, @ref method tooltipsSetTip@
--   is used.
--
-- @documentation@ ------------------------------------------------------------
--
-- * To associate @ref data Tooltips@ to a widget it is has to have its own 
--   GdkWindow.
--   Otherwise the widget must be set into a EventBox. Can this be done
--   automatically? Perhaps even with tooltips_force_window()?
--
-- @todo@ ---------------------------------------------------------------------

module Tooltips(
  Tooltips,
  TooltipsClass,
  castToTooltips,
  tooltipsNew,
  tooltipsEnable,
  tooltipsDisable,
  tooltipsSetDelay,
  tooltipsSetTip
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor tooltipsNew@ Create a new goup of @ref type Tooltips@.
--
tooltipsNew :: IO Tooltips
tooltipsNew  = makeNewObject mkTooltips $ 
  liftM castPtr {#call unsafe tooltips_new#}

-- @method tooltipsEnable@ Display the help the @ref type Tooltips@ group
-- provides.
--
tooltipsEnable :: TooltipsClass t => t -> IO ()
tooltipsEnable t = {#call unsafe tooltips_enable#} (toTooltips t)

-- @method tooltipsDisable@ Disable @ref type Tooltips@ group.
--
-- * Causes all tooltips in tooltips to become inactive. Any widgets that have
--   tips associated with that group will no longer display their tips until
--   they are enabled again with @ref method tooltipsEnable@.
--
tooltipsDisable :: TooltipsClass t => t -> IO ()
tooltipsDisable t = {#call unsafe tooltips_disable#} (toTooltips t)

-- @method tooltipsSetDelay@ Sets the time between the user moving the mouse
-- over a widget and the widget's tooltip appearing.
--
-- * The @ref arg time@ parameter is in ms.
--
tooltipsSetDelay :: TooltipsClass t => t -> Int -> IO ()
tooltipsSetDelay t time = {#call unsafe tooltips_set_delay#}
  (toTooltips t) (fromIntegral time)

-- @method tooltipsSetTip@ Adds a tooltip containing the message tipText to
-- the specified GtkWidget.
--
-- * The @ref arg tipPrivate@ parameter is meant to give a thorough
--   explaination. This might someday be accessible to a questionmark cursor
--   (like MS Windows).
--
tooltipsSetTip :: (TooltipsClass t, WidgetClass w) => t -> w -> String ->
                  String -> IO ()
tooltipsSetTip t w tipText tipPrivate = 
  withCString tipPrivate $ \priPtr ->
  withCString tipText $ \txtPtr ->
  {#call unsafe tooltips_set_tip#} (toTooltips t) (toWidget w) txtPtr priPtr

