-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Viewport@
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
-- * A @ref data Viewport@ a helper widget that adds Adjustment slots to a 
--   widget, i.e.
--   the widget becomes scrollable. It can then be put into 
--   @ref data ScrolledWindow@
--   and will behave as expected.
--
-- @documentation@ ------------------------------------------------------------
--
-- * The binding of this widget is superfluous as far as I can tell.
--
-- * The only signal this widget registers is "set-scroll-adjustments". It is
--   not bound because it is meant to be received by the @ref data Viewport@
--    and sent
--   by @ref data ScrolledWindow@.
--
-- @todo@ ---------------------------------------------------------------------

module Viewport(
  Viewport,
  ViewportClass,
  castToViewport,
  viewportNew,
  viewportGetHAdjustment,
  viewportGetVAdjustment,
  viewportSetHAdjustment,
  viewportSetVAdjustment,
  ShadowType(..),
  viewportSetShadowType
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(ShadowType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor viewportNew@ Create a new @ref type Viewport@.
--
viewportNew :: Adjustment -> Adjustment -> IO Viewport
viewportNew vAdj hAdj = makeNewObject mkViewport $ liftM castPtr $
  {#call unsafe viewport_new#} hAdj vAdj

-- @method viewportGetHAdjustment@ Retrieve the horizontal
-- @ref arg Adjustment@ of the @ref type Viewport@.
--
viewportGetHAdjustment :: ViewportClass v => v -> IO Adjustment
viewportGetHAdjustment v = makeNewObject mkAdjustment $
  {#call unsafe viewport_get_hadjustment#} (toViewport v)

-- @method viewportGetVAdjustment@ Retrieve the vertical @ref arg Adjustment@
-- of the @ref type Viewport@.
--
viewportGetVAdjustment :: ViewportClass v => v -> IO Adjustment
viewportGetVAdjustment v = makeNewObject mkAdjustment $
  {#call unsafe viewport_get_vadjustment#} (toViewport v)

-- @method viewportSetHAdjustment@ Set the horizontal @ref arg Adjustment@ of
-- the @ref type Viewport@.
--
viewportSetHAdjustment :: ViewportClass v => v -> Adjustment -> IO ()
viewportSetHAdjustment v adj = {#call viewport_set_hadjustment#}
  (toViewport v) adj

-- @method viewportSetVAdjustment@ Set the vertical @ref arg Adjustment@ of
-- the @ref type Viewport@.
--
viewportSetVAdjustment :: ViewportClass v => v -> Adjustment -> IO ()
viewportSetVAdjustment v adj = {#call viewport_set_hadjustment#}
  (toViewport v) adj

-- @method viewportSetShadowType@ Specify if and how an outer frame should be
-- drawn around the child.
--
viewportSetShadowType :: ViewportClass v => v -> ShadowType -> IO ()
viewportSetShadowType v st = {#call viewport_set_shadow_type#} (toViewport v)
  ((fromIntegral.fromEnum) st)

-- signals


