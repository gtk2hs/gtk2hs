-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Viewport
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.6 $ from $Date: 2004/05/23 16:07:53 $
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
-- A 'Viewport' a helper widget that adds Adjustment slots to a 
-- widget, i.e. the widget becomes scrollable. It can then be put into 
-- 'ScrolledWindow' and will behave as expected.
--
-- * The binding of this widget is superfluous as far as I can tell.
--
-- * The only signal this widget registers is \"set-scroll-adjustments\". It is
--   not bound because it is meant to be received by the 'Viewport'
--   and sent by 'ScrolledWindow'.
--

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
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(ShadowType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new 'Viewport'.
--
viewportNew :: Adjustment -> Adjustment -> IO Viewport
viewportNew vAdj hAdj = makeNewObject mkViewport $ liftM castPtr $
  {#call unsafe viewport_new#} hAdj vAdj

-- | Retrieve the horizontal
-- 'Adjustment' of the 'Viewport'.
--
viewportGetHAdjustment :: ViewportClass v => v -> IO Adjustment
viewportGetHAdjustment v = makeNewObject mkAdjustment $
  {#call unsafe viewport_get_hadjustment#} (toViewport v)

-- | Retrieve the vertical 'Adjustment'
-- of the 'Viewport'.
--
viewportGetVAdjustment :: ViewportClass v => v -> IO Adjustment
viewportGetVAdjustment v = makeNewObject mkAdjustment $
  {#call unsafe viewport_get_vadjustment#} (toViewport v)

-- | Set the horizontal 'Adjustment' of
-- the 'Viewport'.
--
viewportSetHAdjustment :: ViewportClass v => v -> Adjustment -> IO ()
viewportSetHAdjustment v adj = {#call viewport_set_hadjustment#}
  (toViewport v) adj

-- | Set the vertical 'Adjustment' of
-- the 'Viewport'.
--
viewportSetVAdjustment :: ViewportClass v => v -> Adjustment -> IO ()
viewportSetVAdjustment v adj = {#call viewport_set_hadjustment#}
  (toViewport v) adj

-- | Specify if and how an outer frame should be
-- drawn around the child.
--
viewportSetShadowType :: ViewportClass v => v -> ShadowType -> IO ()
viewportSetShadowType v st = {#call viewport_set_shadow_type#} (toViewport v)
  ((fromIntegral.fromEnum) st)

-- signals


