-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Viewport
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.7 $ from $Date: 2005/04/07 00:40:16 $
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
-- Issues:
--
-- The binding of this widget is superfluous as far as I can tell.
--
-- The only signal this widget registers is \"set-scroll-adjustments\". It is
--   not bound because it is meant to be received by the 'Viewport'
--   and sent by 'ScrolledWindow'.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- An adapter which makes widgets scrollable
--
module Graphics.UI.Gtk.Misc.Viewport (
-- * Detail
--
-- | A 'Viewport' is a helper widget that adds Adjustment slots to a
-- widget, i.e. the widget becomes scrollable. It can then be put into
-- 'ScrolledWindow' and will behave as expected.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----Viewport
-- @

-- * Types
  Viewport,
  ViewportClass,
  castToViewport,

-- * Constructors
  viewportNew,

-- * Methods
  viewportGetHAdjustment,
  viewportGetVAdjustment,
  viewportSetHAdjustment,
  viewportSetVAdjustment,
  ShadowType(..),
  viewportSetShadowType,
  viewportGetShadowType,

-- * Properties
  viewportHAdjustment,
  viewportVAdjustment,
  viewportShadowType
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.Attributes		(Attr(..))
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums	(ShadowType(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'Viewport' with the given adjustments.
--
viewportNew :: 
    Adjustment  -- ^ @hadjustment@ - horizontal adjustment.
 -> Adjustment  -- ^ @vadjustment@ - vertical adjustment.
 -> IO Viewport
viewportNew hadjustment vadjustment =
  makeNewObject mkViewport $
  liftM (castPtr :: Ptr Widget -> Ptr Viewport) $
  {# call unsafe viewport_new #}
    hadjustment
    vadjustment

--------------------
-- Methods

-- | Returns the horizontal adjustment of the viewport.
--
viewportGetHAdjustment :: ViewportClass self => self -> IO Adjustment
viewportGetHAdjustment self =
  makeNewObject mkAdjustment $
  {# call unsafe viewport_get_hadjustment #}
    (toViewport self)

-- | Returns the vertical adjustment of the viewport.
--
viewportGetVAdjustment :: ViewportClass self => self -> IO Adjustment
viewportGetVAdjustment self =
  makeNewObject mkAdjustment $
  {# call unsafe viewport_get_vadjustment #}
    (toViewport self)

-- | Sets the horizontal adjustment of the viewport.
--
viewportSetHAdjustment :: ViewportClass self => self -> Adjustment -> IO ()
viewportSetHAdjustment self adjustment =
  {# call viewport_set_hadjustment #}
    (toViewport self)
    adjustment

-- | Sets the vertical adjustment of the viewport.
--
viewportSetVAdjustment :: ViewportClass self => self -> Adjustment -> IO ()
viewportSetVAdjustment self adjustment =
  {# call viewport_set_vadjustment #}
    (toViewport self)
    adjustment

-- | Sets the shadow type of the viewport.
--
viewportSetShadowType :: ViewportClass self => self
 -> ShadowType -- ^ @type@ - the new shadow type.
 -> IO ()
viewportSetShadowType self type_ =
  {# call viewport_set_shadow_type #}
    (toViewport self)
    ((fromIntegral . fromEnum) type_)

-- | Gets the shadow type of the 'Viewport'. See 'viewportSetShadowType'.
--
viewportGetShadowType :: ViewportClass self => self
 -> IO ShadowType -- ^ returns the shadow type
viewportGetShadowType self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe viewport_get_shadow_type #}
    (toViewport self)

--------------------
-- Properties

-- | The 'Adjustment' that determines the values of the horizontal position
-- for this viewport.
--
viewportHAdjustment :: ViewportClass self => Attr self Adjustment
viewportHAdjustment = Attr 
  viewportGetHAdjustment
  viewportSetHAdjustment

-- | The 'Adjustment' that determines the values of the vertical position for
-- this viewport.
--
viewportVAdjustment :: ViewportClass self => Attr self Adjustment
viewportVAdjustment = Attr 
  viewportGetVAdjustment
  viewportSetVAdjustment

-- | Determines how the shadowed box around the viewport is drawn.
--
-- Default value: 'ShadowIn'
--
viewportShadowType :: ViewportClass self => Attr self ShadowType
viewportShadowType = Attr 
  viewportGetShadowType
  viewportSetShadowType

--------------------
-- Signals

