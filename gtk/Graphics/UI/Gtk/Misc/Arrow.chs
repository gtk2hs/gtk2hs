{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Arrow
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
-- Displays an arrow
--
module Graphics.UI.Gtk.Misc.Arrow (
-- * Detail
--
-- | 'Arrow' should be used to draw simple arrows that need to point in one of
-- the four cardinal directions (up, down, left, or right). The style of the
-- arrow can be one of shadow in, shadow out, etched in, or etched out. Note
-- that these directions and style types may be ammended in versions of Gtk to
-- come.
--
-- 'Arrow' will fill any space alloted to it, but since it is inherited from
-- 'Misc', it can be padded and\/or aligned, to fill exactly the space the
-- programmer desires.
--
-- Arrows are created with a call to 'arrowNew'. The direction or style of
-- an arrow can be changed after creation by using 'arrowSet'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Misc'
-- |                     +----Arrow
-- @

-- * Types
  Arrow,
  ArrowClass,
  castToArrow, gTypeArrow,
  toArrow,
  ArrowType(..),
  ShadowType(..),

-- * Constructors
  arrowNew,

-- * Methods
  arrowSet,

-- * Attributes
  arrowArrowType,
  arrowShadowType,
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.General.Enums    (ArrowType(..), ShadowType(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new arrow widget.
--
arrowNew :: ArrowType -> ShadowType -> IO Arrow
arrowNew arrowType shadowType =
  makeNewObject mkArrow $
  liftM (castPtr :: Ptr Widget -> Ptr Arrow) $
  {# call unsafe arrow_new #}
    ((fromIntegral . fromEnum) arrowType)
    ((fromIntegral . fromEnum) shadowType)

--------------------
-- Methods

-- | Sets the direction and style of the 'Arrow'.
--
arrowSet :: ArrowClass self => self -> ArrowType -> ShadowType -> IO ()
arrowSet self arrowType shadowType =
  {# call arrow_set #}
    (toArrow self)
    ((fromIntegral . fromEnum) arrowType)
    ((fromIntegral . fromEnum) shadowType)

--------------------
-- Attributes

-- | The direction the arrow should point.
--
-- Default value: 'ArrowRight'
--
arrowArrowType :: ArrowClass self => Attr self ArrowType
arrowArrowType = newAttrFromEnumProperty "arrow-type"
  {# call pure unsafe gtk_arrow_type_get_type #}

-- | Appearance of the shadow surrounding the arrow.
--
-- Default value: 'ShadowOut'
--
arrowShadowType :: ArrowClass self => Attr self ShadowType
arrowShadowType = newAttrFromEnumProperty "shadow-type"
  {# call pure unsafe gtk_shadow_type_get_type #}
