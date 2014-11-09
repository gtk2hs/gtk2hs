{-# LANGUAGE CPP #-}
--  GIMP Toolkit (GTK) Binding for Haskell: binding to gio -*-haskell-*-
--
--  Author : Andy Stewart
--  Created: 30-Apirl-2010
--
--  Copyright (c) 2010 Andy Stewart
--
--  This library is free software: you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public License
--  as published by the Free Software Foundation, either version 3 of
--  the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
--  You should have received a copy of the GNU Lesser General Public
--  License along with this program.  If not, see
--  <http://www.gnu.org/licenses/>.
--
--  GIO, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original GIO documentation.
--
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module System.GIO.Icons.Emblem (
-- * Details
--
-- | 'Emblem' is an implementation of 'Icon' that supports having an emblem, which is an icon with
-- additional properties. It can than be added to a 'EmblemedIcon'.
--
-- Currently, only metainformation about the emblem's origin is supported. More may be added in the
-- future.

#if GLIB_CHECK_VERSION(2,18,0)
-- * Types
    Emblem(..),
    EmblemClass,

-- * Enums
    EmblemOrigin (..),

-- * Methods
    emblemNew,
    emblemNewWithOrigin,
    emblemGetIcon,
    emblemGetOrigin,
#endif
    ) where

import Control.Monad
import System.GIO.Enums
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
import System.Glib.GObject
import System.Glib.UTFString
{#import System.GIO.Types#}

{# context lib = "gio" prefix = "g" #}

#if GLIB_CHECK_VERSION(2,18,0)
-------------------
-- Methods
-- | Creates a new emblem for icon.
emblemNew :: IconClass icon => icon -> IO Emblem
emblemNew icon =
    wrapNewGObject mkEmblem $
    {#call g_emblem_new#} (toIcon icon)

-- | Creates a new emblem for icon.
emblemNewWithOrigin :: IconClass icon
 => icon  -- ^ @icon@    a 'Icon' containing the icon.
 -> EmblemOrigin  -- ^ @origin@  a 'EmblemOrigin' enum defining the emblem's origin
 -> IO Emblem
emblemNewWithOrigin icon origin =
    wrapNewGObject mkEmblem $
    {#call g_emblem_new_with_origin#} (toIcon icon) ((fromIntegral . fromEnum) origin)

-- | Gives back the icon from emblem.
emblemGetIcon :: EmblemClass emblem
 => emblem  -- ^ @emblem@  a 'Emblem' from which the icon should be extracted.
 -> IO Icon -- ^ returns a 'Icon'. The returned object belongs to the emblem and should not be modified or freed.
emblemGetIcon emblem =
    makeNewGObject mkIcon $
    {#call g_emblem_get_icon#} (toEmblem emblem)

-- | Gets the origin of the emblem.
emblemGetOrigin :: EmblemClass emblem => emblem
 -> IO EmblemOrigin
emblemGetOrigin emblem =
  liftM (toEnum . fromIntegral) $
  {#call g_emblem_get_origin#} (toEmblem emblem)
#endif
