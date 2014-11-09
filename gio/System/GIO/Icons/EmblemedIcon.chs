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
module System.GIO.Icons.EmblemedIcon (
-- * Details
--
-- | 'EmblemedIcon' is an implementation of 'Icon' that supports adding an emblem to an icon. Adding
-- multiple emblems to an icon is ensured via 'emblemedIconAddEmblem'.
--
-- Note that 'Emblem'edIcon allows no control over the position of the emblems. See also 'Emblem' for
-- more information.

#if GLIB_CHECK_VERSION(2,18,0)
-- * Types
    EmblemedIcon(..),
    EmblemedIconClass,

-- * Methods
   emblemedIconNew,
   emblemedIconGetIcon,
   emblemedIconGetEmblems,
   emblemedIconAddEmblem,
#endif
    ) where

import Control.Monad
import System.GIO.Enums
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
import System.Glib.GList
import System.Glib.GObject
import System.Glib.UTFString
{#import System.GIO.Types#}

{# context lib = "gio" prefix = "g" #}

#if GLIB_CHECK_VERSION(2,18,0)
-------------------
-- Methods
-- | Creates a new emblemed icon for icon with the emblem emblem.
emblemedIconNew :: (IconClass icon, EmblemClass emblem) => icon -> emblem -> IO EmblemedIcon
emblemedIconNew icon emblem =
  {#call g_emblemed_icon_new#}
    (toIcon icon) (toEmblem emblem)
  >>= (constructNewGObject mkEmblemedIcon . return) . castPtr

-- | Gets the main icon for emblemed.
emblemedIconGetIcon :: EmblemedIconClass emblemed => emblemed
 -> IO Icon  -- ^ returns  a 'Icon' that is owned by emblemed
emblemedIconGetIcon emblemed =
    makeNewGObject mkIcon $
    {#call g_emblemed_icon_get_icon#} (toEmblemedIcon emblemed)

-- | Gets the list of emblems for the icon.
emblemedIconGetEmblems :: EmblemedIconClass emblemed => emblemed
 -> IO [Emblem] -- ^ returns  a list of 'Emblem' s that is owned by emblemed
emblemedIconGetEmblems emblemed = do
  glistPtr <- {#call g_emblemed_icon_get_emblems#} (toEmblemedIcon emblemed)
  emblemPtrs <- readGList glistPtr
  mapM (makeNewGObject mkEmblem . return) emblemPtrs

-- | Adds emblem to the 'Emblem'.
emblemedIconAddEmblem :: EmblemedIconClass emblemed => emblemed
 -> Emblem
 -> IO ()
emblemedIconAddEmblem emblemed emblem =
  {#call g_emblemed_icon_add_emblem#}
    (toEmblemedIcon emblemed)
    emblem

#endif
