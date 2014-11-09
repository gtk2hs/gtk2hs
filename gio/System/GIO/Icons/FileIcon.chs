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
module System.GIO.Icons.FileIcon (
-- * Details
--
-- | 'FileIcon' specifies an icon by pointing to an image file to be used as icon.

-- * Types
    FileIcon(..),
    FileIconClass,

-- * Methods
    fileIconNew,
    fileIconGetFile,
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

-------------------
-- Methods
-- | Creates a new icon for a file.
fileIconNew :: FileClass file => file -> IO FileIcon
fileIconNew file =
  {#call g_file_icon_new#} (toFile file)
  >>= (wrapNewGObject mkFileIcon . return) . castPtr

-- | Gets the 'File' associated with the given icon.
fileIconGetFile :: FileIconClass icon => icon -> IO File
fileIconGetFile icon =
  makeNewGObject mkFile $
  {#call g_file_icon_get_file#} (toFileIcon icon)
