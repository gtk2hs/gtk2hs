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
module System.GIO.Icons.ThemedIcon (
-- * Details
--
-- | 'ThemeIcon' specifies an icon by pointing to an image file to be used as icon.

-- * Types
    ThemedIcon(..),
    ThemedIconClass,

-- * Methods
    themedIconNew,
    themedIconNewFromNames,
#if GLIB_CHECK_VERSION(2,18,0)
    themedIconPrependName,
#endif
    themedIconAppendName,
    themedIconGetNames,
    ) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString (useAsCString)
import Data.ByteString.Unsafe (unsafePackCStringFinalizer)
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
themedIconNew :: ByteString -- ^ @iconname@ a string containing an icon name.
 -> IO ThemedIcon
themedIconNew iconName =
  useAsCString iconName $ \ iconNamePtr ->
  {#call g_themed_icon_new#} iconNamePtr
  >>= (wrapNewGObject mkThemedIcon . return) . castPtr

-- | Creates a new themed icon for iconnames.
themedIconNewFromNames :: GlibString string
 => [string]  -- ^ @iconnames@ an array of strings containing icon names.
 -> IO ThemedIcon
themedIconNewFromNames iconNames = do
  let len = if null iconNames then (-1) else length iconNames
  withUTFStringArray iconNames $ \ iconNamesPtr ->
      {#call g_themed_icon_new_from_names#} iconNamesPtr (fromIntegral len)
      >>= (wrapNewGObject mkThemedIcon . return) . castPtr

#if GLIB_CHECK_VERSION(2,18,0)
-- | Prepend a name to the list of icons from within icon.
themedIconPrependName :: (ThemedIconClass icon, GlibString string) => icon
 -> string   -- ^ @iconname@ name of icon to prepend to list of icons from within icon.
 -> IO ()
themedIconPrependName icon iconname =
  withUTFString iconname $ \ iconnamePtr ->
  {#call g_themed_icon_prepend_name#} (toThemedIcon icon) iconnamePtr
#endif

-- | Append a name to the list of icons from within icon.
themedIconAppendName :: (ThemedIconClass icon, GlibString string) => icon
 -> string   -- ^ @iconname@ name of icon to append to list of icons from within icon.
 -> IO ()
themedIconAppendName icon iconname =
  withUTFString iconname $ \ iconnamePtr ->
  {#call g_themed_icon_append_name#} (toThemedIcon icon) iconnamePtr

-- | Gets the names of icons from within icon.
themedIconGetNames :: (ThemedIconClass icon, GlibString string) => icon
 -> IO [string] -- ^ returns a list of icon names.
themedIconGetNames icon =
  {#call g_themed_icon_get_names#} (toThemedIcon icon)
  >>= readUTFStringArray0

