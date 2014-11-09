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
module System.GIO.Icons.Icon (
-- * Details
--
-- | 'Icon' is a very minimal interface for icons. It provides functions for checking the equality of two
-- icons, hashing of icons and serializing an icon to and from strings.
--
-- 'Icon' does not provide the actual pixmap for the icon as this is out of GIO's scope, however
-- implementations of 'Icon' may contain the name of an icon (see 'ThemedIcon'), or the path to an icon
-- (see GLoadableIcon).
--
-- To obtain a hash of a 'Icon', see 'iconHash'.
--
-- To check if two 'Icon's are equal, see 'iconEqual'.
--
-- For serializing a 'Icon', use 'iconToString'.
--
-- If your application or library provides one or more 'Icon' implementations you need to ensure that
-- each GType is registered with the type system prior to calling 'iconNewForString'.

-- * Types
    Icon(..),
    IconClass,

-- * Methods
    iconEqual,
#if GLIB_CHECK_VERSION(2,20,0)
    iconToString,
    iconNewForString,
#endif
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

-- | Checks if two icons are equal.
iconEqual :: (IconClass icon1, IconClass icon2) => icon1 -> icon2
 -> IO Bool -- ^ returns 'True' if icon1 is equal to icon2. 'False' otherwise.
iconEqual icon1 icon2 =
   liftM toBool $
   {#call g_icon_equal#} (toIcon icon1) (toIcon icon2)

#if GLIB_CHECK_VERSION(2,20,0)
-- | Generates a textual representation of icon that can be used for serialization such as when passing
-- icon to a different process or saving it to persistent storage. Use 'iconNewForString' to get
-- icon back from the returned string.
--
-- The encoding of the returned string is proprietary to 'Icon' except in the following two cases
--
--   * If icon is a 'FileIcon', the returned string is a native path (such as /path/to/my icon.png)
--     without escaping if the 'File' for icon is a native file. If the file is not native, the returned
--     string is the result of 'fileGetUri'.
--
--   * If icon is a 'ThemedIcon' with exactly one name, the encoding is simply the name (such as
--   network-server).
iconToString :: IconClass icon => icon
 -> IO ByteString
iconToString icon = do
   sPtr <- {#call g_icon_to_string#} (toIcon icon)
   sLen <- lengthArray0 0 sPtr
   unsafePackCStringFinalizer (castPtr sPtr) (fromIntegral sLen)
        ({#call unsafe g_free#} (castPtr sPtr))

-- | Generate a 'Icon' instance from str. This function can fail if str is not valid - see
-- 'iconToString' for discussion.
--
-- If your application or library provides one or more 'Icon' implementations you need to ensure that
-- each GType is registered with the type system prior to calling 'iconNewForString'.
iconNewForString :: ByteString -> IO Icon
iconNewForString str =
    wrapNewGObject mkIcon $
    useAsCString str $ \ strPtr ->
    propagateGError ({# call g_icon_new_for_string #} strPtr)
#endif
