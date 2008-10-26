-- -*-haskell-*-
--  GIMP Toolkit (GTK) Keys
--
--  Author : Jens Petersen
--
--  Created: 24 May 2002
--
--  Copyright (C) 2002-2005 Jens Petersen
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
-- #prune

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A 'KeyVal' is a numeric value identifying a keyboard key. The defined
-- values can be found at <http://gitweb.freedesktop.org/?p=xorg/proto/x11proto.git;a=blob_plain;f=keysymdef.h>.
-- The names of the keys are the names of the macros without the prefix.
--
module Graphics.UI.Gtk.Gdk.Keys (
  KeyVal,
  keyName,
  keyFromName,
  keyToChar,

  keyvalName,
  keyvalFromName,
  keyvalToChar,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString

{#context lib="gdk" prefix ="gdk"#}

-- | Key values are the codes which are sent whenever a key is pressed or
-- released.
--
type KeyVal = Word32

-- | Converts a key value into a symbolic name.
--
keyName :: KeyVal -> String
keyName k = unsafePerformIO $ keyvalName k

-- | Converts a key name to a key value.
--
keyFromName :: String -> KeyVal
keyFromName k = unsafePerformIO $ keyvalFromName k

-- | Convert from a Gdk key symbol to the corresponding Unicode character.
--
keyToChar :: 
    KeyVal          -- ^ @keyval@ - a Gdk key symbol
 -> Maybe Char -- ^ returns the corresponding unicode character, or
               -- Nothing if there is no corresponding character.
keyToChar k = unsafePerformIO $ keyvalToChar k

keyvalName :: KeyVal -> IO String
keyvalName keyval = do
  strPtr <- {# call gdk_keyval_name #} (fromIntegral keyval)
  if strPtr==nullPtr then return "" else peekUTFString strPtr

keyvalFromName :: String -> IO KeyVal
keyvalFromName keyvalName =
  liftM fromIntegral $
  withCString keyvalName $ \keyvalNamePtr ->
  {# call gdk_keyval_from_name #}
    keyvalNamePtr

keyvalToChar :: KeyVal -> IO (Maybe Char)
keyvalToChar keyval =
  {# call gdk_keyval_to_unicode #} (fromIntegral keyval)
  >>= \code -> if code == 0 then return Nothing
                            else return $ Just $ toEnum $ fromIntegral code
