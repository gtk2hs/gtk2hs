-- -*-haskell-*-
--  GIMP Toolkit (GTK) Keys
--
--  Author : Jens Petersen
--
--  Created: 24 May 2002
--
--  Version $Revision: 1.5 $ from $Date: 2005/10/18 00:56:34 $
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
--
-- TODO
--
-- Documentation
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Gdk KeyVal functions.
--
module Graphics.UI.Gtk.Gdk.Keys (
  KeyVal,
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
keyvalName :: KeyVal -> IO String
keyvalName keyval = do
  strPtr <- {# call gdk_keyval_name #} (fromIntegral keyval)
  if strPtr==nullPtr then return "" else peekUTFString strPtr

-- | Converts a key name to a key value.
--
keyvalFromName :: String -> IO KeyVal
keyvalFromName keyvalName =
  liftM fromIntegral $
  withCString keyvalName $ \keyvalNamePtr ->
  {# call gdk_keyval_from_name #}
    keyvalNamePtr

-- | Convert from a Gdk key symbol to the corresponding Unicode character.
--
keyvalToChar :: 
    KeyVal          -- ^ @keyval@ - a Gdk key symbol
 -> IO (Maybe Char) -- ^ returns the corresponding unicode character, or
                    -- Nothing if there is no corresponding character.
keyvalToChar keyval =
  {# call gdk_keyval_to_unicode #} (fromIntegral keyval)
  >>= \code -> if code == 0 then return Nothing
                            else return $ Just $ toEnum $ fromIntegral code
