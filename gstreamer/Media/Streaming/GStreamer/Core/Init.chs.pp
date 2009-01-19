--  GIMP Toolkit (GTK) Binding for Haskell: binding to gstreamer -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
--
--  Copyright (c) 2007 Peter Gavin
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
--  GStreamer, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original GStreamer documentation.
--  
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module Media.Streaming.GStreamer.Core.Init (
  init,
  initCheck,
  deinit,
  version,
  versionString,
#if GST_CHECK_VERSION(0,10,10)
  segtrapIsEnabled,
  segtrapSetEnabled,
  registryForkIsEnabled,
  registryForkSetEnabled,
#endif
#if GST_CHECK_VERSION(0,10,12)
  updateRegistry
#endif
  ) where

import Control.Exception     ( assert )
import Control.Monad         ( liftM )
import Prelude hiding        ( init )
import System.Glib.FFI
import System.Glib.GError
import System.Glib.UTFString ( peekUTFString )

{# context lib = "gstreamer" prefix = "gst" #}

init :: IO ()
init =
    {# call init #} nullPtr nullPtr

initCheck :: IO ()
initCheck =
    propagateGError $ \gErrorPtr ->
        do succeeded <- liftM toBool $
                        {# call init_check #} nullPtr nullPtr gErrorPtr
           assert (if succeeded
                      then gErrorPtr == nullPtr
                      else gErrorPtr /= nullPtr) $ return ()

deinit :: IO ()
deinit =
    {# call deinit #}

version :: (Word, Word, Word, Word)
version =
    unsafePerformIO $
    alloca $ \majorPtr ->
        alloca $ \minorPtr ->
            alloca $ \microPtr ->
                alloca $ \nanoPtr ->
                    do {# call version #} majorPtr minorPtr microPtr nanoPtr
                       major <- peek majorPtr
                       minor <- peek minorPtr
                       micro <- peek microPtr
                       nano  <- peek nanoPtr
                       return (fromIntegral major,
                               fromIntegral minor,
                               fromIntegral micro,
                               fromIntegral nano)

versionString :: String
versionString =
    unsafePerformIO $
    {# call version_string #} >>= peekUTFString

#if GST_CHECK_VERSION(0,10,10)
segtrapIsEnabled :: IO Bool
segtrapIsEnabled =
    liftM toBool {# call segtrap_is_enabled #}

segtrapSetEnabled :: Bool
                  -> IO ()
segtrapSetEnabled enabled =
    {# call segtrap_set_enabled #} $ fromBool enabled

registryForkIsEnabled :: IO Bool
registryForkIsEnabled =
    liftM toBool {# call registry_fork_is_enabled #}

registryForkSetEnabled :: Bool
                       -> IO ()
registryForkSetEnabled enabled =
    {# call registry_fork_set_enabled #} $ fromBool enabled
#endif

#if GST_CHECK_VERSION(0,10,12)
updateRegistry :: IO Bool
updateRegistry =
    liftM toBool {# call update_registry #}
#endif
