-- GIMP Toolkit (GTK) Binding for Haskell: binding to gstreamer   -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
--
--  Version $Revision$ from $Date$
--
--  Copyright (c) 2007 Peter Gavin
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
module Media.Streaming.GStreamer.Init (
  init,
  initCheck,
  deinit,
  version,
  versionString,
  segtrapIsEnabled,
  segtrapSetEnabled,
  registryForkIsEnabled,
  registryForkSetEnabled,
  updateRegistry
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

updateRegistry :: IO Bool
updateRegistry =
    liftM toBool {# call update_registry #}
