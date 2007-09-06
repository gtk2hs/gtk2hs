--  GIMP Toolkit (GTK) Binding for Haskell: binding to libgnomevfs -*-haskell-*-
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
--  GnomeVFS, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original GnomeVFS documentation,
--  Copyright (c) 2001 Seth Nickell <snickell@stanford.edu>. The
--  documentation is covered by the GNU Free Documentation License,
--  version 1.2.

-- #hide

-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module System.Gnome.VFS.Marshal (
  
  cToEnum,
  cFromEnum,
  cToBool,
  cFromBool,
  cToFlags,
  cFromFlags,
  genericResultMarshal,
  voidResultMarshal,
  newObjectResultMarshal,
  volumeOpCallbackMarshal
  
  ) where

import Control.Exception
import Control.Monad (liftM)
import Data.Dynamic
import System.Glib.FFI
import System.Glib.Flags (Flags, toFlags, fromFlags)
import System.Glib.UTFString (peekUTFString)
{#import System.Gnome.VFS.Types#}
import System.Gnome.VFS.Error
import Prelude hiding (error)

cToEnum :: (Integral a, Enum b) => a -> b
cToEnum = toEnum . fromIntegral

cFromEnum :: (Enum a, Integral b) => a -> b
cFromEnum = fromIntegral . fromEnum

cToBool :: Integral a => a -> Bool
cToBool  = toBool . fromIntegral

cFromBool :: Integral a => Bool -> a
cFromBool = fromIntegral . fromBool

cToFlags :: (Integral a, Flags b) => a -> [b]
cToFlags = toFlags . fromIntegral

cFromFlags :: (Flags a, Integral b) => [a] -> b
cFromFlags = fromIntegral . fromFlags

genericResultMarshal :: IO {# type GnomeVFSResult #}
                     -> IO a
                     -> IO b
                     -> IO a
genericResultMarshal cAction cSuccessAction cFailureAction =
    do result <- liftM cToEnum $ cAction
       case result of
         Ok        -> cSuccessAction
         errorCode -> do cFailureAction
                         error result

voidResultMarshal :: IO {# type GnomeVFSResult #}
                  -> IO ()
voidResultMarshal cAction =
    genericResultMarshal cAction (return ()) (return ())

newObjectResultMarshal :: (ForeignPtr obj -> obj)
                       -> (Ptr (Ptr obj) -> IO {# type GnomeVFSResult #})
                       -> IO obj
newObjectResultMarshal objConstructor cNewObj =
    alloca $ \cObjPtr ->
        do poke cObjPtr nullPtr
           genericResultMarshal (cNewObj cObjPtr)
                                 (do cObj <- peek cObjPtr
                                     assert (cObj /= nullPtr) $ return ()
                                     newObj <- newForeignPtr_ cObj
                                     return $ objConstructor newObj)
                                 (do cObj <- peek cObjPtr
                                     assert (cObj == nullPtr) $ return ())

volumeOpCallbackMarshal :: VolumeOpSuccessCallback
                        -> VolumeOpFailureCallback
                        -> IO {# type GnomeVFSVolumeOpCallback #}
volumeOpCallbackMarshal successCallback failureCallback =
    let cCallback :: CVolumeOpCallback
        cCallback cSucceeded cError cDetailedError cUserData =
            let succeeded = cToBool cSucceeded
                cCallbackFunPtr = castPtrToFunPtr cUserData
            in finally (freeHaskellFunPtr cCallbackFunPtr) $
                   if succeeded
                      then assert (and [cError == nullPtr, cDetailedError == nullPtr]) $
                                  successCallback
                      else assert (and [cError /= nullPtr, cDetailedError /= nullPtr]) $
                               do error <- peekUTFString cError
                                  detailedError <- peekUTFString cDetailedError
                                  failureCallback error detailedError
    in makeVolumeOpCallback cCallback
foreign import ccall safe "wrapper"
  makeVolumeOpCallback :: CVolumeOpCallback
                       -> IO {# type GnomeVFSVolumeOpCallback #}
