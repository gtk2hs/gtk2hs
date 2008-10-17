--  GIMP Toolkit (GTK) Binding for Haskell: binding to gio -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 13-Oct-2008
--
--  Copyright (c) 2008 Peter Gavin
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
--  this library is based on the original GIO documentation,
--  Copyright (c) 2001 Seth Nickell <snickell@stanford.edu>. The
--  documentation is covered by the GNU Free Documentation License,
--  version 1.2.
--  
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module System.GIO.Base where

import Control.Monad

import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GObject

{#import System.GIO.Types#}

cToFlags :: (Integral int, Flags flags)
         => int
         -> [flags]
cToFlags = toFlags . fromIntegral
cFromFlags :: (Integral int, Flags flags)
           => [flags]
           -> int
cFromFlags = fromIntegral . fromFlags

cToEnum :: (Integral int, Enum enum)
        => int
        -> enum
cToEnum = toEnum . fromIntegral
cFromEnum :: (Integral int, Enum enum)
           => enum
           -> int
cFromEnum = fromIntegral . fromEnum

withGObject :: GObjectClass objectT
            => objectT
            -> (Ptr objectT -> IO a)
            -> IO a
withGObject object action =
    let objectFPtr = unGObject $ toGObject object
    in withForeignPtr (castForeignPtr objectFPtr) action

peekGObject :: GObjectClass obj
            => Ptr obj
            -> IO obj
peekGObject cObject = do
  do cObjectRef $ castPtr cObject
     takeGObject cObject
foreign import ccall unsafe "&g_object_unref"
  objectFinalizer :: FunPtr (Ptr () -> IO ())
foreign import ccall unsafe "g_object_ref"
  cObjectRef :: Ptr ()
             -> IO (Ptr ())

takeGObject :: GObjectClass obj
            => Ptr obj
            -> IO obj
takeGObject cObject =
    liftM (unsafeCastGObject . GObject . castForeignPtr) $
        do newForeignPtr (castPtr cObject) objectFinalizer

type AsyncReadyCallback = GObject -> AsyncResult -> IO ()
type CAsyncReadyCallback = Ptr GObject -> Ptr AsyncResult -> Ptr () -> IO ()
foreign import ccall "wrapper"
    makeAsyncReadyCallback :: CAsyncReadyCallback
                           -> IO {# type GAsyncReadyCallback #}

marshalAsyncReadyCallback :: AsyncReadyCallback -> IO {# type GAsyncReadyCallback #}
marshalAsyncReadyCallback asyncReadyCallback =
    makeAsyncReadyCallback cAsyncReadyCallback
    where cAsyncReadyCallback :: CAsyncReadyCallback
          cAsyncReadyCallback cObject cAsyncResult cCallback = do
            object <- peekGObject cObject
            asyncResult <- peekGObject cAsyncResult
            asyncReadyCallback object asyncResult
            freeHaskellFunPtr (castPtrToFunPtr cCallback)
