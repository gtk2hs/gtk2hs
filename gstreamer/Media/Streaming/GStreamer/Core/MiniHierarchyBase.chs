{-# OPTIONS_HADDOCK hide #-}
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

-- #hide

-- | Maintainer  : gtk2hs-devel\@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module Media.Streaming.GStreamer.Core.MiniHierarchyBase (
  module System.Glib.GObject,
  MiniObject(..),
  MiniObjectClass(..),
  mkMiniObject,
  unMiniObject,
  isMiniObject,
  castToMiniObject,
  mkCastToMiniObject,
  mkIsMiniObject,
  ) where

import System.Glib.FFI
import System.Glib.GType
import System.Glib.GObject

{# context lib = "gstreamer" prefix = "gst" #}

{# pointer *GstMiniObject as MiniObject foreign newtype #}

mkMiniObject = MiniObject
unMiniObject (MiniObject o) = o

isMiniObject :: MiniObjectClass obj
             => obj
             -> Bool
isMiniObject _ = True

class MiniObjectClass o where
    toMiniObject :: o -> MiniObject
    unsafeCastMiniObject :: MiniObject -> o

instance MiniObjectClass MiniObject where
    toMiniObject = id
    unsafeCastMiniObject = id

castToMiniObject :: MiniObjectClass obj
                 => obj
                 -> MiniObject
castToMiniObject = mkMiniObject . castForeignPtr . unMiniObject . toMiniObject

-- The usage of foreignPtrToPtr should be safe as the evaluation will only be
-- forced if the object is used afterwards
--
mkCastToMiniObject :: (MiniObjectClass obj, MiniObjectClass obj')
                   => GType
                   -> String
                   -> (obj -> obj')
mkCastToMiniObject gtype objTypeName obj =
  case toMiniObject obj of
    gobj@(MiniObject objFPtr)
      | typeInstanceIsA ((unsafeForeignPtrToPtr.castForeignPtr) objFPtr) gtype
                  -> unsafeCastMiniObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName

mkIsMiniObject :: MiniObjectClass obj
               => GType
               -> obj
               -> Bool
mkIsMiniObject gType obj =
    unsafePerformIO $
        withForeignPtr (unMiniObject $ toMiniObject obj) $ \objPtr ->
            return $ typeInstanceIsA (castPtr objPtr) gType
