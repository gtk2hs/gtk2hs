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

-- # hide

-- | Maintainer  : gtk2hs-devel\@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module Media.Streaming.GStreamer.Core.HierarchyBase (
  mkCastToGObject,
  mkIsGObject,
  ) where

import System.Glib.FFI
import System.Glib.GType
import System.Glib.GObject

-- The usage of foreignPtrToPtr should be safe as the evaluation will only be
-- forced if the object is used afterwards
--
mkCastToGObject :: (GObjectClass obj, GObjectClass obj')
                => GType
                -> String
                -> (obj -> obj')
mkCastToGObject gtype objTypeName obj =
  case toGObject obj of
    gobj@(GObject objFPtr)
      | typeInstanceIsA ((unsafeForeignPtrToPtr.castForeignPtr) objFPtr) gtype
                  -> unsafeCastGObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName

mkIsGObject :: GObjectClass obj
            => GType
            -> obj
            -> Bool
mkIsGObject gType obj =
    unsafePerformIO $
        withForeignPtr (unGObject $ toGObject obj) $ \objPtr ->
            return $ typeInstanceIsA (castPtr objPtr) gType
