{-# LANGUAGE CPP #-}
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
module Media.Streaming.GStreamer.Base.BaseTransform (
  
  BaseTransform,
  BaseTransformClass,
  castToBaseTransform,
  gTypeBaseTransform,

  baseTransformIsPassthrough,
  baseTransformSetPassthrough,
  baseTransformIsInPlace,
  baseTransformSetInPlace,
  baseTransformIsQOSEnabled,
  baseTransformSetQOSEnabled,
  baseTransformGetSinkPad,
  baseTransformGetSrcPad,
  baseTransformQOS
  
  ) where

import Control.Monad (liftM)
{#import Media.Streaming.GStreamer.Base.Types#}
import System.Glib.FFI
#if __GLASGOW_HASKELL__ < 606
    hiding ( withObject )
#endif
import System.Glib.Flags
import System.Glib.Attributes
{#import System.Glib.Properties#}

{# context lib = "gstreamer" prefix = "gst" #}

baseTransformIsPassthrough :: BaseTransformClass baseTransformT
                           => baseTransformT
                           -> IO Bool
baseTransformIsPassthrough baseTransform =
    liftM toBool $
        {# call base_transform_is_passthrough #} (toBaseTransform baseTransform)

baseTransformSetPassthrough :: BaseTransformClass baseTransformT
                            => baseTransformT
                            -> Bool
                            -> IO ()
baseTransformSetPassthrough baseTransform passthrough =
    {# call base_transform_set_passthrough #} (toBaseTransform baseTransform)
                                              (fromBool passthrough)

baseTransformIsInPlace :: BaseTransformClass baseTransformT
                       => baseTransformT
                       -> IO Bool
baseTransformIsInPlace baseTransform =
    liftM toBool $
        {# call base_transform_is_in_place #} (toBaseTransform baseTransform)

baseTransformSetInPlace :: BaseTransformClass baseTransformT
                        => baseTransformT
                        -> Bool
                        -> IO ()
baseTransformSetInPlace baseTransform inPlace =
    {# call base_transform_set_in_place #} (toBaseTransform baseTransform)
                                           (fromBool inPlace)

baseTransformIsQOSEnabled :: BaseTransformClass baseTransformT
                          => baseTransformT
                          -> IO Bool
baseTransformIsQOSEnabled baseTransform =
    liftM toBool $
        {# call base_transform_is_qos_enabled #} (toBaseTransform baseTransform)

baseTransformSetQOSEnabled :: BaseTransformClass baseTransformT
                           => baseTransformT
                           -> Bool
                           -> IO ()
baseTransformSetQOSEnabled baseTransform enabled =
    {# call base_transform_set_qos_enabled #} (toBaseTransform baseTransform)
                                              (fromBool enabled)

baseTransformGetSinkPad :: BaseTransformClass baseTransformT
                        => baseTransformT
                        -> IO Pad
baseTransformGetSinkPad baseTransform =
    withObject (toBaseTransform baseTransform) cBaseTransformGetSinkPad >>=
        peekObject
foreign import ccall unsafe "_hs_gst_base_transform_get_sink_pad"
    cBaseTransformGetSinkPad :: Ptr BaseTransform
                             -> IO (Ptr Pad)

baseTransformGetSrcPad :: BaseTransformClass baseTransformT
                        => baseTransformT
                        -> IO Pad
baseTransformGetSrcPad baseTransform =
    withObject (toBaseTransform baseTransform) cBaseTransformGetSrcPad >>=
        peekObject
foreign import ccall unsafe "_hs_gst_base_transform_get_src_pad"
    cBaseTransformGetSrcPad :: Ptr BaseTransform
                             -> IO (Ptr Pad)

baseTransformQOS :: BaseTransformClass baseTransformT
                 => Attr baseTransformT Bool
baseTransformQOS = newAttr
    baseTransformIsQOSEnabled
    baseTransformSetQOSEnabled

