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
module Media.Streaming.GStreamer.Base.BaseSrc (
  
  BaseSrc,
  BaseSrcClass,
  castToBaseSrc,
  toBaseSrc,
  baseSrcWaitPlaying,
  baseSrcIsLive,
  baseSrcGetPad,
  baseSrcBlockSize,
  baseSrcNumBuffers,
  baseSrcTypefind
  
  ) where

import Control.Monad (liftM)
{#import Media.Streaming.GStreamer.Base.Types#}
import System.Glib.FFI
import System.Glib.Attributes
{#import System.Glib.Properties#}

{# context lib = "gstreamer" prefix = "gst" #}

baseSrcWaitPlaying :: BaseSrcClass baseSrcT
                   => baseSrcT
                   -> IO FlowReturn
baseSrcWaitPlaying baseSrc =
    liftM cToEnum $
        {# call base_src_wait_playing #} (toBaseSrc baseSrc)

baseSrcIsLive :: BaseSrcClass baseSrcT
              => baseSrcT
              -> IO Bool
baseSrcIsLive baseSrc =
    liftM toBool $ {# call base_src_is_live #} (toBaseSrc baseSrc)

baseSrcGetPad :: BaseSrcClass baseSrcT
              => baseSrcT
              -> IO Pad
baseSrcGetPad baseSrc =
    withBaseSrc (toBaseSrc baseSrc) cBaseSrcGetPad >>= peekPad
foreign import ccall unsafe "_hs_gst_base_src_get_pad"
    cBaseSrcGetPad :: Ptr BaseSrc
                   -> IO (Ptr Pad)

-- FIXME: blocksize is actually a gulong...
baseSrcBlockSize :: BaseSrcClass baseSrcT
                 => Attr baseSrcT Word64
baseSrcBlockSize = newAttr
    (objectGetPropertyUInt64 "blocksize")
    (objectSetPropertyUInt64 "blocksize")

baseSrcNumBuffers :: BaseSrcClass baseSrcT
                  => Attr baseSrcT Int
baseSrcNumBuffers =
    newAttrFromIntProperty "num-buffers"

baseSrcTypefind :: BaseSrcClass baseSrcT
                => Attr baseSrcT Bool
baseSrcTypefind =
    newAttrFromBoolProperty "typefind"
