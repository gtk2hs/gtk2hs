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
module Media.Streaming.GStreamer.Audio.AudioClock
    ( AudioClock
    , 
    ) where

import Control.Monad (liftM)
{#import Media.Streaming.GStreamer.Audio.Types#}
import Media.Streaming.GStreamer.Core
import System.Glib.FFI
import System.Glib.UTFString

{# context lib = "gstreamer" prefix = "gst" #}

type AudioClockGetTimeFunc = Clock -> IO ClockTime

type CAudioClockGetTimeFunc =  Ptr Clock
                            -> {# type gpointer #}
                            -> IO {# type GstClockTime #}
marshalAudioClockGetTimeFunc :: AudioClockGetTimeFunc
                             -> IO {# type GstAudioClockGetTimeFunc #}
marshalAudioClockGetTimeFunc func =
    makeAudioClockGetTimeFunc cFunc
    where cFunc :: CAudioClockGetTimeFunc
          cFunc cClock _ = do
            clock <- peekObject cClock
            liftM fromIntegral $ func clock
foreign import ccall unsafe "wrapper"
  makeAudioClockGetTimeFunc :: CAudioClockGetTimeFunc
                            -> IO {# type GstAudioClockGetTimeFunc #}

audioClockNew :: String
              -> AudioClockGetTimeFunc
              -> IO Clock
audioClockNew name func = do
  cFunc <- marshalAudioClockGetTimeFunc func
  withUTFString name $ \cName ->
      {# call audio_clock_new #} cName
                                 cFunc
                                 nullPtr >>=
          takeObject
