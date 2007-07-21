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
module Media.Streaming.GStreamer.Core.GhostPad (
  
  GhostPad,
  GhostPadClass,
  castToGhostPad,
  toGhostPad,
  fromGhostPad,
  ghostPadNew,
  ghostPadNewNoTarget,
  ghostPadNewFromTemplate,
  ghostPadNewNoTargetFromTemplate,
  ghostPadSetTarget,
  ghostPadGetTarget
  
  ) where

import Control.Monad (liftM)
import System.Glib.FFI
import System.Glib.UTFString
{#import Media.Streaming.GStreamer.Core.Types#}

{# context lib = "gstreamer" prefix = "gst" #}

ghostPadNew :: PadClass pad
            => String
            -> pad
            -> IO (Maybe Pad)
ghostPadNew name target =
    (withUTFString name $
         flip {# call ghost_pad_new #} $ toPad target) >>=
        maybePeek newPad

ghostPadNewNoTarget :: String
                    -> PadDirection
                    -> IO (Maybe Pad)
ghostPadNewNoTarget name dir =
    (withUTFString name $
         flip {# call ghost_pad_new_no_target #} $ fromPadDirection dir) >>=
        maybePeek newPad

ghostPadNewFromTemplate :: String
                        -> Pad
                        -> PadTemplate
                        -> IO (Maybe Pad)
ghostPadNewFromTemplate name target templ =
    withUTFString name $ \cName ->
        {# call ghost_pad_new_from_template #} cName target templ >>=
            maybePeek newPad

ghostPadNewNoTargetFromTemplate :: String
                                -> PadTemplate
                                -> IO (Maybe Pad)
ghostPadNewNoTargetFromTemplate name templ =
    withUTFString name $ \cName ->
        {# call ghost_pad_new_no_target_from_template #} cName templ >>=
            maybePeek newPad

ghostPadSetTarget :: GhostPad
                  -> Pad
                  -> IO Bool
ghostPadSetTarget gpad newtarget =
    liftM toBool $ {# call ghost_pad_set_target #} gpad newtarget

ghostPadGetTarget :: GhostPad
                  -> IO Pad
ghostPadGetTarget gpad =
    {# call ghost_pad_get_target #} gpad >>= newPad
