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
module Media.Streaming.GStreamer.Core.PadTemplate (
  
  PadTemplate,
  PadTemplateClass,
  castToPadTemplate,
  gTypePadTemplate,

  PadPresence(..),
  
  padTemplateNew,
  padTemplateGetCaps,
  padTemplateGetNameTemplate,
  padTemplateGetDirection,
  padTemplateGetPresence,
  
  onPadTemplatePadCreated,
  afterPadTemplatePadCreated
  
  ) where

import Control.Monad (liftM)
{#import Media.Streaming.GStreamer.Core.Types#}
{#import Media.Streaming.GStreamer.Core.Signals#}
import System.Glib.FFI
#if __GLASGOW_HASKELL__ < 606
    hiding ( withObject )
#endif
import System.Glib.UTFString
import System.Glib.Signals

{# context lib = "gstreamer" prefix = "gst" #}

padTemplateNew :: String
               -> PadDirection
               -> PadPresence
               -> Caps
               -> IO PadTemplate
padTemplateNew nameTemplate direction presence caps =
    withUTFString nameTemplate $ \cNameTemplate ->
    giveCaps caps $ \caps' ->
        {# call pad_template_new #} cNameTemplate
                                    (fromIntegral $ fromEnum direction)
                                    (fromIntegral $ fromEnum presence)
                                    caps' >>=
            takeObject

padTemplateGetCaps :: PadTemplateClass padTemplate
                   => padTemplate
                   -> IO Caps
padTemplateGetCaps padTemplate =
    {# call pad_template_get_caps #} (toPadTemplate padTemplate) >>=
        peekCaps

padTemplateGetNameTemplate :: PadTemplateClass padTemplate
                           => padTemplate
                           -> IO String
padTemplateGetNameTemplate padTemplate =
    withObject (toPadTemplate padTemplate) {# get PadTemplate->name_template #} >>=
        peekUTFString

padTemplateGetDirection :: PadTemplateClass padTemplate
                        => padTemplate
                        -> IO PadDirection
padTemplateGetDirection padTemplate =
    liftM (toEnum . fromIntegral) $
        withObject (toPadTemplate padTemplate) {# get PadTemplate->direction #}

padTemplateGetPresence :: PadTemplateClass padTemplate
                       => padTemplate
                       -> IO PadPresence
padTemplateGetPresence padTemplate =
    liftM (toEnum . fromIntegral) $
        withObject (toPadTemplate padTemplate) {# get PadTemplate->presence #}

onPadTemplatePadCreated, afterPadTemplatePadCreated :: PadTemplateClass padTemplateT
                                                    => padTemplateT
                                                    -> (Pad -> IO ())
                                                    -> IO (ConnectId padTemplateT)
onPadTemplatePadCreated =
    connect_OBJECT__NONE "pad-created" False
afterPadTemplatePadCreated =
    connect_OBJECT__NONE "pad-created" True
