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
module Media.Streaming.GStreamer.Core.Bin (
  
  Bin,
  BinClass,
  castToBin,
  toBin,
  binAdd,
  binRemove,
  binGetByName,
  binGetByNameRecurseUp,
  binGetByInterface,
  binIterateElements,
  binIterateRecurse,
  binIterateSinks,
  binIterateSorted,
  binIterateSources,
  binIterateAllByInterface,
  binFindUnconnectedPad,
  
  onBinElementAdded,
  afterBinElementAdded,
  onBinElementRemoved,
  afterBinElementRemoved
  
  ) where

import Control.Exception                      ( bracket_ )
import Control.Monad                          ( liftM )
import System.Glib.FFI
{#import System.Glib.GType#}                  ( GType )
import System.Glib.UTFString                  ( withUTFString )
import System.Glib.GList                      ( GList
                                              , readGList )
{#import Media.Streaming.GStreamer.Core.Types#}
{#import Media.Streaming.GStreamer.Core.Signals#}

{# context lib = "gstreamer" prefix = "gst" #}

binAdd :: (BinClass bin,
           ElementClass element) =>
          bin
       -> element
       -> IO Bool
binAdd bin element =
    liftM toBool $ {# call bin_add #} (toBin bin) (toElement element)

binRemove :: (BinClass bin,
              ElementClass element) =>
             bin
          -> element
          -> IO Bool
binRemove bin element =
    liftM toBool $ {# call bin_remove #} (toBin bin) (toElement element)

binGetByName :: BinClass bin =>
                bin
             -> String
             -> IO (Maybe Element)
binGetByName bin name =
    withUTFString name ({# call bin_get_by_name #} (toBin bin)) >>= maybePeek takeElement

binGetByNameRecurseUp :: BinClass bin =>
                bin
             -> String
             -> IO (Maybe Element)
binGetByNameRecurseUp bin name =
    withUTFString name ({# call bin_get_by_name_recurse_up #} $ toBin bin) >>=
        maybePeek takeElement

binGetByInterface :: BinClass bin =>
                     bin
                  -> GType
                  -> IO (Maybe Element)
binGetByInterface bin iface =
    {# call bin_get_by_interface #} (toBin bin) (fromIntegral iface) >>=
        maybePeek takeElement

binIterateElements :: BinClass bin =>
                      bin
                   -> IO (Maybe (Iterator Element))
binIterateElements bin =
    {# call bin_iterate_elements #} (toBin bin) >>=
        maybePeek takeIterator

binIterateRecurse :: BinClass bin =>
                     bin
                  -> IO (Maybe (Iterator Element))
binIterateRecurse bin =
    {# call bin_iterate_recurse #} (toBin bin) >>=
        maybePeek takeIterator

binIterateSinks :: BinClass bin =>
                   bin
                -> IO (Maybe (Iterator Element))
binIterateSinks bin =
    {# call bin_iterate_sinks #} (toBin bin) >>=
        maybePeek takeIterator

binIterateSorted :: BinClass bin =>
                     bin
                  -> IO (Maybe (Iterator Element))
binIterateSorted bin =
    {# call bin_iterate_sorted #} (toBin bin) >>=
        maybePeek takeIterator

binIterateSources :: BinClass bin =>
                     bin
                  -> IO (Maybe (Iterator Element))
binIterateSources bin =
    {# call bin_iterate_sources #} (toBin bin) >>=
        maybePeek takeIterator

binIterateAllByInterface :: BinClass bin =>
                            bin
                         -> GType
                         -> IO (Maybe (Iterator Element))
binIterateAllByInterface bin iface =
    {# call bin_iterate_all_by_interface #} (toBin bin) (fromIntegral iface) >>=
        maybePeek takeIterator

binFindUnconnectedPad :: BinClass bin =>
                         bin
                      -> PadDirection
                      -> IO (Maybe Pad)
binFindUnconnectedPad bin direction =
    {# call bin_find_unconnected_pad #} (toBin bin) (fromIntegral $ fromEnum direction) >>=
        maybePeek takePad

onBinElementAdded, afterBinElementAdded :: BinClass bin
                                        => bin
                                        -> (Element -> IO ())
                                        -> IO (ConnectId bin)
onBinElementAdded =
    connect_OBJECT__NONE "element-added" False
afterBinElementAdded =
    connect_OBJECT__NONE "element-added" True

onBinElementRemoved, afterBinElementRemoved :: BinClass bin
                                            => bin
                                            -> (Element -> IO ())
                                            -> IO (ConnectId bin)
onBinElementRemoved =
    connect_OBJECT__NONE "element-removed" False
afterBinElementRemoved =
    connect_OBJECT__NONE "element-removed" True
