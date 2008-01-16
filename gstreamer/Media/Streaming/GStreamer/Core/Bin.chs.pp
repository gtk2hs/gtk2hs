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
--  |
--  Maintainer  : gtk2hs-devel@lists.sourceforge.net
--  Stability   : alpha
--  Portability : portable (depends on GHC)
--  
--  Base class for elements that can contain other elements.
module Media.Streaming.GStreamer.Core.Bin (

-- * Detail
  -- | 'Bin' is an element that can contain other 'Element's, allowing
  --   them to be managed as a group. 'Pad's from the child elements
  --   can be ghosted to the bin; see 'GhostPad'. This makes the bin
  --   look like any other element, and enables creation of
  --   higher-level abstractions.
  --   
  --   A new 'Bin' is created with 'binNew'. Use a 'Pipeline' instead
  --   if you want to create a toplevel bin, because a normal bin
  --   doesn't have a bus or handle clock distribution of its own.
  --   
  --   After the bin has been created you will typically add elements
  --   to it with 'binAdd'. You can remove elements with 'binRemove'.
  --   
  --   An element can be retrieved from a bin with 'binGetByName',
  --   using the element's name. 'binGetByNameRecurseUp' is mainly
  --   used for internal purposes and will query the parent bins when
  --   the element is not found in the current bin.
  --   
  --   An iterator of elements in a bin can be retrieved with
  --   'binIterateElements'. Various other iterators exist to retrieve
  --   the elements in a bin.
  --   
  --   The 'onBinElementAdded' signal is fired whenever a new element is
  --   added to the bin. Likewise the 'onBinElementRemoved' signal is
  --   fired whenever an element is removed from the bin.

-- * Types
  Bin,
  BinClass,
  -- Safely downcast an 'Object' to a 'Bin'.
  castToBin,
  -- Upcast to a 'Bin'.
  toBin,
  -- See if an 'Object' is a 'Bin'.
  isBin,

-- * Bin Operations
  binNew,
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
  
-- * Bin Signals
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

-- | Creates a new 'Bin' with the given name.
binNew :: String -- ^ the name of the new bin
       -> IO Bin -- ^ the new bin
binNew name =
    withUTFString name {# call bin_new #} >>= takeObject . castPtr

-- | Adds the given 'Element' to the 'Bin'. Sets the element's parent. An
--   element can only be added to one bin.
--   
--   If the element's pads are linked to other pads, the pads will be
--   unlinked before the element is added to the bin.
binAdd :: (BinClass bin, ElementClass element)
       => bin     -- ^ a bin
       -> element -- ^ the element to be added
       -> IO Bool -- ^ 'True' if the element could be added, 'False'
                  --   if the bin does not want to accept the element
binAdd bin element =
    liftM toBool $ {# call bin_add #} (toBin bin) (toElement element)

-- | Removes the given 'Element' from the 'Bin', unparenting it as well.
--   
--   If the element's pads are linked to other pads, the pads will be
--   unlinked before the element is removed from the bin.
binRemove :: (BinClass bin, ElementClass element)
          => bin     -- ^ a bin
          -> element -- ^ the element to remove
          -> IO Bool -- ^ 'True' if the element could be removed, 'False'
                     --   if the bin does not want to remove the element
binRemove bin element =
    liftM toBool $ {# call bin_remove #} (toBin bin) (toElement element)

-- | Gets the 'Element' with the given name from a 'Bin'. This
--   function recurses into child bins. Returns 'Nothing' if no element
--   with the given name is found in the bin.
binGetByName :: BinClass bin
             => bin                -- ^ a bin
             -> String             -- ^ the element name to search for
             -> IO (Maybe Element) -- ^ the element with the given name, or 'Nothing'
binGetByName bin name =
    withUTFString name ({# call bin_get_by_name #} (toBin bin)) >>= maybePeek takeObject

-- | Gets the element with the given name from this bin. If the
--   element is not found, a recursion is performed on the parent
--   bin. Returns 'Nothing' if no element with the given name is
--   found.
binGetByNameRecurseUp :: BinClass bin
                      => bin                -- ^ a bin
                      -> String             -- ^ the element name to search for
                      -> IO (Maybe Element) -- ^ the element with the given name, or 'Nothing'
binGetByNameRecurseUp bin name =
    withUTFString name ({# call bin_get_by_name_recurse_up #} $ toBin bin) >>=
        maybePeek takeObject

-- | Looks for an element inside the bin that implements the given
--   interface. If such an element is found, it returns the
--   element. You can case this element to the given interface
--   afterwards. If you want all the elements that implement an
--   interface, use 'binIterateAllByInterface'.
--   
--   This function recurses into child bins.
binGetByInterface :: BinClass bin
                  => bin                -- ^ a bin
                  -> GType              -- ^ the interface's type
                  -> IO (Maybe Element) -- ^ an element inside the bin implementing the interface, or 'Nothing'
binGetByInterface bin iface =
    {# call bin_get_by_interface #} (toBin bin) (fromIntegral iface) >>=
        maybePeek takeObject

-- | Gets an 'Iterator' for the 'Element's in this 'Bin'.
binIterateElements :: BinClass bin
                   => bin                           -- ^ a bin
                   -> IO (Maybe (Iterator Element)) -- ^ an iterator on elements, or 'Nothing'
binIterateElements bin =
    {# call bin_iterate_elements #} (toBin bin) >>=
        maybePeek takeIterator

-- | Gets an 'Iterator' for the 'Element's in this 'Bin'. This
--   iterator recurses into the bin's children.
binIterateRecurse :: BinClass bin
                  => bin                           -- ^ a bin
                  -> IO (Maybe (Iterator Element)) -- ^ an iterator on elements, or 'Nothing'
binIterateRecurse bin =
    {# call bin_iterate_recurse #} (toBin bin) >>=
        maybePeek takeIterator

-- | Gets an iterator for all elements in the bin that have the
--   'ElementIsSink' flag set.
binIterateSinks :: BinClass bin
                => bin                           -- ^ a bin
                -> IO (Maybe (Iterator Element)) -- ^ an iterator on elements, or 'Nothing'
binIterateSinks bin =
    {# call bin_iterate_sinks #} (toBin bin) >>=
        maybePeek takeIterator

-- | Gets an iterator for the elements in this bin in topologically
--   sorted order. This means that the elements are returned from the
--   most downstream elements (sinks) to the sources.
--   
--   This function is used internally to perform state changes of the
--   bin elements.
binIterateSorted :: BinClass bin
                 => bin                           -- ^ a bin
                 -> IO (Maybe (Iterator Element)) -- ^ an iterator on elements, or 'Nothing'
binIterateSorted bin =
    {# call bin_iterate_sorted #} (toBin bin) >>=
        maybePeek takeIterator

-- | Gets an iterator for all elements in the bin that have no sink
--   pads and have the 'ElementIsSink' flag unset.
binIterateSources :: BinClass bin
                  => bin                           -- ^ a bin
                  -> IO (Maybe (Iterator Element)) -- ^ an iterator on elements, or 'Nothing'
binIterateSources bin =
    {# call bin_iterate_sources #} (toBin bin) >>=
        maybePeek takeIterator

-- | Looks for all elements inside the bin that implement the given
--   interface. You can safely case all elements to the given
--   interface. The function recurses inside child bins.
binIterateAllByInterface :: BinClass bin
                         => bin                           -- ^ a bin
                         -> GType                         -- ^ the interface's type
                         -> IO (Maybe (Iterator Element)) -- ^ an iterator on elements, or 'Nothing'
binIterateAllByInterface bin iface =
    {# call bin_iterate_all_by_interface #} (toBin bin) (fromIntegral iface) >>=
        maybePeek takeIterator

#if GSTREAMER_CHECK_VERSION (0, 10, 3)
-- | Recursively looks for elements with an unconnected pad of the
--   given direction within the specified bin. Returns an unconnected
--   pad if one is found, otherwise 'Nothing'.
binFindUnconnectedPad :: BinClass bin
                      => bin            -- ^ a bin
                      -> PadDirection   -- ^ whether to look for a source pad or a sink pad
                      -> IO (Maybe Pad) -- ^ an unconnected pad or 'Nothing'
binFindUnconnectedPad bin direction =
    {# call bin_find_unconnected_pad #} (toBin bin) (fromIntegral $ fromEnum direction) >>=
        maybePeek takeObject
#endif


-- | Handle the signal emitted when an 'Element' is added to a 'Bin'.
onBinElementAdded, afterBinElementAdded :: BinClass bin
                                        => bin                -- ^ a bin
                                        -> (Element -> IO ()) -- ^ the signal handler
                                        -> IO (ConnectId bin) -- ^ the connection id
onBinElementAdded =
    connect_OBJECT__NONE "element-added" False
afterBinElementAdded =
    connect_OBJECT__NONE "element-added" True

-- | Handle the signal emitted when an 'Element' is removed from a 'Bin'.
onBinElementRemoved, afterBinElementRemoved :: BinClass bin
                                            => bin                -- ^ a bin
                                            -> (Element -> IO ()) -- ^ the signal handler
                                            -> IO (ConnectId bin) -- ^ the connection id
onBinElementRemoved =
    connect_OBJECT__NONE "element-removed" False
afterBinElementRemoved =
    connect_OBJECT__NONE "element-removed" True
