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
  --   The 'binElementAdded' signal is fired whenever a new element is
  --   added to the bin. Likewise the 'binElementRemoved' signal is
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
  binElementAdded,
  binElementRemoved
  
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

-- | Create a new 'Bin' with the given name.
binNew :: String -- ^ @name@ - the name to give the new 'Bin'
       -> IO Bin -- ^ the new 'Bin'
binNew name =
    withUTFString name {# call bin_new #} >>= takeObject . castPtr

-- | Add @element@ to @bin@, and set @element@'s parent to
--   @bin@. An 'Element' can only be added to one 'Bin' at a time.
--   
--   If any of @element@'s pads are linked to other 'Pad's, they will be
--   unlinked before @element@ is added to @bin@.
binAdd :: (BinClass bin, ElementClass element)
       => bin     -- ^ @bin@ - a 'Bin'
       -> element -- ^ @element@ - the element to add
       -> IO Bool -- ^ 'True' if the element could be added, 'False'
                  --   if the bin does not want to accept the element
binAdd bin element =
    liftM toBool $ {# call bin_add #} (toBin bin) (toElement element)

-- | Remove @element@ from @bin@, unparenting it as well.
--   
--   If any @element@'s pads are linked to other pads, they will be
--   unlinked before @element@ is added to @bin@.
binRemove :: (BinClass bin, ElementClass element)
          => bin     -- ^ @bin@ - a 'Bin'
          -> element -- ^ @element@ - the element to remove
          -> IO Bool -- ^ 'True' if @element@ could be removed, otherwise 'False'
binRemove bin element =
    liftM toBool $ {# call bin_remove #} (toBin bin) (toElement element)

-- | Get the 'Element' with the given name @name@ from @bin@,
--   recursing down through @bin@'s children. 'Nothing' is returned if no
--   'Element' with the given name is found.
binGetByName :: BinClass bin
             => bin                -- ^ @bin@ - a 'Bin'
             -> String             -- ^ @name@ - the name to search for
             -> IO (Maybe Element) -- ^ the 'Element' with the name @name@, or 'Nothing'
binGetByName bin name =
    withUTFString name ({# call bin_get_by_name #} (toBin bin)) >>= maybePeek takeObject

-- | Get the 'Element' with the given name @name@ from @bin@,
--   recursing up through @bin@'s parents. Returns 'Nothing' if no
--   element with the given name is found.
binGetByNameRecurseUp :: BinClass bin
                      => bin                -- ^ @bin@ - a 'Bin'
                      -> String             -- ^ @element@ - the name to search for
                      -> IO (Maybe Element) -- ^ the 'Element' with the given name, or 'Nothing'
binGetByNameRecurseUp bin name =
    withUTFString name ({# call bin_get_by_name_recurse_up #} $ toBin bin) >>=
        maybePeek takeObject

-- | Find an 'Element' inside @bin@ that implements the interface
--   given by @iface@. The returned 'Element' can be casted to
--   @iface@'s type. If you want all the 'Element's that implement an
--   interface, use 'binIterateAllByInterface'.
--   
--   This function recurses into child bins.
binGetByInterface :: BinClass bin
                  => bin                -- ^ @bin@ - a 'Bin'
                  -> GType              -- ^ @iface@ - the type of the requested interface
                  -> IO (Maybe Element) -- ^ the 'Element' inside @bin@ that implements @iface@, or 'Nothing'
binGetByInterface bin iface =
    {# call bin_get_by_interface #} (toBin bin) (fromIntegral iface) >>=
        maybePeek takeObject

-- | Get an 'Iterator' over the 'Element's in @bin@.
binIterateElements :: BinClass bin
                   => bin                           -- ^ @bin@ - a 'Bin'
                   -> IO (Maybe (Iterator Element)) -- ^ an 'Iterator' over the 'Element's in @bin@,
                                                    --   or 'Nothing'
binIterateElements bin =
    {# call bin_iterate_elements #} (toBin bin) >>=
        maybePeek takeIterator

-- | Get an 'Iterator' over the 'Element's in @bin@. This
--   iterator recurses into @bin@'s children.
binIterateRecurse :: BinClass bin
                  => bin                           -- ^ @bin@ - a 'Bin'
                  -> IO (Maybe (Iterator Element)) -- ^ an 'Iterator' over the 'Element's in @bin@
                                                   --   and its descendents, or 'Nothing'
binIterateRecurse bin =
    {# call bin_iterate_recurse #} (toBin bin) >>=
        maybePeek takeIterator

-- | Get an iterator over the 'Element's in @bin@ that have the
--   'ElementIsSink' flag set.
binIterateSinks :: BinClass bin
                => bin                           -- ^ @bin@ - a 'Bin'
                -> IO (Maybe (Iterator Element)) -- ^ an 'Iterator' over the sinks in @bin@, or 'Nothing'
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
                 => bin                           -- ^ @bin@ - a 'Bin'
                 -> IO (Maybe (Iterator Element)) -- ^ an 'Iterator' over the 'Element's in @bin@, or 'Nothing'
binIterateSorted bin =
    {# call bin_iterate_sorted #} (toBin bin) >>=
        maybePeek takeIterator

-- | Gets an iterator for all elements in the bin that have no sink
--   pads and have the 'ElementIsSink' flag unset.
binIterateSources :: BinClass bin
                  => bin                           -- ^ @bin@ - a 'Bin'
                  -> IO (Maybe (Iterator Element)) -- ^ an 'Iterator' on elements, or 'Nothing'
binIterateSources bin =
    {# call bin_iterate_sources #} (toBin bin) >>=
        maybePeek takeIterator

-- | Looks for all elements inside the bin that implement the given
--   interface. You can safely case all elements to the given
--   interface. The function recurses inside child bins.
binIterateAllByInterface :: BinClass bin
                         => bin                           -- ^ @bin@ - a 'Bin'
                         -> GType                         -- ^ @iface@ - the interface's 'GType'
                         -> IO (Maybe (Iterator Element)) -- ^ an 'Iterator' on elements, or 'Nothing'
binIterateAllByInterface bin iface =
    {# call bin_iterate_all_by_interface #} (toBin bin) (fromIntegral iface) >>=
        maybePeek takeIterator

#if GSTREAMER_CHECK_VERSION (0, 10, 3)
-- | Recursively looks for elements with an unconnected pad of the
--   given direction within the specified bin. Returns an unconnected
--   pad if one is found, otherwise 'Nothing'.
binFindUnconnectedPad :: BinClass bin
                      => bin            -- ^ @bin@ - a 'Bin'
                      -> PadDirection   -- ^ @direction@ - the direction of the requested 'Pad'
                      -> IO (Maybe Pad) -- ^ an unconnected 'Pad', or 'Nothing'
binFindUnconnectedPad bin direction =
    {# call bin_find_unconnected_pad #} (toBin bin) (fromIntegral $ fromEnum direction) >>=
        maybePeek takeObject
#endif

-- | An 'Element' has been added to the 'Bin'.
binElementAdded :: BinClass bin
                => Signal bin (Element -> IO ())
binElementAdded =
    Signal $ connect_OBJECT__NONE "element-added"

-- | An 'Element' has been removed from the 'Bin'.
binElementRemoved :: BinClass bin
                  => Signal bin (Element -> IO ())
binElementRemoved =
    Signal $ connect_OBJECT__NONE "element-added"
