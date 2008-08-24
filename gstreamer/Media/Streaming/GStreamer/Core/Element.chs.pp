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
--  Abstract class of pipeline elements.
module Media.Streaming.GStreamer.Core.Element (

-- * Detail

  -- | 'Element' is the abstract base class needed to construct an
  --   element that can be used in a GStreamer pipeline.
  --   
  --   All elements have pads (of the type 'Pad'). These pads link to
  --   pads on other elements. 'Buffer's flow between these linked
  --   pads. An 'Element' has a 'Pad' for each input (or sink) and
  --   output (or source).
  --   
  --   An element's pad can be retrieved by name with
  --   'elementGetStaticPad' or 'elementGetRequestPad'. An 'Iterator'
  --   over all an element's pads can be retrieved with
  --   'elementIteratePads'.
  --   
  --   Elements can be linked through their pads. If the link is
  --   straightforward, use the 'elementLink' convenience function to
  --   link two elements. Use 'elementLinkFiltered' to link two
  --   elements constrained by a specified set of 'Caps'. For finer
  --   control, use 'elementLinkPads' and 'elementLinkPadsFiltered' to
  --   specify the pads to link on each element by name.
  --   
  --   Each element has a 'State'. You can get and set the state of an
  --   element with 'elementGetState' and 'elementSetState'. To get a
  --   string representation of a 'State', use 'elementStateGetName'.
  --   
  --   You can get and set a 'Clock' on an element using
  --   'elementGetClock' and 'elementSetClock'. Some elements can
  --   provide a clock for the pipeline if 'elementProvidesClock'
  --   returns 'True'. With the 'elementProvideClock' method one can
  --   retrieve the clock provided by such an element. Not all
  --   elements require a clock to operate correctly. If
  --   'elementRequiresClock' returns 'True', a clock should be set on
  --   the element with 'elementSetClock'.
  --   
  --   Note that clock slection and distribution is normally handled
  --   by the toplevel 'Pipeline' so the clock functions should only
  --   be used in very specific situations.

-- * Types  
  Element,
  ElementClass,
  castToElement,
  toElement,
  ElementFlags(..),
  State(..),
  StateChange(..),
  StateChangeReturn(..),

-- * Element Operations
  elementAddPad,
  elementGetCompatiblePad,
  elementGetCompatiblePadTemplate,
  elementGetRequestPad,
  elementGetStaticPad,
  elementReleaseRequestPad,
  elementRemovePad,
  elementIteratePads,
  elementIterateSinkPads,
  elementIterateSrcPads,
  elementLink,
  elementUnlink,
  elementLinkPads,
  elementUnlinkPads,
  elementLinkPadsFiltered,
  elementLinkFiltered,
  elementSetBaseTime,
  elementGetBaseTime,
  elementSetBus,
  elementGetBus,
  elementGetFactory,
  elementSetIndex,
  elementIsIndexable,
  elementRequiresClock,
  elementSetClock,
  elementGetClock,
  elementProvidesClock,
  elementProvideClock,
  elementSetState,
  elementGetState,
  elementSetLockedState,
  elementIsLockedState,
  elementAbortState,
  elementStateGetName,
#if GSTREAMER_CHECK_VERSION(0,10,11)
  elementStateChangeReturnGetName,
#endif
  elementSyncStateWithParent,
  elementGetQueryTypes,
  elementQuery,
  elementQueryConvert,
  elementQueryPosition,
  elementQueryDuration,
  elementSendEvent, 
#if GSTREAMER_CHECK_VERSION(0,10,7)
  elementSeekSimple,
#endif
  elementSeek,
  elementNoMorePads,
  elementPadAdded,
  elementPadRemoved,
  
  ) where

import Control.Monad         ( liftM )
import Data.Maybe            ( fromMaybe )
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.UTFString ( withUTFString
                             , peekUTFString )
{#import System.Glib.Signals#}
{#import System.Glib.GObject#}
{#import Media.Streaming.GStreamer.Core.Types#}
{#import Media.Streaming.GStreamer.Core.Signals#}

{# context lib = "gstreamer" prefix = "gst" #}

-- | Get the flags set on the element.
elementGetFlags :: ElementClass elementT
                => elementT          -- ^ @element@ - an element
                -> IO [ElementFlags]
elementGetFlags = mkObjectGetFlags

-- | Set the given flags on the element.
elementSetFlags :: ElementClass elementT
                => elementT
                -> [ElementFlags]
                -> IO ()
elementSetFlags = mkObjectSetFlags

-- | Unset the given flags on the element.
elementUnsetFlags :: ElementClass elementT
                  => elementT
                  -> [ElementFlags]
                  -> IO ()
elementUnsetFlags = mkObjectUnsetFlags

-- | Add a pad (link point) to an element. The pad's parent will be set to
--   @element@.
--   
--   Pads are not automatically activated so elements should perform
--   the needed steps to activate the pad in case this pad is added in
--   the 'StatePaused' or 'StatePlaying' state. See 'padSetActive' for
--   more information about activating pads.
--   
--   This function will emit the 'elementPadAdded' signal on the
--   element.
elementAddPad :: (ElementClass elementT, PadClass padT)
              => elementT -- ^ @element@ - an element
              -> padT     -- ^ @pad@ - 
              -> IO Bool
elementAddPad element pad =
    liftM toBool $ {# call element_add_pad #} (toElement element) (toPad pad)

-- | Look for an unlinked pad to which the @pad@ can link. It is not
--   guaranteed that linking the pads will work, though it should work in most
--   cases.
elementGetCompatiblePad :: (ElementClass elementT, PadClass padT)
                        => elementT       -- ^ @element@ - an element
                        -> padT           -- ^ @pad@ - a pad
                        -> Caps           -- ^ @caps@ - the 'Caps' to
                                          --   use as a filter
                        -> IO (Maybe Pad) -- ^ a 'Pad' that is compatible with @pad@, or
                                          --   'Nothing' if none was found
elementGetCompatiblePad element pad caps =
    {# call element_get_compatible_pad #} (toElement element) (toPad pad) caps >>=
        maybePeek takeObject

-- | Retrieve a pad template from @element@ that is compatible with
--   @padTemplate@.  Pads from compatible templates can be linked
--   together.
elementGetCompatiblePadTemplate :: (ElementClass elementT, PadTemplateClass padTemplateT)
                                => elementT               -- ^ @element@ - an element
                                -> padTemplateT           -- ^ @padTemplate@ - a pad template
                                -> IO (Maybe PadTemplate) -- ^ the compatible 'PadTemplate',
                                                          --   or 'Nothing' if none was found
elementGetCompatiblePadTemplate element padTemplate =
    {# call element_get_compatible_pad_template #} (toElement element) (toPadTemplate padTemplate) >>=
        maybePeek takeObject

-- | Retrieve a pad from the element by name. This version only
--   retrieves request pads. The pad should be released with
--   'elementReleaseRequestPad'.
elementGetRequestPad :: ElementClass elementT
                     => elementT       -- ^ @element@ - an element
                     -> String         -- ^ @name@ - 
                     -> IO (Maybe Pad) -- ^ the requested 'Pad' if
                                       --   found, otherwise 'Nothing'.
elementGetRequestPad element name =
    (withUTFString name $ {# call element_get_request_pad #} (toElement element)) >>=
        maybePeek peekObject

-- | Retreive a pad from @element@ by name. This version only
--   retrieves already-existing (i.e. "static") pads.
elementGetStaticPad :: ElementClass elementT
                    => elementT -- ^ @element@ - an element
                    -> String   -- ^ @name@ - 
                    -> IO (Maybe Pad) -- ^ the requested 'Pad' if
                                      --   found, otherwise 'Nothing'.
elementGetStaticPad element name =
    (withUTFString name $ {# call element_get_static_pad #} (toElement element)) >>=
        maybePeek takeObject

-- | Release a request pad that was previously obtained with
--   'elementGetRequestPad'.
elementReleaseRequestPad :: (ElementClass elementT, PadClass padT)
                         => elementT -- ^ @element@ - 
                         -> padT     -- ^ @pad@ - 
                         -> IO ()
elementReleaseRequestPad element pad =
    {# call element_release_request_pad #} (toElement element) (toPad pad)

-- | Remove @pad@ from @element@.
--   
--   This function is used by plugin developers and should not be used
--   by applications. Pads that were dynamically requested from
--   elements with 'elementGetRequestPad' should be released with the
--   'elementReleaseRequestPad' function instead.
--   
--   Pads are not automatically deactivated so elements should perform the needed
--   steps to deactivate the pad in case this pad is removed in the PAUSED or
--   PLAYING state. See 'padSetActive' for more information about
--   deactivating pads.
--   
--   The pad and the element should be unlocked when calling this function.
--   
--   This function will emit the 'padRemoved' signal on the element.
--   
--   Returns: 'True' if the pad could be removed. Can return 'False' if the
--   pad does not belong to the provided element.
elementRemovePad :: (ElementClass elementT, PadClass padT)
                 => elementT -- ^ @element@ - 
                 -> padT     -- ^ @pad@ - 
                 -> IO Bool  -- ^ 'True' if the pad was succcessfully
                             --   removed, otherwise 'False'
elementRemovePad element pad =
    liftM toBool $ {# call element_remove_pad #} (toElement element) (toPad pad)

-- | Retrieve an 'Iterator' over @element@'s pads.
elementIteratePads :: (ElementClass elementT)
                   => elementT          -- ^ @element@ - 
                   -> IO (Iterator Pad) -- ^ an iterator over the element's pads.
elementIteratePads element =
    {# call element_iterate_pads #} (toElement element) >>= takeIterator


-- | Retrieve an 'Iterator' over @element@'s sink pads.
elementIterateSinkPads :: (ElementClass elementT)
                       => elementT
                       -> IO (Iterator Pad)
elementIterateSinkPads element =
    {# call element_iterate_sink_pads #} (toElement element) >>= takeIterator

-- | Retrieve an 'Iterator' over @element@'s src pads.
elementIterateSrcPads :: (ElementClass elementT)
                      => elementT
                      -> IO (Iterator Pad)
elementIterateSrcPads element =
    {# call element_iterate_sink_pads #} (toElement element) >>= takeIterator

-- | Link @src@ to @sink@. The link must be from source to
--   sink; the other direction will not be tried. The function
--   looks for existing pads that aren't linked yet. It will request
--   new pads if necessary. Such pads must be released manually (with
--   'elementReleaseRequestPad') when unlinking. If multiple links are
--   possible, only one is established.
--   
--   Make sure you have added your elements to a 'Bin' or 'Pipeline'
--   with 'binAdd' before trying to link them.
elementLink :: (ElementClass srcT, ElementClass sinkT)
            => srcT    -- ^ @src@ - 
            -> sinkT   -- ^ @sink@ - 
            -> IO Bool -- ^ 'True' if the pads could be linked,
                       --   otherwise 'False'
elementLink src sink =
    liftM toBool $ {# call element_link #} (toElement src) (toElement sink)

-- | Unlink all source pads of the @src@ from all sink pads of the
--   @sink@.
elementUnlink :: (ElementClass srcT, ElementClass sinkT)
              => srcT
              -> sinkT
              -> IO ()
elementUnlink src sink =
    {# call element_unlink #} (toElement src) (toElement sink)

-- | Link the named pads of @src@ and @sink@.
elementLinkPads :: (ElementClass srcT, ElementClass sinkT)
                => srcT         -- ^ @src@ - the element containing the source pad
                -> Maybe String -- ^ @srcPadName@ - the name of the source pad, or 'Nothing' for any pad
                -> sinkT        -- ^ @sink@ - the element containing the sink pad
                -> Maybe String -- ^ @sinkPadName@ - the name of the sink pad, or 'Nothing' for any pad
                -> IO Bool      -- ^ 'True' if the pads could be linked, otherwise 'False'
elementLinkPads src srcPadName sink sinkPadName =
    maybeWith withUTFString sinkPadName $ \cSinkPadName ->
        maybeWith withUTFString srcPadName $ \cSrcPadName ->
            liftM toBool $ {# call element_link_pads #} (toElement src) cSrcPadName (toElement sink) cSinkPadName

-- | Unlink the named pads of @src@ and @sink@.
elementUnlinkPads :: (ElementClass srcT, ElementClass sinkT)
                  => srcT   -- ^ @src@ - 
                  -> String -- ^ @srcPadName@ - 
                  -> sinkT  -- ^ @sink@ - 
                  -> String -- ^ @sinkPadName@ - 
                  -> IO ()
elementUnlinkPads src srcPadName sink sinkPadName =
    withUTFString sinkPadName $ \cSinkPadName ->
        withUTFString srcPadName $ \cSrcPadName ->
            {# call element_unlink_pads #} (toElement src) cSrcPadName (toElement sink) cSinkPadName

-- | Link the named pads of @src@ and @sink@. A side effect is that if
--   one of the pads has no parent, it becomes a child of the parent
--   of the other element. If they have different parents, the link
--   will fail. If @caps@ is not 'Nothing', make sure that the 'Caps'
--   of the link is a subset of @caps@.
elementLinkPadsFiltered :: (ElementClass srcT, ElementClass sinkT)
                        => srcT         -- ^ @src@ - 
                        -> Maybe String -- ^ @srcPadName@ - 
                        -> sinkT        -- ^ @sink@ - 
                        -> Maybe String -- ^ @sinkPadName@ - 
                        -> Caps         -- ^ @caps@ - 
                        -> IO Bool      -- ^ 'True' if the pads could be
                                        --   linked, otherwise 'False'
elementLinkPadsFiltered src srcPadName sink sinkPadName filter =
    maybeWith withUTFString sinkPadName $ \cSinkPadName ->
        maybeWith withUTFString srcPadName $ \cSrcPadName ->
            liftM toBool $ {# call element_link_pads_filtered #} (toElement src) cSrcPadName (toElement sink) cSinkPadName filter

-- | Link @src@ to @dest@ using the given 'Caps' as a filter. The link
--   must be from source to sink; the other direction will not be
--   tried. The function looks for existing pads that aren't linked
--   yet. If will request new pads if necessary. If multiple links are
--   possible, only one is established.
--   
--   Make sure you have added your elements to a 'Bin' or 'Pipeline'
--   with 'binAdd' before trying to link them.
elementLinkFiltered :: (ElementClass srcT, ElementClass sinkT)
                    => srcT       -- ^ @src@ - 
                    -> sinkT      -- ^ @sink@ - 
                    -> Maybe Caps -- ^ @caps@ - 
                    -> IO Bool    -- ^ 'True' if the pads could be
                                  --   linked, otherwise 'False'
elementLinkFiltered src sink filter =
    liftM toBool $
        {# call element_link_filtered #} (toElement src) (toElement sink) $
            fromMaybe (Caps nullForeignPtr) filter

-- | Set the base time of @element@. See 'elementGetBaseTime' for more
--   information.
elementSetBaseTime :: ElementClass elementT
                   => elementT      -- ^ @element@ - 
                   -> ClockTimeDiff -- ^ @time@ - 
                   -> IO ()
elementSetBaseTime element time =
    {# call element_set_base_time #} (toElement element) $ fromIntegral time

-- | Return the base time of @element@. The base time is the absolute
--   time of the clock when this element was last set to
--   'StatePlaying'. Subtract the base time from the clock time to get
--   the stream time of the element.
elementGetBaseTime :: ElementClass elementT
                   => elementT         -- ^ @element@ - 
                   -> IO ClockTimeDiff -- ^ the base time of the element
elementGetBaseTime element =
    liftM fromIntegral $ {# call element_get_base_time #} (toElement element)

-- | Set the 'Bus' used by @element@. For internal use only, unless
--   you're testing elements.
elementSetBus :: (ElementClass elementT, BusClass busT)
              => elementT -- ^ @element@ - 
              -> busT     -- ^ @bus@ - 
              -> IO ()
elementSetBus element bus =
    {# call element_set_bus #} (toElement element) (toBus bus)

-- | Get the bus of @element@. Not that only a 'Pipeline' will
--   provide a bus for the application.
elementGetBus :: ElementClass elementT
              => elementT -- ^ @element@ - 
              -> IO Bus   -- ^ the bus used by the element
elementGetBus element =
    {# call element_get_bus #} (toElement element) >>= takeObject

-- | Get the factory used to create @element@.
elementGetFactory :: ElementClass elementT
                  => elementT          -- ^ @element@ - 
                  -> IO ElementFactory -- ^ the factory that created @element@
elementGetFactory element =
    {# call element_get_factory #} (toElement element) >>= peekObject

-- | Set the 'Index' used by @element@.
elementSetIndex :: (ElementClass elementT, IndexClass indexT)
                => elementT -- ^ @element@ - 
                -> indexT   -- ^ @index@ - 
                -> IO ()
elementSetIndex element index =
    {# call element_set_index #} (toElement element) (toIndex index)

-- | Get the 'Index' used by @element@.
elementGetIndex :: ElementClass elementT
                => elementT         -- ^ @element@ - 
                -> IO (Maybe Index) -- ^ the index, or 'Nothing' if @element@ has none
elementGetIndex element =
    {# call element_get_index #} (toElement element) >>= maybePeek takeObject

-- | Determine whether @element@ can be indexed.
elementIsIndexable :: ElementClass elementT
                   => elementT -- ^ @element@ - 
                   -> IO Bool  -- ^ 'True' if the element can be indexed
elementIsIndexable element =
    liftM toBool $ {# call element_is_indexable #} (toElement element)

-- | Determine whether @element@ requires a clock.
elementRequiresClock :: ElementClass elementT
                     => elementT -- ^ @element@ - 
                     -> IO Bool  -- ^ 'True' if the element requires a clock
elementRequiresClock element =
    liftM toBool $ {# call element_requires_clock #} (toElement element)

-- | Set the 'Clock' used by @element@.
elementSetClock :: (ElementClass elementT, ClockClass clockT)
                => elementT -- ^ @element@ - 
                -> clockT   -- ^ @clock@ - 
                -> IO Bool  -- ^ 'True' if the element accepted the clock
elementSetClock element clock =
    liftM toBool $ {# call element_set_clock #} (toElement element) (toClock clock)

-- | Get the 'Clock' used by @element@.
elementGetClock :: ElementClass elementT
                => elementT         -- ^ @element@ - 
                -> IO (Maybe Clock) -- ^ the clock, or 'Nothing' if @element@ has none
elementGetClock element =
    {# call element_get_clock #} (toElement element) >>= maybePeek takeObject

-- | Determine whether @element@ provides a clock. A 'Clock' provided
--   by an element can be used as the global clock for a pipeline. An
--   element that can provide a clock is only required to do so in the
--   'StatePaused' state, meaning that it is fully negotiated and has
--   allocated the resources needed to operate the clock.
elementProvidesClock :: ElementClass elementT
                     => elementT -- ^ @element@ - 
                     -> IO Bool  -- ^ 'True' if the element provides a clock
elementProvidesClock element =
    liftM toBool $ {# call element_provides_clock #} $ toElement element

-- | Get the 'Clock' provided by @element@.
--   
--   Note that an element is only required to provide a clock in the
--   'StatePaused' state. Some elements can provide a clock in other
--   states.
elementProvideClock :: ElementClass elementT
                    => elementT         -- ^ @element@ - 
                    -> IO (Maybe Clock) -- ^ a 'Clock', or 'Nothing' if
                                        --   none could be provided
elementProvideClock element =
    {# call element_provide_clock #} (toElement element) >>= maybePeek takeObject

-- | Set the state of @element@ to @state@. This function will try to
--   set the requested state by going through all the intermediary
--   states and calling the class's state change function for each.
--   
--   This function can return 'StateChangeAsync', in which case the
--   element will perform the remainder of the state change
--   asynchronously in another thread. An application can use
--   'elementGetState' to wait for the completion of the state change
--   or it can wait for a state change message on the bus.
elementSetState :: ElementClass elementT
                => elementT             -- ^ @element@ - 
                -> State                -- ^ @state@ - 
                -> IO StateChangeReturn -- ^ the result of the state change
elementSetState element state =
    liftM (toEnum . fromIntegral) $ {# call element_set_state #} (toElement element) $
        fromIntegral $ fromEnum state

-- | Get the state of @element@.
--   
--   For elements that performed an asynchronous state change, as
--   reported by 'elementSetState', this function will block up to the
--   specified timeout value for the state change to complete. If the
--   element completes the state change or goes into an error, this
--   function returns immediately with a return value of
--   'StateChangeSuccess' or 'StateChangeFailure', respectively.
--   
--   This function returns 'StateChangeNoPreroll' if the element
--   successfully changed its state but is not able to provide data
--   yet. This mostly happens for live sources that not only produce
--   data in the 'StatePlaying' state. While the state change return
--   is equivalent to 'StateChangeSuccess', it is returned to the
--   application to signal that some sink elements might not be able
--   to somplete their state change because an element is not
--   producing data to complete the preroll. When setting the element
--   to playing, the preroll will complete and playback will start.
elementGetState :: ElementClass elementT
                => elementT  -- ^ @element@ - 
                -> ClockTime -- ^ @timeout@ - 
                -> IO (StateChangeReturn, Maybe State, Maybe State)
                   -- ^ the result of the state change, the current
                   --   state, and the pending state
elementGetState element timeout =
    alloca $ \statePtr ->
        alloca $ \pendingPtr ->
            do poke statePtr (-1)   -- -1 is not use by any enum value
               poke pendingPtr (-1)
               result <- {# call element_get_state #} (toElement element)
                                                      statePtr
                                                      pendingPtr
                                                      (fromIntegral timeout)
               state <- unmarshalState statePtr
               pending <- unmarshalState pendingPtr
               return (toEnum $ fromIntegral result, state, pending)
    where unmarshalState statePtr = do
            cState <- peek statePtr
            if cState /= -1
               then return $ Just $ toEnum $ fromIntegral cState
               else return Nothing

-- | Lock the state of @element@, so state changes in the parent don't
--   affect this element any longer.
elementSetLockedState :: ElementClass elementT
                      => elementT -- ^ @element@ - 
                      -> Bool     -- ^ @lockedState@ - 'True' for locked, 'False' for unlocked
                      -> IO Bool  -- ^ 'True' if the state was changed, 'False' if bad
                                  --   parameters were given or no change was needed
elementSetLockedState element lockedState =
    liftM toBool $ {# call element_set_locked_state #} (toElement element) $ fromBool lockedState

-- | Determine whether @element@'s state is locked.
elementIsLockedState :: ElementClass elementT
                     => elementT -- ^ @element@ - 
                     -> IO Bool  -- ^ 'True' if @element@'s state is locked, 'False' otherwise
elementIsLockedState element =
    liftM toBool $ {# call element_is_locked_state #} $ toElement element

-- | Abort @element@'s state change. This function is used by elements
--   that do asynchronous state changes and find out something is wrong.
--   
--   This function should be called with the state lock held.
elementAbortState :: ElementClass elementT
                  => elementT -- ^ @element@ - 
                  -> IO ()
elementAbortState element =
    {# call element_abort_state #} $ toElement element

-- | Get a string representation of @state@.
elementStateGetName :: State  -- ^ @state@ - 
                    -> String -- ^ the name of @state@
elementStateGetName state =
    unsafePerformIO $ ({# call element_state_get_name #} $ fromIntegral $ fromEnum state) >>= peekUTFString

#if GSTREAMER_CHECK_VERSION(0,10,11)
-- | Get a string representation of @stateRet@.
--   
--   Since 0.10.11.
elementStateChangeReturnGetName :: StateChangeReturn -- ^ @stateRet@ - 
                                -> String            -- ^ the name of @stateRet@
elementStateChangeReturnGetName stateRet =
    unsafePerformIO $ ({# call element_state_change_return_get_name #} $ fromIntegral $ fromEnum stateRet) >>= peekUTFString
#endif

-- | Try to change the state of @element@ to the same as its
--   parent. If this function returns 'False', the state of the
--   element is undefined.
elementSyncStateWithParent :: ElementClass elementT
                           => elementT -- ^ @element@ - 
                           -> IO Bool  -- ^ 'True' if the element's state could be
                                       --   synced with its parent's state
elementSyncStateWithParent element =
    liftM toBool $ {# call element_sync_state_with_parent #} $ toElement element

elementGetQueryTypes :: ElementClass element
                     => element
                     -> IO [QueryType]
elementGetQueryTypes element =
    liftM (map (toEnum . fromIntegral)) $
        {# call element_get_query_types #} (toElement element) >>=
            peekArray0 0

-- | Perform a query on the given element.
--   
--   For elements that don't implement a query handler, this function
--   forwards the query to a random srcpad or to the peer of a random
--   linked sinkpad of this element.
elementQuery :: (ElementClass element, QueryClass query)
             => element -- ^ @element@ - 
             -> query   -- ^ @query@ - 
             -> IO Bool -- ^ 'True' if the query could be performed
elementQuery element query =
    do query' <- {# call mini_object_copy #} (toMiniObject query) >>=
                    newForeignPtr_ . castPtr
       fmap toBool $ {# call element_query #} (toElement element) $ Query query'

-- | Query an element for the convertion of a value from one format to
--   another.
elementQueryConvert :: ElementClass element
                    => element -- ^ @element@ - the element to query
                    -> Format  -- ^ @srcFormat@ - the format to convert from
                    -> Int64   -- ^ @srcVal@ - the value to convert
                    -> Format  -- ^ @destFormat@ - the format to convert to
                    -> IO (Maybe (Format, Word64)) -- ^ the resulting format and value
elementQueryConvert element srcFormat srcVal destFormat =
    alloca $ \destFormatPtr ->
        alloca $ \destValPtr ->
            do poke destFormatPtr $ fromIntegral $ fromEnum destFormat
               success <- {# call element_query_convert #} (toElement element)
                                                       (fromIntegral $ fromEnum srcFormat)
                                                       (fromIntegral srcVal)
                                                       destFormatPtr
                                                       destValPtr
               if toBool success
                   then do destFormat <- peek destFormatPtr
                           destVal    <- peek destValPtr
                           return $ Just (toEnum $ fromIntegral destFormat,
                                          fromIntegral destVal)
                   else return Nothing

-- | Query an element for its stream position.
elementQueryPosition :: ElementClass element
                     => element -- ^ @element@ - the element to query
                     -> Format  -- ^ @format@ - the format requested
                     -> IO (Maybe (Format, Word64)) -- ^ the resulting format and value
elementQueryPosition element format =
    alloca $ \formatPtr ->
        alloca $ \curPtr ->
            do poke formatPtr $ fromIntegral $ fromEnum format
               success <- {# call element_query_position #} (toElement element) formatPtr curPtr
               if toBool success
                   then do format <- peek formatPtr
                           cur    <- peek curPtr
                           return $ Just (toEnum $ fromIntegral format,
                                          fromIntegral cur)
                   else return Nothing

-- | Query an element for its stream duration.
elementQueryDuration :: ElementClass element
                     => element -- ^ @element@ - the element to query
                     -> Format -- ^ @format@ - the format requested
                     -> IO (Maybe (Format, Word64)) -- ^ the resulting format and value
elementQueryDuration element format =
    alloca $ \formatPtr ->
        alloca $ \durationPtr ->
            do poke formatPtr $ fromIntegral $ fromEnum format
               success <- {# call element_query_duration #} (toElement element) formatPtr durationPtr
               if toBool success
                   then do format   <- peek formatPtr
                           duration <- peek durationPtr
                           return $ Just (toEnum $ fromIntegral format,
                                          fromIntegral duration)
                   else return Nothing

-- | Send an event to an element.
--   
--   If the element doesn't implement an event handler, the event will
--   be pushed to a random linked sink pad for upstream events or a
--   random linked source pad for downstream events.
elementSendEvent :: (ElementClass element, EventClass event)
                 => element -- ^ @element@ - the element to send the event to
                 -> event   -- ^ @event@ - the event to send
                 -> IO Bool -- ^ 'True' if the event was handled
elementSendEvent element event =
    liftM toBool $
        giveMiniObject (toEvent event) $ {# call element_send_event #} (toElement element)

#if GSTREAMER_CHECK_VERSION(0,10,7)
-- | Perform a seek on the given element. This function only supports
--   seeking to a position relative to the start of the stream. For
--   more complex operations like segment seeks (such as for looping),
--   or changing the playback rate, or seeking relative to the last
--   configured playback segment you should use 'elementSeek'.
--   
--   In a completely prerolled pipeline in the 'StatePaused' or
--   'StatePlaying' states, seeking is always guaranteed to return
--   'True' on a seekable media type, or 'False' when the media type
--   is certainly not seekable (such as a live stream).
--   
--   Some elements allow for seeking in the 'StateReady' state, in
--   which case they will store the seek event and execute it when
--   they are put into the 'StatePaused' state. If the element
--   supports seek in "StateReady", it will always return 'True' when
--   it recieves the event in the 'StateReady' state.
elementSeekSimple :: ElementClass element
                  => element     -- ^ @element@ - the element to seek on
                  -> Format      -- ^ @format@ - the 'Format' to evecute the seek in,
                                 --              such as 'FormatTime'
                  -> [SeekFlags] -- ^ @seekFlags@ - seek options; playback applications
                                 --                 will usually want to use
                                 --                 @['SeekFlagFlush','SeekFlagKeyUnit']@
                  -> Int64       -- ^ @seekPos@ - the position to seek to, relative to
                                 --               start; if you are doing a seek in
                                 --               'FormatTime' this value is in nanoseconds;
                                 --               see 'second', 'msecond', 'usecond', &
                                 --               'nsecond'
                  -> IO Bool     -- ^ 'True' if the seek operation succeeded
elementSeekSimple element format seekFlags seekPos =
    liftM toBool $
        {# call element_seek_simple #} (toElement element)
                                       (fromIntegral $ fromEnum format)
                                       (fromIntegral $ fromFlags seekFlags)
                                       (fromIntegral seekPos)
#endif

-- | Send a seek event to an element. See
--   'Media.Streaming.GStreamer.Core.Event.eventNewSeek' for the
--   details of the parameters. The seek event is sent to the element
--   using 'elementSendEvent'.
elementSeek :: ElementClass element
            => element     -- ^ @element@ - the element to seek on
            -> Double      -- ^ @rate@ - the new playback rate
            -> Format      -- ^ @format@ - the format of the seek values
            -> [SeekFlags] -- ^ @seekFlags@ - the options to use
            -> SeekType    -- ^ @curType@ - type and flags for the new current position
            -> Int64       -- ^ @cur@ - the value of the new current position
            -> SeekType    -- ^ @stopType@ - type and flags for the new stop position
            -> Int64       -- ^ @stop@ - the value of the new stop position
            -> IO Bool     -- ^ 'True' if the event was handled
elementSeek element rate format flags curType cur stopType stop =
    liftM toBool $
        {# call element_seek #} (toElement element)
                                (realToFrac rate)
                                (fromIntegral $ fromEnum format)
                                (fromIntegral $ fromFlags flags)
                                (fromIntegral $ fromEnum curType)
                                (fromIntegral cur)
                                (fromIntegral $ fromEnum stopType)
                                (fromIntegral stop)

-- | The signal emitted when an element will not generate more dynamic
--   pads.
elementNoMorePads :: (ElementClass element)
                  => Signal element (IO ())
elementNoMorePads =
    Signal $ connect_NONE__NONE "no-more-pads"

-- | The signal emitted when a new 'Pad' has been added to the
--   element.
elementPadAdded :: (ElementClass element)
                => Signal element (Pad -> IO ())
elementPadAdded =
    Signal $ connect_OBJECT__NONE "pad-added"

-- | The signal emitted when a 'Pad' has been removed from the
--   element.
elementPadRemoved :: (ElementClass element)
                  => Signal element (Pad -> IO ())
elementPadRemoved =
    Signal $ connect_OBJECT__NONE "pad-removed"
