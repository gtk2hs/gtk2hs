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
module Media.Streaming.GStreamer.Element (
  
  Element,
  ElementClass,
  castToElement,
  toElement,
  fromElement,
  ElementFlags(..),
  State(..),
  StateChange(..),
  StateChangeReturn(..),
  elementAddPad,
  elementGetPad,
  elementGetCompatiblePad,
  elementGetCompatiblePadTemplate,
  elementGetRequestPad,
  elementGetStaticPad,
  elementNoMorePads,
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
  elementStateChangeReturnGetName,
  elementSyncStateWithParent,
  elementGetQueryTypes,
  elementQuery,
  elementQueryConvert,
  elementQueryPosition,
  elementQueryDuration,
  elementSendEvent, 
  elementSeekSimple,
  elementSeek,
  onElementNoMorePads,
  afterElementNoMorePads,
  onElementPadAdded,
  afterElementPadAdded,
  onElementPadRemoved,
  afterElementPadRemoved
  
  ) where

import Control.Monad         ( liftM )
import Data.Maybe            ( fromMaybe )
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.UTFString ( withUTFString
                             , peekUTFString )
{#import System.Glib.Signals#}
{#import System.Glib.GObject#}
{#import Media.Streaming.GStreamer.Types#}
{#import Media.Streaming.GStreamer.Signals#}
import GHC.Base              ( unsafeCoerce# )

{# context lib = "gstreamer" prefix = "gst" #}

elementAddPad :: (ElementClass elementT, PadClass padT) =>
                 elementT
              -> padT
              -> IO Bool
elementAddPad element pad =
    liftM toBool $ {# call element_add_pad #} (toElement element) (toPad pad)

elementGetPad :: ElementClass elementT =>
                 elementT
              -> String
              -> IO (Maybe Pad)
elementGetPad element name =
    (withUTFString name $ {# call element_get_pad #} (toElement element)) >>=
        maybePeek newPad

elementGetCompatiblePad :: (ElementClass elementT, PadClass padT) =>
                           elementT
                        -> padT
                        -> Caps
                        -> IO (Maybe Pad)
elementGetCompatiblePad element pad caps =
    {# call element_get_compatible_pad #} (toElement element) (toPad pad) caps >>=
        maybePeek newPad

elementGetCompatiblePadTemplate :: (ElementClass elementT, PadTemplateClass padTemplateT) =>
                                   elementT
                                -> padTemplateT
                                -> IO (Maybe PadTemplate)
elementGetCompatiblePadTemplate element padTemplate =
    {# call element_get_compatible_pad_template #} (toElement element) (toPadTemplate padTemplate) >>=
        maybePeek newPadTemplate


elementGetRequestPad :: ElementClass elementT =>
                        elementT
                     -> String
                     -> IO (Maybe Pad)
elementGetRequestPad element name =
    (withUTFString name $ {# call element_get_request_pad #} (toElement element)) >>=
        maybePeek newPad_ -- no finalizer; use elementReleaseRequestPad

elementGetStaticPad :: ElementClass elementT =>
                       elementT
                    -> String
                    -> IO (Maybe Pad)
elementGetStaticPad element name =
    (withUTFString name $ {# call element_get_static_pad #} (toElement element)) >>=
        maybePeek newPad

elementNoMorePads :: ElementClass elementT =>
                     elementT
                  -> IO ()
elementNoMorePads element =
    {# call element_no_more_pads #} (toElement element)

elementReleaseRequestPad :: (ElementClass elementT, PadClass padT) =>
                            elementT
                         -> padT
                         -> IO ()
elementReleaseRequestPad element pad =
    {# call element_release_request_pad #} (toElement element) (toPad pad)

elementRemovePad :: (ElementClass elementT, PadClass padT) =>
                    elementT
                 -> padT
                 -> IO Bool
elementRemovePad element pad =
    liftM toBool $ {# call element_remove_pad #} (toElement element) (toPad pad)

elementIteratePads :: (ElementClass elementT) =>
                      elementT
                   -> IO (Iterator Pad)
elementIteratePads element =
    {# call element_iterate_pads #} (toElement element) >>= newIterator

elementIterateSinkPads :: (ElementClass elementT) =>
                          elementT
                       -> IO (Iterator Pad)
elementIterateSinkPads element =
    {# call element_iterate_sink_pads #} (toElement element) >>= newIterator

elementIterateSrcPads :: (ElementClass elementT) =>
                          elementT
                       -> IO (Iterator Pad)
elementIterateSrcPads element =
    {# call element_iterate_sink_pads #} (toElement element) >>= newIterator

elementLink :: (ElementClass elementT1, ElementClass elementT2) =>
               elementT1
            -> elementT2
            -> IO Bool
elementLink element1 element2 =
    liftM toBool $ {# call element_link #} (toElement element1) (toElement element2)

elementUnlink :: (ElementClass elementT1, ElementClass elementT2) =>
                 elementT1
              -> elementT2
              -> IO ()
elementUnlink element1 element2 =
    {# call element_unlink #} (toElement element1) (toElement element2)

elementLinkPads :: (ElementClass elementT1, ElementClass elementT2) =>
                   elementT1
                -> String
                -> elementT2
                -> String
                -> IO Bool
elementLinkPads src srcPadName dest destPadName =
    withUTFString destPadName $ \cDestPadName ->
        withUTFString srcPadName $ \cSrcPadName ->
            liftM toBool $ {# call element_link_pads #} (toElement src) cSrcPadName (toElement dest) cDestPadName

elementUnlinkPads :: (ElementClass elementT1, ElementClass elementT2) =>
                     elementT1
                  -> String
                  -> elementT2
                  -> String
                  -> IO ()
elementUnlinkPads src srcPadName dest destPadName =
    withUTFString destPadName $ \cDestPadName ->
        withUTFString srcPadName $ \cSrcPadName ->
            {# call element_unlink_pads #} (toElement src) cSrcPadName (toElement dest) cDestPadName

elementLinkPadsFiltered :: (ElementClass elementT1, ElementClass elementT2) =>
                           elementT1
                        -> String
                        -> elementT2
                        -> String
                        -> Caps
                        -> IO Bool
elementLinkPadsFiltered src srcPadName dest destPadName filter =
    withUTFString destPadName $ \cDestPadName ->
        withUTFString srcPadName $ \cSrcPadName ->
            liftM toBool $ {# call element_link_pads_filtered #} (toElement src) cSrcPadName (toElement dest) cDestPadName filter

elementLinkFiltered :: (ElementClass elementT1, ElementClass elementT2) =>
                       elementT1
                    -> elementT2
                    -> Maybe Caps
                    -> IO Bool
elementLinkFiltered element1 element2 filter =
    liftM toBool $
        {# call element_link_filtered #} (toElement element1) (toElement element2) $
            fromMaybe (Caps nullForeignPtr) filter

elementSetBaseTime :: ElementClass elementT =>
                      elementT
                   -> ClockTimeDiff
                   -> IO ()
elementSetBaseTime element time =
    {# call element_set_base_time #} (toElement element) $ fromIntegral time

elementGetBaseTime :: ElementClass elementT =>
                      elementT
                   -> IO ClockTimeDiff
elementGetBaseTime element =
    liftM fromIntegral $ {# call element_get_base_time #} (toElement element)

elementSetBus :: (ElementClass elementT, BusClass busT) =>
                 elementT
              -> busT
              -> IO ()
elementSetBus element bus =
    {# call element_set_bus #} (toElement element) (toBus bus)

elementGetBus :: ElementClass elementT =>
                 elementT
              -> IO Bus
elementGetBus element =
    {# call element_get_bus #} (toElement element) >>= newBus

elementGetFactory :: ElementClass elementT =>
                     elementT
                  -> IO ElementFactory
elementGetFactory element =
    {# call element_get_factory #} (toElement element) >>= newElementFactory_

elementSetIndex :: (ElementClass elementT, IndexClass indexT) =>
                   elementT
                -> indexT
                -> IO ()
elementSetIndex element index =
    {# call element_set_index #} (toElement element) (toIndex index)

elementGetIndex :: ElementClass elementT =>
                   elementT
                -> IO (Maybe Index)
elementGetIndex element =
    {# call element_get_index #} (toElement element) >>= maybePeek newIndex

elementIsIndexable :: ElementClass elementT =>
                      elementT
                   -> IO Bool
elementIsIndexable element =
    liftM toBool $ {# call element_is_indexable #} (toElement element)

elementRequiresClock :: ElementClass elementT =>
                        elementT
                     -> IO Bool
elementRequiresClock element =
    liftM toBool $ {# call element_requires_clock #} (toElement element)

elementSetClock :: (ElementClass elementT, ClockClass clockT) =>
                   elementT
                -> clockT
                -> IO Bool
elementSetClock element clock =
    liftM toBool $ {# call element_set_clock #} (toElement element) (toClock clock)

elementGetClock :: ElementClass elementT =>
                   elementT
                -> IO (Maybe Clock)
elementGetClock element =
    {# call element_get_clock #} (toElement element) >>= maybePeek newClock

elementProvidesClock :: ElementClass elementT =>
                        elementT
                     -> IO Bool
elementProvidesClock element =
    liftM toBool $ {# call element_provides_clock #} $ toElement element

elementProvideClock :: ElementClass elementT =>
                       elementT
                    -> IO (Maybe Clock)
elementProvideClock element =
    {# call element_provide_clock #} (toElement element) >>= maybePeek newClock

elementSetState :: ElementClass elementT =>
                   elementT
                -> State
                -> IO StateChangeReturn
elementSetState element state =
    liftM (toEnum . fromIntegral) $ {# call element_set_state #} (toElement element) $
        fromIntegral $ fromEnum state

elementGetState :: ElementClass elementT =>
                   elementT
                -> ClockTime
                -> IO (StateChangeReturn, State, State)
elementGetState element timeout =
    alloca $ \statePtr ->
        alloca $ \pendingPtr ->
            do result <- {# call element_get_state #} (toElement element) statePtr pendingPtr $ fromIntegral timeout
               state <- peek statePtr
               pending <- peek pendingPtr
               return (toEnum (fromIntegral result),
                       toEnum (fromIntegral state),
                       toEnum (fromIntegral pending))

elementSetLockedState :: ElementClass elementT =>
                         elementT
                      -> Bool
                      -> IO Bool
elementSetLockedState element lockedState =
    liftM toBool $ {# call element_set_locked_state #} (toElement element) $ fromBool lockedState

elementIsLockedState :: ElementClass elementT =>
                        elementT
                     -> IO Bool
elementIsLockedState element =
    liftM toBool $ {# call element_is_locked_state #} $ toElement element

elementAbortState :: ElementClass elementT =>
                     elementT
                  -> IO ()
elementAbortState element =
    {# call element_abort_state #} $ toElement element

elementStateGetName :: State
                    -> String
elementStateGetName state =
    unsafePerformIO $ ({# call element_state_get_name #} $ fromIntegral $ fromEnum state) >>= peekUTFString

elementStateChangeReturnGetName :: StateChangeReturn
                                -> String
elementStateChangeReturnGetName stateRet =
    unsafePerformIO $ ({# call element_state_change_return_get_name #} $ fromIntegral $ fromEnum stateRet) >>= peekUTFString

elementSyncStateWithParent :: ElementClass elementT =>
                              elementT
                           -> IO Bool
elementSyncStateWithParent element =
    liftM toBool $ {# call element_sync_state_with_parent #} $ toElement element

elementGetQueryTypes :: ElementClass element
                     => element
                     -> IO [QueryType]
elementGetQueryTypes element =
    liftM (map (toEnum . fromIntegral)) $
        {# call element_get_query_types #} (toElement element) >>=
            peekArray0 0

elementQuery :: (ElementClass element, QueryClass query)
             => element
             -> query
             -> IO (Maybe query)
elementQuery element query =
    do query' <- {# call mini_object_copy #} (toMiniObject query) >>=
                    newForeignPtr_ . castPtr
       success <- {# call element_query #} (toElement element) $ Query query'
       if toBool success
           then liftM (Just . unsafeCoerce#) $ withForeignPtr query' $ newQuery . castPtr
           else return Nothing

elementQueryConvert :: ElementClass element
                    => element
                    -> Format
                    -> Int64
                    -> IO (Maybe (Format, Int64))
elementQueryConvert element srcFormat srcVal =
    alloca $ \destFormatPtr ->
        alloca $ \destValPtr ->
            do success <- {# call element_query_convert #} (toElement element)
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

elementQueryPosition :: ElementClass element
                     => element
                     -> IO (Maybe (Format, Int64))
elementQueryPosition element =
    alloca $ \formatPtr ->
        alloca $ \curPtr ->
            do success <- {# call element_query_position #} (toElement element) formatPtr curPtr
               if toBool success
                   then do format <- peek formatPtr
                           cur    <- peek curPtr
                           return $ Just (toEnum $ fromIntegral format,
                                          fromIntegral cur)
                   else return Nothing

elementQueryDuration :: ElementClass element
                     => element
                     -> IO (Maybe (Format, Int64))
elementQueryDuration element =
    alloca $ \formatPtr ->
        alloca $ \durationPtr ->
            do success <- {# call element_query_duration #} (toElement element) formatPtr durationPtr
               if toBool success
                   then do format   <- peek formatPtr
                           duration <- peek durationPtr
                           return $ Just (toEnum $ fromIntegral format,
                                          fromIntegral duration)
                   else return Nothing

elementSendEvent :: (ElementClass element, EventClass event)
                 => element
                 -> event
                 -> IO Bool
elementSendEvent element event =
    liftM toBool $
        giveMiniObject (toEvent event) $ {# call element_send_event #} (toElement element)

elementSeekSimple :: ElementClass element
                  => element
                  -> Format
                  -> [SeekFlags]
                  -> Int64
                  -> IO Bool
elementSeekSimple element format seekFlags seekPos =
    liftM toBool $
        {# call element_seek_simple #} (toElement element)
                                       (fromIntegral $ fromEnum format)
                                       (fromIntegral $ fromFlags seekFlags)
                                       (fromIntegral seekPos)

elementSeek :: ElementClass element
            => element
            -> Double
            -> Format
            -> [SeekFlags]
            -> SeekType
            -> Int64
            -> SeekType
            -> Int64
            -> IO Bool
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

onElementNoMorePads, afterElementNoMorePads :: (ElementClass element)
                                            => element
                                            -> IO ()
                                            -> IO (ConnectId element)
onElementNoMorePads =
    connect_NONE__NONE "no-more-pads" False
afterElementNoMorePads =
    connect_NONE__NONE "no-more-pads" True

onElementPadAdded, afterElementPadAdded :: (ElementClass element)
                                        => element
                                        -> (Pad -> IO ())
                                        -> IO (ConnectId element)
onElementPadAdded =
    connect_OBJECT__NONE "pad-added" False
afterElementPadAdded =
    connect_OBJECT__NONE "pad-added" True

onElementPadRemoved, afterElementPadRemoved :: (ElementClass element)
                                            => element
                                            -> (Pad -> IO ())
                                            -> IO (ConnectId element)
onElementPadRemoved =
    connect_OBJECT__NONE "pad-removed" False
afterElementPadRemoved =
    connect_OBJECT__NONE "pad-removed" True
