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
module Media.Streaming.GStreamer.Pad (
  
  Pad,
  PadClass,
  castToPad,
  toPad,
  fromPad,
  PadFlags(..),
  
  PadDirection(..),
  PadLinkReturn(..),
  FlowReturn(..),
  ActivateMode(..),
  
  padNew,
  padGetDirection,
  padGetParentElement,
  padLink,
  padUnlink,
  padIsLinked,
  padCanLink,
  padGetCaps,
  padGetAllowedCaps,
  padGetNegotiatedCaps,
  padGetPadTemplateCaps,
  padSetCaps,
  padGetPeer,
  padPeerGetCaps,
  padIsActive,
  padSetBlocked,
  padIsBlocked,
  padIsBlocking,
  padNewFromTemplate,
  padAcceptCaps,
  padProxyGetcaps,
  padFixateCaps,
  padPeerAcceptCaps,
  padSendEvent,
  padQuery,
  padQueryPosition,
  padQueryDuration,
  padQueryConvert,
  padQueryPeerPosition,
  padQueryPeerDuration,
  padQueryPeerConvert,
  padGetQueryTypes,
  padGetInternalLinks,
  onPadLinked,
  afterPadLinked,
  onPadRequestLink,
  afterPadRequestLink,
  onPadUnlinked,
  afterPadUnlinked
  
  ) where

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
{#import Media.Streaming.GStreamer.Types#}
{#import Media.Streaming.GStreamer.Signals#}
import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
{#import System.Glib.Signals#}
import GHC.Base (unsafeCoerce#)

{# context lib = "gstreamer" prefix = "gst" #}

padNew :: String
       -> PadDirection
       -> IO Pad
padNew name direction =
    withUTFString name $ \cName ->
        {# call pad_new #} cName (fromIntegral $ fromEnum direction) >>=
            newPad

padGetDirection :: PadClass pad
                => pad
                -> IO PadDirection
padGetDirection pad =
    liftM (toEnum . fromIntegral) $
        {# call pad_get_direction #} $ toPad pad

padGetParentElement :: PadClass pad
                    => pad
                    -> IO Element
padGetParentElement pad =
    {# call pad_get_parent_element #} (toPad pad) >>=
        newElement

padLink :: (PadClass srcpad, PadClass sinkpad)
        => srcpad
        -> sinkpad
        -> IO PadLinkReturn
padLink srcpad sinkpad =
    liftM (toEnum . fromIntegral) $
        {# call pad_link #} (toPad srcpad) (toPad sinkpad)

padUnlink :: (PadClass srcpad, PadClass sinkpad)
          => srcpad
          -> sinkpad
          -> IO Bool
padUnlink srcpad sinkpad =
    liftM toBool $
        {# call pad_unlink #} (toPad srcpad) (toPad sinkpad)

padIsLinked :: PadClass pad
            => pad
            -> IO Bool
padIsLinked pad =
    liftM toBool $
        {# call pad_is_linked #} (toPad pad)

padCanLink :: (PadClass srcpad, PadClass sinkpad)
           => srcpad
           -> sinkpad
           -> IO Bool
padCanLink srcpad sinkpad =
    liftM toBool $
        {# call pad_can_link #} (toPad srcpad) (toPad sinkpad)

padGetCaps :: PadClass pad
           => pad
           -> IO Caps
padGetCaps pad =
    {# call pad_get_caps #} (toPad pad) >>= newCaps

padGetAllowedCaps :: PadClass pad
                  => pad
                  -> IO (Maybe Caps)
padGetAllowedCaps pad =
    {# call pad_get_allowed_caps #} (toPad pad) >>=
        maybePeek newCaps

padGetNegotiatedCaps :: PadClass pad
                     => pad
                     -> IO (Maybe Caps)
padGetNegotiatedCaps pad =
    {# call pad_get_negotiated_caps #} (toPad pad) >>=
        maybePeek newCaps

padGetPadTemplateCaps :: PadClass pad
                      => pad
                      -> IO Caps
padGetPadTemplateCaps pad =
    (liftM Caps $
         {# call pad_get_pad_template_caps #} (toPad pad) >>=
             newForeignPtr_) >>=
        {# call caps_copy #} >>= newCaps

padSetCaps :: PadClass pad
           => pad
           -> Maybe Caps
           -> IO Bool
padSetCaps pad caps =
    liftM toBool $ {# call pad_set_caps #} (toPad pad) $
        fromMaybe (Caps nullForeignPtr) caps

padGetPeer :: PadClass pad
           => pad
           -> IO (Maybe Pad)
padGetPeer pad =
    {# call pad_get_peer #} (toPad pad) >>= maybePeek newPad

padPeerGetCaps :: PadClass pad
               => pad
               -> IO (Maybe Caps)
padPeerGetCaps pad =
    {# call pad_peer_get_caps #} (toPad pad) >>= maybePeek newCaps

padIsActive :: PadClass pad
            => pad
            -> IO Bool
padIsActive =
    (liftM toBool) . {# call pad_is_active #} . toPad

padSetBlocked :: PadClass pad
              => pad
              -> Bool
              -> IO Bool
padSetBlocked pad blocked =
    liftM toBool $
        {# call pad_set_blocked #} (toPad pad) (fromBool blocked)

padIsBlocked :: PadClass pad
             => pad
             -> IO Bool
padIsBlocked =
    (liftM toBool) . {# call pad_is_blocked #} . toPad

padIsBlocking :: PadClass pad
              => pad
              -> IO Bool
padIsBlocking =
    (liftM toBool) . {# call pad_is_blocking #} . toPad

padNewFromTemplate :: PadTemplateClass padTemplate
                   => padTemplate
                   -> String
                   -> IO (Maybe Pad)
padNewFromTemplate padTemplate name =
    withUTFString name $ \cName ->
        {# call pad_new_from_template #} (toPadTemplate padTemplate) cName >>=
            maybePeek newPad

padAcceptCaps :: PadClass pad
              => pad
              -> Caps
              -> IO Bool
padAcceptCaps pad caps =
    liftM toBool $ {# call pad_accept_caps #} (toPad pad) caps

padProxyGetcaps :: PadClass pad
                => pad
                -> IO Caps
padProxyGetcaps pad =
    {# call pad_proxy_getcaps #} (toPad pad) >>= newCaps

padFixateCaps :: PadClass pad
              => pad
              -> Caps
              -> IO Caps
padFixateCaps pad caps =
    do caps' <- {# call caps_copy #} caps >>= newForeignPtr_
       {# call pad_fixate_caps #} (toPad pad) (Caps caps')
       withForeignPtr caps' newCaps

padPeerAcceptCaps :: PadClass pad
                  => pad
                  -> Caps
                  -> IO Bool
padPeerAcceptCaps pad caps =
    liftM toBool $ {# call pad_peer_accept_caps #} (toPad pad) caps

padSendEvent :: (PadClass pad, EventClass event)
             => pad
             -> event
             -> IO Bool
padSendEvent pad event =
    liftM toBool $
        giveMiniObject (toEvent event) $ {# call pad_send_event #} (toPad pad)

padQuery :: (PadClass pad, QueryClass query)
         => pad
         -> query
         -> IO (Maybe query)
padQuery pad query =
    do query' <- {# call mini_object_copy #} (toMiniObject query) >>=
                    newForeignPtr_ . castPtr
       success <- {# call pad_query #} (toPad pad) $ Query query'
       if toBool success
           then liftM (Just . unsafeCoerce#) $ withForeignPtr query' $ newQuery . castPtr
           else return Nothing

padQueryPosition :: PadClass pad
                 => pad
                 -> IO (Maybe (Format, Int64))
padQueryPosition pad =
    alloca $ \formatPtr ->
        alloca $ \curPtr ->
            do success <- {# call pad_query_position #} (toPad pad) formatPtr curPtr
               if toBool success
                   then do format <- peek formatPtr
                           cur    <- peek curPtr
                           return $ Just (toEnum $ fromIntegral format,
                                          fromIntegral cur)
                   else return Nothing

padQueryDuration :: PadClass pad
                 => pad
                 -> IO (Maybe (Format, Int64))
padQueryDuration pad =
    alloca $ \formatPtr ->
        alloca $ \durationPtr ->
            do success <- {# call pad_query_duration #} (toPad pad) formatPtr durationPtr
               if toBool success
                   then do format   <- peek formatPtr
                           duration <- peek durationPtr
                           return $ Just (toEnum $ fromIntegral format,
                                          fromIntegral duration)
                   else return Nothing

padQueryConvert :: PadClass pad
                => pad
                -> Format
                -> Int64
                -> IO (Maybe (Format, Int64))
padQueryConvert pad srcFormat srcVal =
    alloca $ \destFormatPtr ->
        alloca $ \destValPtr ->
            do success <- {# call pad_query_convert #} (toPad pad)
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

padQueryPeerPosition :: PadClass pad
                     => pad
                     -> IO (Maybe (Format, Int64))
padQueryPeerPosition pad =
    alloca $ \formatPtr ->
        alloca $ \curPtr ->
            do success <- {# call pad_query_peer_position #} (toPad pad) formatPtr curPtr
               if toBool success
                   then do format <- peek formatPtr
                           cur    <- peek curPtr
                           return $ Just (toEnum $ fromIntegral format,
                                          fromIntegral cur)
                   else return Nothing

padQueryPeerDuration :: PadClass pad
                     => pad
                     -> IO (Maybe (Format, Int64))
padQueryPeerDuration pad =
    alloca $ \formatPtr ->
        alloca $ \durationPtr ->
            do success <- {# call pad_query_peer_duration #} (toPad pad) formatPtr durationPtr
               if toBool success
                   then do format   <- peek formatPtr
                           duration <- peek durationPtr
                           return $ Just (toEnum $ fromIntegral format,
                                          fromIntegral duration)
                   else return Nothing

padQueryPeerConvert :: PadClass pad
                    => pad
                    -> Format
                    -> Int64
                    -> IO (Maybe (Format, Int64))
padQueryPeerConvert pad srcFormat srcVal =
    alloca $ \destFormatPtr ->
        alloca $ \destValPtr ->
            do success <- {# call pad_query_peer_convert #} (toPad pad)
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

padGetQueryTypes :: PadClass pad
                 => pad
                 -> IO [QueryType]
padGetQueryTypes pad =
    liftM (map (toEnum . fromIntegral)) $
        {# call pad_get_query_types #} (toPad pad) >>=
            peekArray0 0

padGetInternalLinks :: PadClass pad
                    => pad
                    -> IO [Pad]
padGetInternalLinks pad =
    {# call pad_get_internal_links #} (toPad pad) >>=
        fromGList >>=
            mapM newPad_

onPadLinked, afterPadLinked :: (PadClass pad)
                            => pad
                            -> (Pad -> IO ())
                            -> IO (ConnectId pad)
onPadLinked =
    connect_OBJECT__NONE "linked" False
afterPadLinked =
    connect_OBJECT__NONE "linked" True

onPadRequestLink, afterPadRequestLink :: (PadClass pad)
                                      => pad
                                      -> IO ()
                                      -> IO (ConnectId pad)
onPadRequestLink =
    connect_NONE__NONE "request-link" False
afterPadRequestLink =
    connect_NONE__NONE "request-link" True

onPadUnlinked, afterPadUnlinked :: (PadClass pad)
                                => pad
                                -> (Pad -> IO ())
                                -> IO (ConnectId pad)
onPadUnlinked =
    connect_OBJECT__NONE "unlinked" False
afterPadUnlinked =
    connect_OBJECT__NONE "unlinked" True

