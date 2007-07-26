-- GIMP Toolkit (GTK) Binding for Haskell: binding to gstreamer   -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
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
module Media.Streaming.GStreamer.Core.Index (
  
  Index,
  IndexClass,
  castToIndex,
  toIndex,
  fromIndex,
  indexNew,
  indexCommit,
  indexGetGroup,
  indexNewGroup,
  indexSetGroup,
  indexSetCertainty,
  indexSetFilter,
  indexGetWriterId,
  indexAddFormat,
  indexAddAssociations,
  indexAddId,
  indexGetAssocEntry,
  indexEntryAssocMap,
  onIndexEntryAdded,
  afterIndexEntryAdded,
  
  ) where

import Control.Monad (liftM)
{#import Media.Streaming.GStreamer.Core.Types#}
{#import Media.Streaming.GStreamer.Core.Signals#}
import System.Glib.Flags
import System.Glib.FFI
import System.Glib.UTFString
{#import System.Glib.Signals#}
{#import System.Glib.GObject#} (mkFunPtrDestroyNotify)

{# context lib = "gstreamer" prefix = "gst" #}

indexNew :: IO Index
indexNew =
    {# call index_new #} >>= takeIndex

indexCommit :: IndexClass index
            => index
            -> Int
            -> IO ()
indexCommit index id =
    {# call index_commit #} (toIndex index) $ fromIntegral id

indexGetGroup :: IndexClass index
              => index
              -> IO Int
indexGetGroup index =
    liftM fromIntegral $ {# call index_get_group #} $ toIndex index

indexNewGroup :: IndexClass index
              => index
              -> IO Int
indexNewGroup index =
    liftM fromIntegral $ {# call index_new_group #} $ toIndex index

indexSetGroup :: IndexClass index
              => index
              -> Int
              -> IO Bool
indexSetGroup index groupnum =
    liftM toBool $ {# call index_set_group #} (toIndex index) $ fromIntegral groupnum

indexSetCertainty :: IndexClass index
                  => index
                  -> IndexCertainty
                  -> IO ()
indexSetCertainty index certainty =
    {# call index_set_certainty #} (toIndex index) $ cFromEnum certainty

type CIndexFilter =  Ptr Index
                  -> Ptr IndexEntry
                  -> {# type gpointer #}
                  -> IO {# type gboolean #}
marshalIndexFilter :: IndexFilter
                   -> IO {# type GstIndexFilter #}
marshalIndexFilter indexFilter =
    makeIndexFilter cIndexFilter
    where cIndexFilter :: CIndexFilter
          cIndexFilter cIndex cIndexEntry _ =
              do index <- peekIndex cIndex
                 indexEntry <- peekIndexEntry cIndexEntry
                 liftM fromBool $ indexFilter index indexEntry
foreign import ccall "wrapper"
    makeIndexFilter :: CIndexFilter
                    -> IO {# type GstIndexFilter #}

indexSetFilter :: IndexClass index
               => index
               -> IndexFilter
               -> IO ()
indexSetFilter index filter =
    do cFilter <- marshalIndexFilter filter
       destroyNotify <- mkFunPtrDestroyNotify cFilter
       {# call index_set_filter_full #} (toIndex index) cFilter nullPtr destroyNotify

indexGetWriterId :: IndexClass index
                 => index
                 -> Object
                 -> IO (Maybe Int)
indexGetWriterId index writer =
    alloca $ \idPtr ->
        do result <- {# call index_get_writer_id #} (toIndex index) (toObject writer) idPtr
           if toBool result
               then liftM (Just . fromIntegral) $ peek idPtr
               else return Nothing

indexAddFormat :: IndexClass index
               => index
               -> Int
               -> Format
               -> IO IndexEntry
indexAddFormat index id format =
    {# call index_add_format #} (toIndex index)
                                (fromIntegral id)
                                (cFromEnum format) >>=
        peekIndexEntry

indexAddAssociations :: IndexClass index
                     => index
                     -> Int
                     -> [AssocFlags]
                     -> [IndexAssociation]
                     -> IO IndexEntry
indexAddAssociations index id flags associations =
    withArrayLen associations $ \numAssociations cAssociations ->
        {# call index_add_associationv #} (toIndex index)
                                          (fromIntegral id)
                                          (fromIntegral $ fromFlags flags)
                                          (fromIntegral numAssociations)
                                          (castPtr cAssociations) >>=
            peekIndexEntry

indexAddId :: IndexClass index
           => index
           -> Int
           -> String
           -> IO IndexEntry
indexAddId index id description =
    withUTFString description
                  ({# call index_add_id #} (toIndex index) $ fromIntegral id) >>=
        peekIndexEntry

indexGetAssocEntry :: IndexClass index
                   => index
                   -> Int
                   -> IndexLookupMethod
                   -> [AssocFlags]
                   -> Format
                   -> Int64
                   -> IO (Maybe IndexEntry)
indexGetAssocEntry index id method flags format value =
    {# call index_get_assoc_entry #} (toIndex index)
                                     (fromIntegral id)
                                     (cFromEnum method)
                                     (fromIntegral $ fromFlags flags)
                                     (cFromEnum format)
                                     (fromIntegral value) >>=
        maybePeek peekIndexEntry

indexEntryAssocMap :: IndexEntry
                   -> Format
                   -> Maybe Int64
indexEntryAssocMap entry format =
    unsafePerformIO $ alloca $ \valuePtr ->
        do result <- {# call gst_index_entry_assoc_map #} entry
                                                          (cFromEnum format)
                                                          valuePtr
           if toBool result
               then liftM (Just . fromIntegral) $ peek valuePtr
               else return Nothing

onIndexEntryAdded, afterIndexEntryAdded :: IndexClass index
                                        => index
                                        -> (IndexEntry -> IO ())
                                        -> IO (ConnectId index)
onIndexEntryAdded =
    connect_BOXED__NONE "entry-added" peekIndexEntry False
afterIndexEntryAdded =
    connect_BOXED__NONE "entry-added" peekIndexEntry True
