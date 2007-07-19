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
module Media.Streaming.GStreamer.Index (
  ) where

import Control.Monad (liftM)
{#import Media.Streaming.GStreamer.Types#}
{#import Media.Streaming.GStreamer.Signals#}
import System.Glib.Flags
import System.Glib.FFI
import System.Glib.UTFString
{#import System.Glib.Signals#}
{#import System.Glib.GObject#} (mkFunPtrDestroyNotify)

{# context lib = "gstreamer" prefix = "gst" #}

indexNew :: IO Index
indexNew =
    {# call index_new #} >>= newIndex

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
    {# call index_set_certainty #} (toIndex index) $ fromIndexCertainty certainty

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
              do index <- newIndex_ cIndex
                 indexEntry <- newIndexEntry_ cIndexEntry
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
                                (fromFormat format) >>=
        newIndexEntry_

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
            newIndexEntry_

indexAddId :: IndexClass index
           => index
           -> Int
           -> String
           -> IO IndexEntry
indexAddId index id description =
    withUTFString description
                  ({# call index_add_id #} (toIndex index) $ fromIntegral id) >>=
        newIndexEntry_

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
                                     (fromIndexLookupMethod method)
                                     (fromIntegral $ fromFlags flags)
                                     (fromFormat format)
                                     (fromIntegral value) >>=
        maybePeek newIndexEntry_

indexEntryAssocMap :: IndexEntry
                   -> Format
                   -> Maybe Int64
indexEntryAssocMap entry format =
    unsafePerformIO $ alloca $ \valuePtr ->
        do result <- {# call gst_index_entry_assoc_map #} entry
                                                          (fromFormat format)
                                                          valuePtr
           if toBool result
               then liftM (Just . fromIntegral) $ peek valuePtr
               else return Nothing

onIndexEntryAdded, afterIndexEntryAdded :: IndexClass index
                                        => index
                                        -> (IndexEntry -> IO ())
                                        -> IO (ConnectId index)
onIndexEntryAdded =
    connect_BOXED__NONE "entry-added" newIndexEntry_ False
afterIndexEntryAdded =
    connect_BOXED__NONE "entry-added" newIndexEntry_ True
