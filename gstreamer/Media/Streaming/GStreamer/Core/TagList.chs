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
module Media.Streaming.GStreamer.Core.TagList (
  
  StandardTag(..),
  standardTagToString,
  tagNick,
  tagDescription,
  tagGetFlag,
  tagIsFixed,
  tagListEmpty,
  tagListIsEmpty,
  tagListMerge,
  tagListGetTagSize,
  tagListCreate,
  tagListModify,
  tagListInsert,
  tagListRemoveTag,
  tagListGetChar,
  tagListGetCharIndex,
  tagListGetUChar,
  tagListGetUCharIndex,
  tagListGetBool,
  tagListGetBoolIndex,
  tagListGetInt,
  tagListGetIntIndex,
  tagListGetUInt,
  tagListGetUIntIndex,
  tagListGetLong,
  tagListGetLongIndex,
  tagListGetULong,
  tagListGetULongIndex,
  tagListGetInt64,
  tagListGetInt64Index,
  tagListGetUInt64,
  tagListGetUInt64Index,
  tagListGetFloat,
  tagListGetFloatIndex,
  tagListGetDouble,
  tagListGetDoubleIndex,
  tagListGetString,
  tagListGetStringIndex,
  tagListGetDate,
  tagListGetDateIndex
  
  ) where

import Control.Monad (liftM)
{#import Media.Streaming.GStreamer.Core.Types#}
import System.Glib.FFI
import System.Glib.UTFString
{#import System.Glib.GDateTime#}
import Data.Char ( ord
                 , chr )

{# context lib = "gstreamer" prefix = "gst" #}

data StandardTag = StandardTagTitle
                 | StandardTagArtist
                 | StandardTagAlbum
                 | StandardTagDate
                 | StandardTagGenre
                 | StandardTagComment
                 | StandardTagExtendedComment
                 | StandardTagTrackNumber
                 | StandardTagTrackCount
                 | StandardTagAlbumVolumeNumber
                 | StandardTagVolumeCount
                 | StandardTagLocation
                 | StandardTagDescription
                 | StandardTagVersion
                 | StandardTagISRC
                 | StandardTagOrganization
                 | StandardTagCopyright
                 | StandardTagContact
                 | StandardTagLicense
                 | StandardTagPerformer
                 | StandardTagDuration
                 | StandardTagCodec
                 | StandardTagVideoCodec
                 | StandardTagAudioCodec
                 | StandardTagBitrate
                 | StandardTagNominalBitrate
                 | StandardTagMinimumBitrate
                 | StandardTagMaximumBitrate
                 | StandardTagSerial
                 | StandardTagEncoder
                 | StandardTagEncoderVersion
                 | StandardTagTrackGain
                 | StandardTagTrackPeak
                 | StandardTagAlbumGain
                 | StandardTagAlbumPeak
                 | StandardTagReferenceLevel
                 | StandardTagLanguageCode
                 | StandardTagImage
                 | StandardTagPreviewImage
                 | StandardTagBeatsPerMinute
                   deriving (Eq, Enum, Bounded, Show)

standardTagToString :: StandardTag
                    -> Tag
standardTagToString StandardTagTitle             = "title"
standardTagToString StandardTagArtist            = "artist"
standardTagToString StandardTagAlbum             = "album"
standardTagToString StandardTagDate              = "date"
standardTagToString StandardTagGenre             = "genre"
standardTagToString StandardTagComment           = "comment"
standardTagToString StandardTagExtendedComment   = "extended-comment"
standardTagToString StandardTagTrackNumber       = "track-number"
standardTagToString StandardTagTrackCount        = "track-count"
standardTagToString StandardTagAlbumVolumeNumber = "album-volume-number"
standardTagToString StandardTagVolumeCount       = "album-volume-count"
standardTagToString StandardTagLocation          = "location"
standardTagToString StandardTagDescription       = "description"
standardTagToString StandardTagVersion           = "version"
standardTagToString StandardTagISRC              = "isrc"
standardTagToString StandardTagOrganization      = "organization"
standardTagToString StandardTagCopyright         = "copyright"
standardTagToString StandardTagContact           = "contact"
standardTagToString StandardTagLicense           = "license"
standardTagToString StandardTagPerformer         = "performer"
standardTagToString StandardTagDuration          = "duration"
standardTagToString StandardTagCodec             = "codec"
standardTagToString StandardTagVideoCodec        = "video-codec"
standardTagToString StandardTagAudioCodec        = "audio-codec"
standardTagToString StandardTagBitrate           = "bitrate"
standardTagToString StandardTagNominalBitrate    = "nominal-bitrate"
standardTagToString StandardTagMinimumBitrate    = "minimum-bitrate"
standardTagToString StandardTagMaximumBitrate    = "maximum-bitrate"
standardTagToString StandardTagSerial            = "serial"
standardTagToString StandardTagEncoder           = "encoder"
standardTagToString StandardTagEncoderVersion    = "encoder-version"
standardTagToString StandardTagTrackGain         = "track-gain"
standardTagToString StandardTagTrackPeak         = "track-peak"
standardTagToString StandardTagAlbumGain         = "album-gain"
standardTagToString StandardTagAlbumPeak         = "album-peak"
standardTagToString StandardTagReferenceLevel    = "reference-level"
standardTagToString StandardTagLanguageCode      = "language-code"
standardTagToString StandardTagImage             = "image"
standardTagToString StandardTagPreviewImage      = "preview-image"
standardTagToString StandardTagBeatsPerMinute    = "beats-per-minute"

tagNick :: Tag
        -> String
tagNick tag =
    unsafePerformIO $
        withUTFString tag {# call tag_get_nick #} >>= peekUTFString

tagDescription :: Tag
               -> String
tagDescription tag =
    unsafePerformIO $
        withUTFString tag {# call tag_get_description #} >>= peekUTFString

tagGetFlag :: Tag
           -> TagFlag
tagGetFlag tag =
    toTagFlag $ unsafePerformIO $
        withUTFString tag {# call tag_get_flag #}

tagIsFixed :: Tag
           -> Bool
tagIsFixed tag =
    toBool $ unsafePerformIO $
        withUTFString tag {# call tag_is_fixed #}

tagListEmpty :: TagList
tagListEmpty =
    unsafePerformIO $ {# call tag_list_new #} >>= takeTagList . castPtr

tagListIsEmpty :: TagList
               -> Bool
tagListIsEmpty tagList =
    toBool $ unsafePerformIO $ withTagList tagList $ {# call tag_list_is_empty #} . castPtr

tagListMerge :: TagList
             -> TagList
             -> TagMergeMode
             -> TagList
tagListMerge list1 list2 mode =
    unsafePerformIO $ withTagList list1 $ \listPtr1 ->
        withTagList list2 $ \listPtr2 ->
            {# call tag_list_merge #} (castPtr listPtr1)
                                      (castPtr listPtr2)
                                      (fromTagMergeMode mode) >>=
                takeTagList . castPtr

tagListGetTagSize :: TagList
                  -> Tag
                  -> Word
tagListGetTagSize tagList tag =
    fromIntegral $ unsafePerformIO $
        withUTFString tag $ \cTag ->
            withTagList tagList $ \tagListPtr ->
                {# call tag_list_get_tag_size #} (castPtr tagListPtr) cTag

tagListCreate :: StructureM a
              -> (TagList, a)
tagListCreate (StructureM action) =
    unsafePerformIO $
        do tagListPtr <- liftM castPtr {# call tag_list_new #}
           tagList <- liftM Structure $ newForeignPtr_ tagListPtr
           result <- action tagList
           tagList' <- takeTagList tagListPtr
           return (tagList', result)

tagListModify :: TagList
              -> StructureM a
              -> (TagList, a)
tagListModify tagList (StructureM action) =
    unsafePerformIO $
        do tagListPtr <- withTagList tagList $ {# call tag_list_copy #} . castPtr
           tagList' <- liftM Structure $ newForeignPtr_ $ castPtr tagListPtr
           result <- action tagList'
           tagList'' <- takeTagList $ castPtr tagListPtr
           return $ (tagList'', result)

tagListInsert :: TagList
              -> TagMergeMode
              -> StructureM ()
tagListInsert tagList1 mode =
    StructureM $ \tagList2 ->
        withTagList tagList1 $ \tagListPtr1 ->
            withTagList tagList2 $ \tagListPtr2 ->
                {# call tag_list_insert #} (castPtr tagListPtr1)
                                           (castPtr tagListPtr2)
                                           (fromTagMergeMode mode)

tagListRemoveTag :: Tag
                 -> StructureM ()
tagListRemoveTag tag =
    StructureM $ \tagList ->
        withUTFString tag $ \cTag ->
            withTagList tagList $ \tagListPtr ->
                {# call tag_list_remove_tag #} (castPtr tagListPtr)
                                               cTag

marshalTagListGet :: Storable a
                  => (Ptr () -> CString -> Ptr a -> IO {# type gboolean #})
                  -> (a -> IO b)
                  -> TagList
                  -> Tag
                  -> Maybe b
marshalTagListGet getAction convert tagList tag =
    unsafePerformIO $ alloca $ \valuePtr ->
        withTagList tagList $ \tagListPtr ->
            withUTFString tag $ \cTag ->
                do success <- getAction (castPtr tagListPtr)
                                        cTag
                                        valuePtr
                   if toBool success
                       then liftM Just $ peek valuePtr >>= convert
                       else return Nothing

marshalTagListGetIndex :: Storable a
                       => (Ptr () -> CString -> {# type guint #} -> Ptr a -> IO {# type gboolean #})
                       -> (a -> IO b)
                       -> TagList
                       -> Tag
                       -> Word
                       -> Maybe b
marshalTagListGetIndex getAction convert tagList tag index =
    unsafePerformIO $ alloca $ \valuePtr ->
        withTagList tagList $ \tagListPtr ->
            withUTFString tag $ \cTag ->
                do success <- getAction (castPtr tagListPtr)
                                        cTag
                                        (fromIntegral index)
                                        valuePtr
                   if toBool success
                       then liftM Just $ peek valuePtr >>= convert
                       else return Nothing

tagListGetChar :: TagList
               -> Tag
               -> Maybe Char
tagListGetChar =
    marshalTagListGet {# call tag_list_get_char #} $ return . chr . fromIntegral

tagListGetCharIndex :: TagList
                    -> Tag
                    -> Word
                    -> Maybe Char
tagListGetCharIndex =
    marshalTagListGetIndex {# call tag_list_get_char_index #} $ return . chr . fromIntegral

tagListGetUChar :: TagList
                -> Tag
                -> Maybe Word8
tagListGetUChar =
    marshalTagListGet {# call tag_list_get_uchar #} $ return . fromIntegral

tagListGetUCharIndex :: TagList
                     -> Tag
                     -> Word
                     -> Maybe Word8
tagListGetUCharIndex =
    marshalTagListGetIndex {# call tag_list_get_uchar_index #} $ return . fromIntegral

tagListGetBool :: TagList
               -> Tag
               -> Maybe Bool
tagListGetBool =
    marshalTagListGet {# call tag_list_get_boolean #} $ return . toBool

tagListGetBoolIndex :: TagList
                    -> Tag
                    -> Word
                    -> Maybe Bool
tagListGetBoolIndex =
    marshalTagListGetIndex {# call tag_list_get_boolean_index #} $ return . toBool

tagListGetInt :: TagList
              -> Tag
              -> Maybe Int
tagListGetInt =
    marshalTagListGet {# call tag_list_get_int #} $ return . fromIntegral

tagListGetIntIndex :: TagList
                   -> Tag
                   -> Word
                   -> Maybe Int
tagListGetIntIndex =
    marshalTagListGetIndex {# call tag_list_get_int_index #} $ return . fromIntegral

tagListGetUInt :: TagList
               -> Tag
               -> Maybe Word
tagListGetUInt =
    marshalTagListGet {# call tag_list_get_uint #} $ return . fromIntegral

tagListGetUIntIndex :: TagList
                    -> Tag
                    -> Word
                    -> Maybe Word
tagListGetUIntIndex =
    marshalTagListGetIndex {# call tag_list_get_uint_index #} $ return . fromIntegral

tagListGetLong :: TagList
               -> Tag
               -> Maybe Int64
tagListGetLong =
    marshalTagListGet {# call tag_list_get_long #} $ return . fromIntegral

tagListGetLongIndex :: TagList
                    -> Tag
                    -> Word
                    -> Maybe Int64
tagListGetLongIndex =
    marshalTagListGetIndex {# call tag_list_get_long_index #} $ return . fromIntegral

tagListGetULong :: TagList
                -> Tag
                -> Maybe Word64
tagListGetULong =
    marshalTagListGet {# call tag_list_get_ulong #} $ return . fromIntegral

tagListGetULongIndex :: TagList
                     -> Tag
                     -> Word
                     -> Maybe Word64
tagListGetULongIndex =
    marshalTagListGetIndex {# call tag_list_get_ulong_index #} $ return . fromIntegral


tagListGetInt64 :: TagList
                -> Tag
                -> Maybe Int64
tagListGetInt64 =
    marshalTagListGet {# call tag_list_get_int64 #} $ return . fromIntegral

tagListGetInt64Index :: TagList
                     -> Tag
                     -> Word
                     -> Maybe Int64
tagListGetInt64Index =
    marshalTagListGetIndex {# call tag_list_get_int64_index #} $ return . fromIntegral

tagListGetUInt64 :: TagList
                 -> Tag
                 -> Maybe Word64
tagListGetUInt64 =
    marshalTagListGet {# call tag_list_get_uint64 #} $ return . fromIntegral

tagListGetUInt64Index :: TagList
                      -> Tag
                      -> Word
                      -> Maybe Word64
tagListGetUInt64Index =
    marshalTagListGetIndex {# call tag_list_get_uint64_index #} $ return . fromIntegral

tagListGetFloat :: TagList
                -> Tag
                -> Maybe Float
tagListGetFloat =
    marshalTagListGet {# call tag_list_get_float #} $ return . realToFrac

tagListGetFloatIndex :: TagList
                     -> Tag
                     -> Word
                     -> Maybe Float
tagListGetFloatIndex =
    marshalTagListGetIndex {# call tag_list_get_float_index #} $ return . realToFrac

tagListGetDouble :: TagList
                 -> Tag
                 -> Maybe Double
tagListGetDouble =
    marshalTagListGet {# call tag_list_get_double #} $ return . realToFrac

tagListGetDoubleIndex :: TagList
                      -> Tag
                      -> Word
                      -> Maybe Double
tagListGetDoubleIndex =
    marshalTagListGetIndex {# call tag_list_get_double_index #} $ return . realToFrac

tagListGetString :: TagList
                 -> Tag
                 -> Maybe String
tagListGetString =
    marshalTagListGet {# call tag_list_get_string #} readUTFString

tagListGetStringIndex :: TagList
                      -> Tag
                      -> Word
                      -> Maybe String
tagListGetStringIndex =
    marshalTagListGetIndex {# call tag_list_get_string_index #} readUTFString

tagListGetDate :: TagList
                 -> Tag
                 -> Maybe GDate
tagListGetDate =
    marshalTagListGet {# call tag_list_get_date #} $ peek . castPtr

tagListGetDateIndex :: TagList
                      -> Tag
                      -> Word
                      -> Maybe GDate
tagListGetDateIndex =
    marshalTagListGetIndex {# call tag_list_get_date_index #} $ peek . castPtr

