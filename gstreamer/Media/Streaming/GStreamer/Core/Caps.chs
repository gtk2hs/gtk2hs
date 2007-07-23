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
module Media.Streaming.GStreamer.Core.Caps (
  
  Caps,
  capsNone,
  capsAny,
  capsSize,
  capsGetStructure,
  capsIsEmpty,
  capsIsFixed,
  capsIsEqual,
  capsIsEqualFixed,
  capsIsAlwaysCompatible, 
  capsIsSubset,
  capsIntersect,
  capsUnion,
  capsSubtract,
  capsNormalize,
  capsFromString,
  capsToString,
  
  CapsM,
  capsCreate,
  capsModify,
  capsAppend,
  capsMerge,
  capsAppendStructure,
  capsMergeStructure,
  capsRemoveStructure,
  capsTruncate
  
  ) where

{# context lib = "gstreamer" prefix = "gst" #}

import Control.Monad (liftM)
import System.Glib.FFI
import System.Glib.UTFString
{#import Media.Streaming.GStreamer.Core.Types#}

capsNone :: Caps
capsNone =
    unsafePerformIO $ {# call caps_new_empty #} >>= takeCaps

capsAny :: Caps
capsAny =
    unsafePerformIO $ {# call caps_new_any #} >>= takeCaps

capsSize :: Caps
         -> Word
capsSize caps =
    fromIntegral $ unsafePerformIO $ {# call caps_get_size #} caps

capsGetStructure :: Caps
                 -> Word
                 -> Maybe Structure
capsGetStructure caps index =
    unsafePerformIO $
        {# call caps_get_structure #} caps (fromIntegral index) >>=
            maybePeek peekStructure

capsIsEmpty :: Caps
            -> Bool
capsIsEmpty caps =
    toBool $ unsafePerformIO $
        {# call caps_is_empty #} caps

capsIsFixed :: Caps
            -> Bool
capsIsFixed caps =
    toBool $ unsafePerformIO $
        {# call caps_is_fixed #} caps

capsIsEqual :: Caps
            -> Caps
            -> Bool
capsIsEqual caps1 caps2 =
    toBool $ unsafePerformIO $
        {# call caps_is_equal #} caps1 caps2

instance Eq Caps where
    (==) = capsIsEqual

capsIsEqualFixed :: Caps
                 -> Caps
                 -> Bool
capsIsEqualFixed caps1 caps2 =
    toBool $ unsafePerformIO $
        {# call caps_is_equal_fixed #} caps1 caps2

capsIsAlwaysCompatible :: Caps
                       -> Caps
                       -> Bool
capsIsAlwaysCompatible caps1 caps2 =
    toBool $ unsafePerformIO $
        {# call caps_is_always_compatible #} caps1 caps2

capsIsSubset :: Caps
             -> Caps
             -> Bool
capsIsSubset caps1 caps2 =
    toBool $ unsafePerformIO $
        {# call caps_is_subset #} caps1 caps2

capsIntersect :: Caps
             -> Caps
             -> Caps
capsIntersect caps1 caps2 =
    unsafePerformIO $
        {# call caps_intersect #} caps1 caps2 >>=
            takeCaps

capsUnion :: Caps
          -> Caps
          -> Caps
capsUnion caps1 caps2 =
    unsafePerformIO $
        {# call caps_union #} caps1 caps2 >>=
            takeCaps

capsSubtract :: Caps
             -> Caps
             -> Caps
capsSubtract caps1 caps2 =
    unsafePerformIO $
        {# call caps_subtract #} caps1 caps2 >>=
            takeCaps

capsNormalize :: Caps
              -> Caps
capsNormalize caps =
    unsafePerformIO $
        {# call caps_normalize #} caps >>= takeCaps

capsToString :: Caps
             -> String
capsToString caps =
    unsafePerformIO $
        {# call caps_to_string #} caps >>= readUTFString

capsFromString :: String
               -> Caps
capsFromString string =
    unsafePerformIO $
        withUTFString string {# call caps_from_string #} >>=
            takeCaps

newtype CapsM a = CapsM (CapsMRep a)
type CapsMRep a = (Caps -> IO a)

instance Monad CapsM where
    (CapsM aM) >>= fbM =
        CapsM $ \caps ->
            do a <- aM caps
               let CapsM bM = fbM a
               bM caps
    return a = CapsM $ const $ return a

marshalCapsModify :: IO (Ptr Caps)
                  -> CapsM a
                  -> (Caps, a)
marshalCapsModify mkCaps (CapsM action) =
    unsafePerformIO $
        do ptr <- mkCaps
           caps <- liftM Caps $ newForeignPtr_ ptr
           result <- action caps
           caps' <- takeCaps ptr
           return (caps', result)

capsCreate :: CapsM a
           -> (Caps, a)
capsCreate action =
    marshalCapsModify
        {# call caps_new_empty #}
        action

capsModify :: Caps
           -> CapsM a
           -> (Caps, a)
capsModify caps action =
    marshalCapsModify
        ({# call caps_copy #} caps)
        action

capsAppend :: Caps
           -> CapsM ()
capsAppend caps2 =
    CapsM $ \caps1 ->
        {# call caps_copy #} caps2 >>= takeCaps >>=
            {# call caps_append #} caps1

capsMerge :: Caps
          -> CapsM ()
capsMerge caps2 =
    CapsM $ \caps1 ->
        {# call caps_copy #} caps2 >>= takeCaps >>=
            {# call caps_merge #} caps1

capsAppendStructure :: Structure
                    -> CapsM ()
capsAppendStructure structure =
    CapsM $ \caps ->
        giveStructure structure $
            {# call caps_append_structure #} caps

capsMergeStructure :: Structure
                   -> CapsM ()
capsMergeStructure structure =
    CapsM $ \caps ->
        giveStructure structure $
            {# call caps_merge_structure #} caps

capsRemoveStructure :: Word
                    -> CapsM ()
capsRemoveStructure idx =
    CapsM $ \caps ->
        {# call caps_remove_structure #} caps $ fromIntegral idx

capsTruncate :: CapsM ()
capsTruncate =
    CapsM {# call caps_truncate #}
