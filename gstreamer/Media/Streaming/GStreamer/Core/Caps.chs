{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
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
--  A structure describing sets of media formats.
module Media.Streaming.GStreamer.Core.Caps (
  
-- * Detail
  -- | 'Caps' (short for /capabilities/) are lightweight objects
  --   describing media types. They are composed of arrays of
  --   'Structure's.
  --   
  --   'Caps' are exposed on 'PadTemplate's to describe all the
  --   possible types a given 'Pad' can handle. They are also stored
  --   in the 'Registry' along with the description of an 'Element'.
  --   
  --   'Caps' can be retrieved from an 'Element'\'s 'Pad's using the
  --   'padGetCaps' function. The returned 'Caps' describes the possible
  --   types that the pad can handle or produce at runtime.
  --   
  --   'Caps' are also attached to 'Buffers' to describe the type of
  --   the contained data using the function 'bufferSetCaps'. 'Caps'
  --   attached to a buffer allow for format negotiation upstream and
  --   downstream.
  --   
  --   'Caps' are /fixed/ when they have no properties with ranges or
  --   lists. Use 'capsIsFixed' to test for fixed caps. Only fixed
  --   caps may be set on a 'Pad' or 'Buffer'.

-- * Types
  Caps,
  capsNone,
  capsAny,

-- * Caps Operations
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

-- * Caps Mutation
  CapsM,
  capsCreate,
  capsModify,
  capsAppendStructure,
#if GST_CHECK_VERSION(0,10,10)
  capsMergeStructure,
  capsRemoveStructure,
#endif
  capsTruncate
  
  ) where

{# context lib = "gstreamer" prefix = "gst" #}

import Control.Monad (liftM)
import Control.Monad.Reader
import System.Glib.FFI
import System.Glib.UTFString
{#import Media.Streaming.GStreamer.Core.Types#}

-- | A 'Caps' that represents an undefined media type.
capsNone :: Caps
capsNone =
    unsafePerformIO $ {# call caps_new_empty #} >>= takeCaps

-- | A 'Caps' that represents all possible media types.
capsAny :: Caps
capsAny =
    unsafePerformIO $ {# call caps_new_any #} >>= takeCaps

-- | Get the number of structures contained in the 'Caps'.
capsSize :: Caps -- ^ @caps@ - a 'Caps'
         -> Word -- ^ the number of structures contained in the 'Caps'
capsSize caps =
    fromIntegral $ unsafePerformIO $ {# call caps_get_size #} caps

-- | Get the 'Structure' at the given index.
capsGetStructure :: Caps            -- ^ @caps@ - a 'Caps'
                 -> Word            -- ^ @index@ - the index of the 'Structure'
                 -> Maybe Structure -- ^ the 'Structure' at the given index, or 'Nothing'
                                    --   if @index@ is invalid
capsGetStructure caps index =
    unsafePerformIO $
        {# call caps_get_structure #} caps (fromIntegral index) >>=
            maybePeek peekStructure

-- | Create a new 'Caps' containing only the 'Structure' at the given
--   index of the caps.
capsCopyNth :: Caps       -- ^ @caps@ - a 'Caps'
            -> Word       -- ^ @index@ - the index of the 'Structure'
            -> Maybe Caps -- ^ the new 'Caps', or 'Nothing'
                          --   if @index@ is invalid
capsCopyNth caps index =
    unsafePerformIO $
        {# call caps_copy_nth #} caps (fromIntegral index) >>=
            maybePeek takeCaps

-- | Determine whether @caps@ represents no media formats.
capsIsEmpty :: Caps -- ^ @caps@ - a 'Caps'
            -> Bool -- ^ 'True' if @caps@ is empty, otherwise 'False'
capsIsEmpty caps =
    toBool $ unsafePerformIO $
        {# call caps_is_empty #} caps

-- | Determine whether the @caps@ is fixed; that is, if it has exactly
--   one structure, and each field in the structure describes a fixed type.
capsIsFixed :: Caps -- ^ @caps@ - a 'Caps'
            -> Bool -- ^ 'True' if @caps@ is fixed, otherwise 'False'
capsIsFixed caps =
    toBool $ unsafePerformIO $
        {# call caps_is_fixed #} caps

-- | Returns 'True' if the caps represent the same set of capabilities.
--   
--   This function does not work reliably if optional properties for
--   'Caps' are included on one 'Caps' but omitted on the other.
capsIsEqual :: Caps -- ^ @caps1@ - the first 'Caps'
            -> Caps -- ^ @caps2@ - the second 'Caps'
            -> Bool -- ^ 'True' if both 'Caps' represent the same set
                    --   of capabilities.
capsIsEqual caps1 caps2 =
    toBool $ unsafePerformIO $
        {# call caps_is_equal #} caps1 caps2

instance Eq Caps where
    (==) = capsIsEqual

-- | Returns 'True' if the caps are equal.  The caps must both be
--   fixed.
capsIsEqualFixed :: Caps -- ^ @caps1@ - the first 'Caps'
                 -> Caps -- ^ @caps2@ - the second 'Caps'
                 -> Bool -- ^ 'True' if both 'Caps' represent the same set
                         --   of capabilities
capsIsEqualFixed caps1 caps2 =
    toBool $ unsafePerformIO $
        {# call caps_is_equal_fixed #} caps1 caps2

-- | Returns 'True' if every media format in the first caps is also
--   contained by the second. That is, the first is a subset of the
--   second.
capsIsAlwaysCompatible :: Caps -- ^ @caps1@ - the first 'Caps'
                       -> Caps -- ^ @caps2@ - the second 'Caps'
                       -> Bool -- ^ 'True' if @caps1@ is a subset of @caps2@, otherwise 'False'
capsIsAlwaysCompatible caps1 caps2 =
    toBool $ unsafePerformIO $
        {# call caps_is_always_compatible #} caps1 caps2

-- | Returns 'True' if all caps represented by the first argument are
--   also represented by the second.
--   
--   This function does not work reliably if optional properties for
--   caps are included on one caps and omitted on the other.
capsIsSubset :: Caps -- ^ @caps1@ - the first 'Caps'
             -> Caps -- ^ @caps2@ - the second 'Caps'
             -> Bool -- ^ 'True' if @caps1@ is a subset of @caps2@, otherwise 'False'
capsIsSubset caps1 caps2 =
    toBool $ unsafePerformIO $
        {# call caps_is_subset #} caps1 caps2

-- | Creates a new caps containing all the formats that are common to
--   both of the caps.
capsIntersect :: Caps -- ^ @caps1@ - the first 'Caps'
              -> Caps -- ^ @caps2@ - the second 'Caps'
              -> Caps -- ^ a new 'Caps' containing all capabilities present
                      --   in both @caps1@ and @caps2@
capsIntersect caps1 caps2 =
    unsafePerformIO $
        {# call caps_intersect #} caps1 caps2 >>=
            takeCaps

-- | Creates a new caps containing all the formats that are common to
--   either of the caps. If either of the structures are equivalient
--   to 'capsAny', the result will be 'capsAny'.
capsUnion :: Caps -- ^ @caps1@ - the first 'Caps'
          -> Caps -- ^ @caps2@ - the second 'Caps'
          -> Caps -- ^ a new 'Caps' containing all capabilities present
                  --   in either @caps1@ and @caps2@
capsUnion caps1 caps2 =
    unsafePerformIO $
        {# call caps_union #} caps1 caps2 >>=
            takeCaps

-- | Creates a new caps containing all the formats that are in the
--   first but not the second.
capsSubtract :: Caps -- ^ @caps1@ - the first 'Caps'
             -> Caps -- ^ @caps2@ - the second 'Caps'
             -> Caps -- ^ a new 'Caps' containing all capabilities present
                     --   in @caps1@ but not @caps2@
capsSubtract caps1 caps2 =
    unsafePerformIO $
        {# call caps_subtract #} caps1 caps2 >>=
            takeCaps

-- | Creates a new caps that represents the same set of formats as the
--   argument, but that contains no lists.
capsNormalize :: Caps -- ^ @caps@ - a 'Caps'
              -> Caps -- ^ the new, normalized 'Caps'
capsNormalize caps =
    unsafePerformIO $
        {# call caps_normalize #} caps >>= takeCaps

-- | Converts the argument to a string representation. The string can
--   be converted back to a caps using 'capsFromString'.
capsToString :: Caps   -- ^ @caps@ - a 'Caps'
             -> String -- ^ the string representation of 'Caps'
capsToString caps =
    unsafePerformIO $
        {# call caps_to_string #} caps >>= readUTFString

-- | Read a caps from a string.
capsFromString :: String     -- ^ @string@ - the string representation of a 'Caps'
               -> Maybe Caps -- ^ the new 'Caps', or 'Nothing' if @string@ is invalid
capsFromString string =
    unsafePerformIO $
        withUTFString string {# call caps_from_string #} >>=
            maybePeek takeCaps

-- | A 'Monad' for sequencing modifications to a 'Caps'.
newtype CapsM a =
    CapsM (ReaderT (Ptr Caps) IO a)
    deriving (Functor, Monad)

askCapsPtr :: CapsM (Ptr Caps)
askCapsPtr = CapsM $ ask

marshalCapsModify :: IO (Ptr Caps)
                  -> CapsM a
                  -> (Caps, a)
marshalCapsModify mkCaps (CapsM action) =
    unsafePerformIO $
        do ptr <- mkCaps
           result <- runReaderT action ptr
           caps <- takeCaps ptr
           return (caps, result)

-- | Create a caps and mutate it according to the given action.
capsCreate :: CapsM a   -- ^ @mutate@ - the mutating action
           -> (Caps, a) -- ^ the new 'Caps' and the action's result
capsCreate mutate =
    marshalCapsModify
        {# call caps_new_empty #}
        mutate

-- | Copy a caps and mutate it according to the given action.
capsModify :: Caps      -- ^ @caps@ - the 'Caps' to modify
           -> CapsM a   -- ^ @mutate@ - the mutating action
           -> (Caps, a) -- ^ the new 'Caps' and the action's result
capsModify caps mutate =
    marshalCapsModify ({# call caps_copy #} caps) mutate

-- | Append the given structure to the current caps.
capsAppendStructure :: Structure -- ^ @structure@ - the 'Structure' to append to the current 'Caps'
                    -> CapsM ()
capsAppendStructure structure = do
  capsPtr <- askCapsPtr
  CapsM $ liftIO $ withStructure structure $ \structurePtr ->
      do structurePtr' <- gst_structure_copy structurePtr
         gst_caps_append_structure capsPtr structurePtr
  where _ = {# call caps_append_structure #}
        _ = {# call structure_copy #}

#if GST_CHECK_VERSION(0,10,10)
-- | Append the structure to the current caps, if it is not already
--   expressed by the caps.
--   
--   Since 0.10.10.
capsMergeStructure :: Structure -- ^ @structure@ - the 'Structure' to merge with the current 'Caps'
                   -> CapsM ()
capsMergeStructure structure = do
  capsPtr <- askCapsPtr
  CapsM $ liftIO $ withStructure structure $ \structurePtr ->
      do structurePtr' <- gst_structure_copy structurePtr
         gst_caps_merge_structure capsPtr structurePtr
  where _ = {# call caps_merge_structure #}
        _ = {# call structure_copy #}

-- | Removes the structure at the given index from the current caps.
capsRemoveStructure :: Word     -- ^ @idx@ - the index of the 'Structure' to remove
                    -> CapsM ()
capsRemoveStructure idx = do
  capsPtr <- askCapsPtr
  CapsM $ liftIO $ gst_caps_remove_structure capsPtr $ fromIntegral idx
  where _ = {# call caps_remove_structure #}
#endif

-- | Discard all but the first structure from the current caps.
capsTruncate :: CapsM ()
capsTruncate = do
  capsPtr <- askCapsPtr
  CapsM $ liftIO $ gst_caps_truncate capsPtr
  where _ = {# call caps_truncate #}
