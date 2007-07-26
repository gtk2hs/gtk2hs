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
module Media.Streaming.GStreamer.Core.Types (
  
  module Media.Streaming.GStreamer.Core.Hierarchy,
  module Media.Streaming.GStreamer.Core.MiniHierarchy,
  module Media.Streaming.GStreamer.Core.GObjectHierarchy,
  
  cToFlags,
  cFromFlags,
  cToEnum,
  cFromEnum,
  
  FourCC,
  Fraction,
  
  Format(..),
  FormatDefinition(..),
  
  mkObjectGetFlags,
  mkObjectSetFlags,
  mkObjectUnsetFlags,
  ObjectFlags(..),
  objectFlagLast,
  objectGetFlags,
  objectSetFlags,
  objectUnsetFlags,
  giveObject,
  
  PadFlags(..),
  padFlagLast,
  padGetFlags,
  padSetFlags,
  padUnsetFlags,
  PadDirection(..),
  PadPresence(..),
  PadLinkReturn(..),
  FlowReturn(..),
  ActivateMode(..),
  
  ElementFlags(..),
  elementFlagLast,
  elementGetFlags,
  elementSetFlags,
  elementUnsetFlags,
  State(..),
  StateChange(..),
  StateChangeReturn(..),
  SeekFlags(..),
  SeekType(..),
  
  PluginFilter,
  PluginFeatureFilter,
  
  BusFunc,
  BusFlags(..),
  busFlagLast,
  busGetFlags,
  busSetFlags,
  busUnsetFlags,
  BusSyncReply(..),
  
  ClockFlags(..),
  clockFlagLast,
  clockGetFlags,
  clockSetFlags,
  clockUnsetFlags,
  ClockTime,
  ClockTimeDiff,
  ClockReturn(..),
  ClockID(..),
  withClockID,
  takeClockID,
  peekClockID,
  
  IndexFlags(..),
  indexFlagLast,
  indexGetFlags,
  indexSetFlags,
  indexUnsetFlags,
  IndexCertainty(..),
  IndexEntry(..),
  takeIndexEntry,
  peekIndexEntry,
  IndexEntryType(..),
  IndexLookupMethod(..),
  IndexFilter,
  IndexAssociation(..),
  AssocFlags(..),
  
  giveMiniObject,
  
  MessageType(..),
  
  QueryType(..),
  
  PtrIterator(..),
  Iterator(..),
  IteratorItem(..),
  IteratorResult(..),
  Iteratable(..),
  IteratorFilter,
  IteratorFoldFunction,
  withIterator,
  takeIterator,
  peekIterator,
  -- giveIterator - not defined - iterators don't have a refcount and
  -- are not copyable
  
  Caps(..),
  withCaps,
  takeCaps,
  peekCaps,
  giveCaps,
  
  Structure(..),
  StructureForeachFunc,
  withStructure,
  takeStructure,
  peekStructure,
  giveStructure,
  StructureM(..),
  StructureMRep,
  
  TagList,
  withTagList,
  takeTagList,
  peekTagList,
  giveTagList,
  Tag,
  TagFlag,
  TagMergeMode,
  
  Segment(..),
  
  ) where

import Control.Monad       ( liftM )
import Data.Bits           ( shiftL
                           , bit
                           , (.|.) )
import Data.Ratio          ( Ratio )
import System.Glib.FFI
import System.Glib.Flags
{#import System.Glib.GType#}
{#import System.Glib.GObject#}
{#import System.Glib.GValue#}
{#import System.Glib.GType#}
import System.Glib.UTFString
import GHC.Base            ( unsafeCoerce# )
{#import Media.Streaming.GStreamer.Core.Hierarchy#}
{#import Media.Streaming.GStreamer.Core.MiniHierarchy#}
{#import Media.Streaming.GStreamer.Core.GObjectHierarchy#}

{# context lib = "gstreamer" prefix = "gst" #}

type FourCC = Word32
type Fraction = Ratio Int

{# enum GstParseError as ParseError {underscoreToCase} with prefix = "GST" deriving (Eq) #}

cToFlags :: (Integral int, Flags flags)
         => int
         -> [flags]
cToFlags = toFlags . fromIntegral
cFromFlags :: (Integral int, Flags flags)
           => [flags]
           -> int
cFromFlags = fromIntegral . fromFlags

cToEnum :: (Integral int, Enum enum)
        => int
        -> enum
cToEnum = toEnum . fromIntegral
cFromEnum :: (Integral int, Enum enum)
           => enum
           -> int
cFromEnum = fromIntegral . fromEnum

--------------------------------------------------------------------

{# enum GstFormat as Format {underscoreToCase} with prefix = "GST" deriving (Eq) #}

data FormatDefinition = FormatDefinition { formatValue       :: Format
                                         , formatNick        :: String
                                         , formatDescription :: String
                                         , formatQuark       :: Quark }
instance Storable FormatDefinition where
    sizeOf = undefined
    alignment = undefined
    peek ptr =
        do value       <- liftM cToEnum $ {# get GstFormatDefinition->value #} ptr
           nick        <- {# get GstFormatDefinition->nick #} ptr >>= peekUTFString
           description <- {# get GstFormatDefinition->description #} ptr >>= peekUTFString
           quark       <- {# get GstFormatDefinition->quark #} ptr
           return $ FormatDefinition value nick description quark
    poke = undefined
instance Iteratable FormatDefinition where
    peekIteratable = peek . castPtr
    withIteratable = with

--------------------------------------------------------------------

mkObjectGetFlags :: (ObjectClass objectT, Flags flagsT)
                 => objectT
                 -> IO [flagsT]
mkObjectGetFlags object =
    liftM (toFlags . fromIntegral) $
        withObject object cObjectGetFlags
foreign import ccall unsafe "_hs_gst_object_flags"
    cObjectGetFlags :: Ptr Object
                    -> IO CUInt

mkObjectSetFlags :: (ObjectClass objectT, Flags flagsT)
                 => objectT
                 -> [flagsT]
                 -> IO ()
mkObjectSetFlags object flags =
    withObject object $ \cObject ->
        cObjectSetFlags cObject (fromIntegral $ fromFlags flags)
foreign import ccall unsafe "_hs_gst_object_flag_set"
    cObjectSetFlags :: Ptr Object
                    -> CUInt
                    -> IO ()

mkObjectUnsetFlags :: (ObjectClass objectT, Flags flagsT)
                   => objectT
                   -> [flagsT]
                   -> IO ()
mkObjectUnsetFlags object flags =
    withObject object $ \cObject ->
        cObjectUnsetFlags cObject (fromIntegral $ fromFlags flags)
foreign import ccall unsafe "_hs_gst_object_flag_unset"
    cObjectUnsetFlags :: Ptr Object
                    -> CUInt
                    -> IO ()

-- | Use 'giveObject' to pass an object to a function that takes
--   ownership of it.
giveObject :: ObjectClass obj
           => obj
           -> (obj -> IO a)
           -> IO a
giveObject obj action =
    do withObject (toObject obj) $ {# call gst_object_ref #} . castPtr
       action obj

data ObjectFlags = ObjectDisposing
                   deriving (Bounded)
instance Enum ObjectFlags where
    toEnum n | n == (shiftL 1 0) = ObjectDisposing
    fromEnum ObjectDisposing = (shiftL 1 0)
instance Flags ObjectFlags
objectFlagLast :: Int
objectFlagLast = shiftL 1 4

objectGetFlags :: ObjectClass objectT
               => objectT
               -> IO [ObjectFlags]
objectGetFlags = mkObjectGetFlags

objectSetFlags :: ObjectClass objectT
               => objectT
               -> [ObjectFlags]
               -> IO ()
objectSetFlags = mkObjectSetFlags

objectUnsetFlags :: ObjectClass objectT
                 => objectT
                 -> [ObjectFlags]
                 -> IO ()
objectUnsetFlags = mkObjectUnsetFlags

--------------------------------------------------------------------

data PadFlags = PadBlocked
              | PadFlushing
              | PadInGetcaps
              | PadInSetcaps
              | PadBlocking
                deriving (Eq, Bounded)
instance Enum PadFlags where
    toEnum n | n == (shiftL objectFlagLast 0) = PadBlocked
             | n == (shiftL objectFlagLast 1) = PadFlushing
             | n == (shiftL objectFlagLast 2) = PadInGetcaps
             | n == (shiftL objectFlagLast 3) = PadInSetcaps
             | n == (shiftL objectFlagLast 4) = PadBlocking
    fromEnum PadBlocked   = shiftL objectFlagLast 0
    fromEnum PadFlushing  = shiftL objectFlagLast 1
    fromEnum PadInGetcaps = shiftL objectFlagLast 2
    fromEnum PadInSetcaps = shiftL objectFlagLast 3
    fromEnum PadBlocking  = shiftL objectFlagLast 4
instance Flags PadFlags
padFlagLast :: Int
padFlagLast = shiftL objectFlagLast 8

padGetFlags :: PadClass padT
            => padT
            -> IO [PadFlags]
padGetFlags = mkObjectGetFlags

padSetFlags :: PadClass padT
            => padT
            -> [PadFlags]
            -> IO ()
padSetFlags = mkObjectSetFlags

padUnsetFlags :: PadClass padT
              => padT
              -> [PadFlags]
              -> IO ()
padUnsetFlags = mkObjectUnsetFlags

{# enum GstPadDirection  as PadDirection  {underscoreToCase} with prefix = "GST" deriving (Eq, Show) #}
{# enum GstPadPresence   as PadPresence   {underscoreToCase} with prefix = "GST" deriving (Eq, Show) #}
{# enum GstPadLinkReturn as PadLinkReturn {underscoreToCase} with prefix = "GST" deriving (Eq, Show) #}
{# enum GstFlowReturn    as FlowReturn    {underscoreToCase} with prefix = "GST" deriving (Eq, Show) #}
{# enum GstActivateMode  as ActivateMode  {underscoreToCase} with prefix = "GST" deriving (Eq, Show) #}

instance Iteratable Pad where
    peekIteratable = peekPad . castPtr
    withIteratable = withPad

--------------------------------------------------------------------

{# enum GstPluginError as PluginError {underscoreToCase} with prefix = "GST" deriving (Eq, Show) #}

--------------------------------------------------------------------

data ElementFlags = ElementLockedState
                  | ElementIsSink
                  | ElementUnparenting
                    deriving (Bounded)
instance Enum ElementFlags where
    toEnum n | n == (shiftL objectFlagLast 0) = ElementLockedState
             | n == (shiftL objectFlagLast 1) = ElementIsSink
             | n == (shiftL objectFlagLast 2) = ElementUnparenting
    fromEnum ElementLockedState = (shiftL objectFlagLast 0)
    fromEnum ElementIsSink      = (shiftL objectFlagLast 1)
    fromEnum ElementUnparenting = (shiftL objectFlagLast 2)
instance Flags ElementFlags
elementFlagLast :: Int
elementFlagLast = shiftL objectFlagLast 16

elementGetFlags :: ElementClass elementT
                => elementT
                -> IO [ElementFlags]
elementGetFlags = mkObjectGetFlags

elementSetFlags :: ElementClass elementT
                => elementT
                -> [ElementFlags]
                -> IO ()
elementSetFlags = mkObjectSetFlags

elementUnsetFlags :: ElementClass elementT
                  => elementT
                  -> [ElementFlags]
                  -> IO ()
elementUnsetFlags = mkObjectUnsetFlags

{# enum GstSeekFlags as SeekFlags {underscoreToCase} with prefix = "GST" deriving (Eq, Bounded) #}
instance Flags SeekFlags
{# enum GstSeekType as SeekType {underscoreToCase} with prefix = "GST" deriving (Eq, Bounded) #}
{# enum GstState as State {underscoreToCase} with prefix = "GST" deriving (Eq) #}
data StateChange = StateChangeNullToReady
                 | StateChangeReadyToPaused
                 | StateChangePausedToPlaying
                 | StateChangePlayingToPaused
                 | StateChangePausedToReady
                 | StateChangeReadyToNull
mkStateChange :: State
              -> State
              -> Int
mkStateChange from to =
    (shiftL (fromEnum from) 3) .|. (fromEnum to)
instance Enum StateChange where
    toEnum n | n == (mkStateChange StateNull     StateReady)   = StateChangeNullToReady
             | n == (mkStateChange StateReady    StatePaused)  = StateChangeReadyToPaused
             | n == (mkStateChange StatePaused   StatePlaying) = StateChangePausedToPlaying
             | n == (mkStateChange StatePlaying  StatePaused)  = StateChangePlayingToPaused
             | n == (mkStateChange StatePaused   StateReady)   = StateChangePausedToReady
             | n == (mkStateChange StateReady    StateNull)    = StateChangeReadyToNull
    
    fromEnum StateChangeNullToReady     = mkStateChange StateNull     StateReady
    fromEnum StateChangeReadyToPaused   = mkStateChange StateReady    StatePaused
    fromEnum StateChangePausedToPlaying = mkStateChange StatePaused   StatePlaying
    fromEnum StateChangePlayingToPaused = mkStateChange StatePlaying  StatePaused
    fromEnum StateChangePausedToReady   = mkStateChange StatePaused   StatePlaying
    fromEnum StateChangeReadyToNull     = mkStateChange StateReady    StateNull

{# enum GstStateChangeReturn as StateChangeReturn {underscoreToCase} with prefix = "GST" deriving (Eq) #}

instance Iteratable Element where
    peekIteratable = peekElement . castPtr
    withIteratable = withElement

--------------------------------------------------------------------

type PluginFilter = Plugin -> IO Bool
type PluginFeatureFilter = PluginFeature -> IO Bool

--------------------------------------------------------------------

data BusFlags = BusFlushing
                deriving (Eq, Bounded)
instance Enum BusFlags where
    toEnum n | n == (shiftL objectFlagLast 0) = BusFlushing
    fromEnum BusFlushing  = shiftL objectFlagLast 0
instance Flags BusFlags
busFlagLast :: Int
busFlagLast = shiftL objectFlagLast 1

busGetFlags :: BusClass busT
            => busT
            -> IO [BusFlags]
busGetFlags = mkObjectGetFlags

busSetFlags :: BusClass busT
            => busT
            -> [BusFlags]
            -> IO ()
busSetFlags = mkObjectSetFlags

busUnsetFlags :: BusClass busT
              => busT
              -> [BusFlags]
              -> IO ()
busUnsetFlags = mkObjectUnsetFlags

{# enum GstBusSyncReply as BusSyncReply {underscoreToCase} with prefix = "GST" deriving (Eq) #}

type BusFunc =  Bus
             -> Message
             -> IO Bool

--------------------------------------------------------------------

data ClockFlags = ClockCanDoSingleSync
                | ClockCanDoSingleAsync
                | ClockCanDoPeriodicSync
                | ClockCanDoPeriodicAsync
                | ClockCanSetResolution
                | ClockCanSetMaster
                  deriving (Eq, Bounded)
instance Enum ClockFlags where
    toEnum n | n == (shiftL objectFlagLast 0) = ClockCanDoSingleSync
             | n == (shiftL objectFlagLast 1) = ClockCanDoSingleAsync
             | n == (shiftL objectFlagLast 2) = ClockCanDoPeriodicSync
             | n == (shiftL objectFlagLast 3) = ClockCanDoPeriodicAsync
             | n == (shiftL objectFlagLast 4) = ClockCanSetResolution
             | n == (shiftL objectFlagLast 5) = ClockCanSetMaster
    
    fromEnum ClockCanDoSingleSync    = shiftL objectFlagLast 0
    fromEnum ClockCanDoSingleAsync   = shiftL objectFlagLast 1
    fromEnum ClockCanDoPeriodicSync  = shiftL objectFlagLast 2
    fromEnum ClockCanDoPeriodicAsync = shiftL objectFlagLast 3
    fromEnum ClockCanSetResolution   = shiftL objectFlagLast 4
    fromEnum ClockCanSetMaster       = shiftL objectFlagLast 5
instance Flags ClockFlags
clockFlagLast :: Int
clockFlagLast = shiftL objectFlagLast 8

clockGetFlags :: ClockClass clockT
              => clockT
              -> IO [ClockFlags]
clockGetFlags = mkObjectGetFlags

clockSetFlags :: ClockClass clockT
              => clockT
              -> [ClockFlags]
              -> IO ()
clockSetFlags = mkObjectSetFlags

clockUnsetFlags :: ClockClass clockT
                => clockT
                -> [ClockFlags]
                -> IO ()
clockUnsetFlags = mkObjectUnsetFlags

type ClockTime = Word64
type ClockTimeDiff = Int64

{# enum GstClockReturn as ClockReturn {underscoreToCase} with prefix = "GST" deriving (Eq) #}

{# pointer *GstClockID as ClockID foreign newtype #}
withClockID :: ClockID
            -> (Ptr ClockID -> IO a)
            -> IO a
withClockID (ClockID clockID) = withForeignPtr clockID
takeClockID, peekClockID :: Ptr ClockID
                        -> IO ClockID
takeClockID clockIDPtr =
    liftM ClockID $ newForeignPtr clockIDPtr clockIDFinalizer
peekClockID clockIDPtr =
    do {# call clock_id_ref #} $ castPtr clockIDPtr
       takeClockID clockIDPtr

foreign import ccall unsafe "&gst_clock_id_unref"
    clockIDFinalizer :: FunPtr (Ptr ClockID -> IO ())

--------------------------------------------------------------------

data IndexFlags = IndexWritable
                | IndexReadable
                    deriving (Bounded)
instance Enum IndexFlags where
    toEnum n | n == (shiftL objectFlagLast 0) = IndexWritable
             | n == (shiftL objectFlagLast 1) = IndexReadable
    fromEnum IndexWritable = (shiftL objectFlagLast 0)
    fromEnum IndexReadable = (shiftL objectFlagLast 1)
instance Flags IndexFlags
indexFlagLast :: Int
indexFlagLast = shiftL objectFlagLast 8

indexGetFlags :: IndexClass indexT
              => indexT
              -> IO [IndexFlags]
indexGetFlags = mkObjectGetFlags

indexSetFlags :: IndexClass indexT
              => indexT
              -> [IndexFlags]
              -> IO ()
indexSetFlags = mkObjectSetFlags

indexUnsetFlags :: IndexClass indexT
                => indexT
                -> [IndexFlags]
                -> IO ()
indexUnsetFlags = mkObjectUnsetFlags

{# enum GstIndexCertainty as IndexCertainty {underscoreToCase} with prefix = "GST" deriving (Eq) #}
{# enum GstIndexEntryType as IndexEntryType {underscoreToCase} with prefix = "GST" deriving (Eq) #}
{# enum GstIndexLookupMethod as IndexLookupMethod {underscoreToCase} with prefix = "GST" deriving (Eq) #}

{# pointer *GstIndexEntry as IndexEntry foreign newtype #}

takeIndexEntry :: Ptr IndexEntry
              -> IO IndexEntry
takeIndexEntry ptr =
    liftM IndexEntry $ newForeignPtr ptr indexEntryFinalizer
foreign import ccall unsafe "&gst_index_entry_free"
    indexEntryFinalizer :: FunPtr (Ptr IndexEntry -> IO ())
peekIndexEntry :: Ptr IndexEntry
               -> IO IndexEntry
peekIndexEntry ptr =
    (liftM IndexEntry $ newForeignPtr_ ptr) >>=
        {# call index_entry_copy #} >>=
            takeIndexEntry

type IndexFilter  = Index
                 -> IndexEntry
                 -> IO Bool

data IndexAssociation = IndexAssociation Format Int64
instance Storable IndexAssociation where
    sizeOf _ = {# sizeof GstIndexAssociation #}
    alignment _ = alignment (undefined :: CString)
    peek ptr =
        do format <- {# get GstIndexAssociation->format #} ptr
           value <- {# get GstIndexAssociation->value #} ptr
           return $ IndexAssociation (cToEnum format) (fromIntegral value)
    poke ptr (IndexAssociation format value) =
        do {# set GstIndexAssociation->format #} ptr $ cFromEnum format
           {# set GstIndexAssociation->value #} ptr $ fromIntegral value

{# enum GstAssocFlags as AssocFlags {underscoreToCase} with prefix = "GST" deriving (Eq, Bounded) #}
instance Flags AssocFlags

--------------------------------------------------------------------

-- | Use 'giveMiniObject' to pass an object to a function that takes
--   ownership of it.
giveMiniObject :: MiniObjectClass obj
               => obj
               -> (obj -> IO a)
               -> IO a
giveMiniObject obj action =
    do {# call gst_mini_object_ref #} (toMiniObject obj)
       action obj

--------------------------------------------------------------------

data MessageType = MessageEOS
                 | MessageError
                 | MessageWarning
                 | MessageInfo
                 | MessageTag
                 | MessageBuffering
                 | MessageStateChanged
                 | MessageStateDirty
                 | MessageStepDone
                 | MessageClockProvide
                 | MessageClockLost
                 | MessageNewClock
                 | MessageStructureChange
                 | MessageStreamStatus
                 | MessageApplication
                 | MessageElement
                 | MessageSegmentStart
                 | MessageSegmentDone
                 | MessageDuration
                 | MessageLatency
                 | MessageAsyncStart
                 | MessageAsyncDone
                   deriving (Eq, Bounded)
instance Enum MessageType where
    toEnum n | n == bit  0 = MessageEOS
             | n == bit  1 = MessageError
             | n == bit  2 = MessageWarning
             | n == bit  3 = MessageInfo
             | n == bit  4 = MessageTag
             | n == bit  5 = MessageBuffering
             | n == bit  6 = MessageStateChanged
             | n == bit  7 = MessageStateDirty
             | n == bit  8 = MessageStepDone
             | n == bit  9 = MessageClockProvide
             | n == bit 10 = MessageClockLost
             | n == bit 11 = MessageNewClock
             | n == bit 12 = MessageStructureChange
             | n == bit 13 = MessageStreamStatus
             | n == bit 14 = MessageApplication
             | n == bit 15 = MessageElement
             | n == bit 16 = MessageSegmentStart
             | n == bit 17 = MessageSegmentDone
             | n == bit 18 = MessageDuration
             | n == bit 19 = MessageLatency
             | n == bit 20 = MessageAsyncStart
             | n == bit 21 = MessageAsyncDone
    fromEnum MessageEOS             = bit  0
    fromEnum MessageError           = bit  1
    fromEnum MessageWarning         = bit  2
    fromEnum MessageInfo            = bit  3
    fromEnum MessageTag             = bit  4
    fromEnum MessageBuffering       = bit  5
    fromEnum MessageStateChanged    = bit  6
    fromEnum MessageStateDirty      = bit  7
    fromEnum MessageStepDone        = bit  8
    fromEnum MessageClockProvide    = bit  9
    fromEnum MessageClockLost       = bit 10
    fromEnum MessageNewClock        = bit 11
    fromEnum MessageStructureChange = bit 12
    fromEnum MessageStreamStatus    = bit 13
    fromEnum MessageApplication     = bit 14
    fromEnum MessageElement         = bit 15
    fromEnum MessageSegmentStart    = bit 16
    fromEnum MessageSegmentDone     = bit 17
    fromEnum MessageDuration        = bit 18
    fromEnum MessageLatency         = bit 19
    fromEnum MessageAsyncStart      = bit 20
    fromEnum MessageAsyncDone       = bit 21
instance Flags MessageType

--------------------------------------------------------------------

{#enum GstQueryType as QueryType {underscoreToCase} with prefix = "GST" deriving (Eq) #}

--------------------------------------------------------------------

{# pointer *GstIterator as PtrIterator foreign newtype #}

withPtrIterator :: PtrIterator
                -> (Ptr PtrIterator -> IO a)
                -> IO a
withPtrIterator (PtrIterator cPtrIterator) = withForeignPtr cPtrIterator

takePtrIterator, peekPtrIterator :: Ptr PtrIterator
                                -> IO  PtrIterator
takePtrIterator ptrIteratorPtr =
    liftM PtrIterator $ newForeignPtr ptrIteratorPtr ptrIteratorFinalizer
peekPtrIterator ptrIteratorPtr =
    liftM PtrIterator $ newForeignPtr_ ptrIteratorPtr

foreign import ccall unsafe "&gst_iterator_free"
    ptrIteratorFinalizer :: FunPtr (Ptr PtrIterator -> IO ())

{# enum GstIteratorItem   as IteratorItem   {underscoreToCase} with prefix = "GST" deriving (Eq) #}
{# enum GstIteratorResult as IteratorResult {underscoreToCase} with prefix = "GST" deriving (Eq) #}

mkIterator newPtrIterator cPtrIterator =
    do ptrIterator <- newPtrIterator cPtrIterator
       return $ Iterator ptrIterator

newtype Iteratable a => Iterator a = Iterator PtrIterator

withIterator :: Iterator a
             -> (Ptr PtrIterator -> IO a)
             -> IO a
withIterator (Iterator ptrIterator) = withPtrIterator ptrIterator

takeIterator, peekIterator :: Ptr PtrIterator
                          -> IO (Iterator a)
takeIterator  cPtrIterator = mkIterator takePtrIterator  cPtrIterator
peekIterator cPtrIterator = mkIterator peekPtrIterator cPtrIterator

class Iteratable a where
    peekIteratable :: Ptr ()
                   -> IO a
    withIteratable :: a
                   -> (Ptr a -> IO b)
                   -> IO b

type IteratorFilter itemT = itemT
                         -> IO Bool
type IteratorFoldFunction itemT accumT = itemT
                                      -> accumT
                                      -> IO (Bool, accumT)

--------------------------------------------------------------------

{# pointer *GstCaps as Caps foreign newtype #}
mkCaps :: ForeignPtr Caps -> Caps
mkCaps = Caps
unCaps :: Caps -> ForeignPtr Caps
unCaps (Caps caps) = caps

withCaps :: Caps -> (Ptr Caps -> IO a) -> IO a
withCaps = withForeignPtr . unCaps

takeCaps, peekCaps :: Ptr Caps
                  -> IO Caps
takeCaps capsPtr =
    liftM Caps $ newForeignPtr capsPtr capsFinalizer
peekCaps capsPtr =
    cCapsRef capsPtr >>= takeCaps
foreign import ccall unsafe "gst_caps_ref"
    cCapsRef :: Ptr Caps
             -> IO (Ptr Caps)

giveCaps :: Caps
         -> (Caps -> IO a)
         -> IO a
giveCaps caps action =
    do {# call caps_ref #} caps
       action caps

foreign import ccall unsafe "&gst_caps_unref"
    capsFinalizer :: FunPtr (Ptr Caps -> IO ())

data CapsFlags = CapsFlagsAny
                 deriving (Eq, Bounded)
instance Enum CapsFlags where
    toEnum n | n == shiftL 1 0 = CapsFlagsAny
    fromEnum CapsFlagsAny = shiftL 1 0
instance Flags CapsFlags

--------------------------------------------------------------------

{# pointer *GstStructure as Structure foreign newtype #}
mkStructure :: ForeignPtr Structure -> Structure
mkStructure = Structure
unStructure :: Structure -> ForeignPtr Structure
unStructure (Structure structure) = structure

withStructure :: Structure -> (Ptr Structure -> IO a) -> IO a
withStructure = withForeignPtr . unStructure

mkNewStructure :: (Ptr Structure -> IO (ForeignPtr Structure))
               -> Ptr Structure
               -> IO Structure
mkNewStructure mkFP structurePtr =
    do cStructureMakeImmutable structurePtr
       liftM Structure $ mkFP structurePtr
foreign import ccall unsafe "_hs_gst_structure_make_immutable"
    cStructureMakeImmutable :: Ptr Structure
                            -> IO ()

takeStructure, peekStructure :: Ptr Structure
                            -> IO Structure
takeStructure =
    mkNewStructure $ flip newForeignPtr structureFinalizer
peekStructure =
    mkNewStructure $ newForeignPtr_

giveStructure :: Structure
              -> (Structure -> IO a)
              -> IO a
giveStructure structure action =
    {# call structure_copy #} structure >>= peekStructure >>= action

foreign import ccall unsafe "&gst_structure_free"
    structureFinalizer :: FunPtr (Ptr Structure -> IO ())

type StructureForeachFunc =  Quark
                          -> GValue
                          -> IO Bool

newtype StructureM a = StructureM (StructureMRep a)
type StructureMRep a = (Structure -> IO a)

instance Monad StructureM where
    (StructureM aM) >>= fbM =
        StructureM $ \structure ->
            do a <- aM structure
               let StructureM bM = fbM a
               bM structure
    return a = StructureM $ const $ return a

--------------------------------------------------------------------

type TagList = Structure
mkTagList = mkStructure
unTagList = unStructure
withTagList = withStructure
takeTagList = takeStructure
peekTagList = takeStructure
giveTagList = giveStructure

type Tag = String

{# enum GstTagFlag as TagFlag {underscoreToCase} with prefix = "GST" deriving (Eq) #}
{# enum GstTagMergeMode as TagMergeMode {underscoreToCase} with prefix = "GST" deriving (Eq) #}

--------------------------------------------------------------------

data Segment = Segment { segmentRate        :: Double
                       , segmentAbsRate     :: Double
                       , segmentFormat      :: Format
                       , segmentFlags       :: [SeekFlags]
                       , segmentStart       :: Int64
                       , segmentStop        :: Int64
                       , segmentTime        :: Int64
                       , segmentAccum       :: Int64
                       , segmentLastStop    :: Int64
                       , segmentDuration    :: Int64 }
instance Storable Segment where
    sizeOf _ = fromIntegral cSegmentSizeof
    alignment _ = alignment (undefined :: CString)
    peek ptr =
        do rate        <- {# get GstSegment->rate #} ptr
           absRate     <- {# get GstSegment->abs_rate #} ptr
           format      <- {# get GstSegment->format #} ptr
           flags       <- {# get GstSegment->flags #} ptr
           start       <- {# get GstSegment->start #} ptr
           stop        <- {# get GstSegment->stop #} ptr
           time        <- {# get GstSegment->time #} ptr
           accum       <- {# get GstSegment->accum #} ptr
           lastStop    <- {# get GstSegment->last_stop #} ptr
           duration    <- {# get GstSegment->duration #} ptr
           return $ Segment (realToFrac rate)
                            (realToFrac absRate)
                            (cToEnum format)
                            (toFlags $ fromIntegral flags)
                            (fromIntegral start)
                            (fromIntegral stop)
                            (fromIntegral time)
                            (fromIntegral accum)
                            (fromIntegral lastStop)
                            (fromIntegral duration)
    poke ptr (Segment rate
                      absRate
                      format
                      flags
                      start
                      stop
                      time
                      accum
                      lastStop
                      duration) =
        do {# call segment_init #} (castPtr ptr)
                                   (cFromEnum format)
           {# set GstSegment->rate #} ptr $ realToFrac rate
           {# set GstSegment->abs_rate #} ptr $ realToFrac absRate
           {# set GstSegment->format #} ptr $ cFromEnum format
           {# set GstSegment->flags #} ptr $ fromIntegral $ fromFlags flags
           {# set GstSegment->start #} ptr $ fromIntegral start
           {# set GstSegment->stop #} ptr $ fromIntegral stop
           {# set GstSegment->time #} ptr $ fromIntegral time
           {# set GstSegment->accum #} ptr $ fromIntegral accum
           {# set GstSegment->last_stop #} ptr $ fromIntegral lastStop
           {# set GstSegment->duration #} ptr $ fromIntegral duration
foreign import ccall unsafe "_hs_gst_segment_sizeof"
    cSegmentSizeof :: {# type gsize #}

--------------------------------------------------------------------
