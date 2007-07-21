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
  
  FourCC,
  Fraction,
  
  Format(..),
  toFormat,
  fromFormat,
  FormatDefinition(..),
  
  ObjectFlags(..),
  withObject,
  newObject,
  newObject_,
  giveObject,
  
  PadFlags(..),
  PadDirection(..),
  toPadDirection,
  fromPadDirection,
  PadPresence(..),
  PadLinkReturn(..),
  FlowReturn(..),
  ActivateMode(..),
  withPad,
  newPad,
  newPad_,
  
  withGhostPad,
  newGhostPad,
  newGhostPad_,

  withPluginFeature,
  newPluginFeature,
  newPluginFeature_,

  withElementFactory,
  newElementFactory,
  newElementFactory_,
  
  withTypeFindFactory,
  newTypeFindFactory,
  newTypeFindFactory_,
  
  withIndexFactory,
  newIndexFactory,
  newIndexFactory_,
  
  ElementFlags(..),
  State(..),
  StateChange(..),
  StateChangeReturn(..),
  SeekFlags(..),
  SeekType(..),
  fromSeekType,
  toSeekType,
  withElement,
  newElement,
  newElement_,
  
  withBin,
  newBin,
  newBin_,
  
  withPipeline,
  newPipeline,
  newPipeline_,
  
  withPlugin,
  newPlugin,
  newPlugin_,
  
  PluginFilter,
  PluginFeatureFilter,
  withRegistry,
  newRegistry,
  newRegistry_,
  
  BusFunc,
  BusFlags(..),
  BusSyncReply(..),
  withBus,
  newBus,
  newBus_,
  
  ClockFlags(..),
  ClockTime,
  ClockTimeDiff,
  ClockReturn(..),
  toClockReturn,
  fromClockReturn,
  withClock,
  newClock,
  newClock_,
  ClockID(..),
  withClockID,
  newClockID,
  newClockID_,
  
  withSystemClock,
  newSystemClock,
  newSystemClock_,
  
  IndexFlags(..),
  withIndex,
  newIndex,
  newIndex_,
  IndexCertainty(..),
  toIndexCertainty,
  fromIndexCertainty,
  IndexEntry(..),
  newIndexEntry,
  newIndexEntry_,
  IndexEntryType(..),
  IndexLookupMethod(..),
  fromIndexLookupMethod,
  toIndexLookupMethod,
  IndexFilter,
  IndexAssociation(..),
  AssocFlags(..),
  
  StaticPadTemplate(..),
  withPadTemplate,
  newPadTemplate,
  newPadTemplate_,
  
  withTask,
  newTask,
  newTask_,
  
  withXML,
  newXML,
  newXML_,
  
  MiniObject(..),
  MiniObjectClass,
  mkMiniObject,
  unMiniObject,
  withMiniObject,
  toMiniObject,
  fromMiniObject,
  castToMiniObject,
  newMiniObject,
  newMiniObject_,
  giveMiniObject,
  
  Buffer(..),
  BufferClass,
  mkBuffer,
  unBuffer,
  withBuffer,
  toBuffer,
  fromBuffer,
  newBuffer,
  newBuffer_,
  castToBuffer,
  
  Event(..),
  EventClass,
  mkEvent,
  unEvent,
  withEvent,
  toEvent,
  fromEvent,
  newEvent,
  newEvent_,
  castToEvent,
  
  Message(..),
  MessageClass,
  MessageType(..),
  mkMessage,
  unMessage,
  withMessage,
  toMessage,
  fromMessage,
  newMessage,
  newMessage_,
  castToMessage,
  
  Query(..),
  QueryType(..),
  QueryClass,
  mkQuery,
  unQuery,
  withQuery,
  toQuery,
  fromQuery,
  newQuery,
  newQuery_,
  castToQuery,
  
  PtrIterator(..),
  Iterator(..),
  IteratorItem(..),
  IteratorResult(..),
  Iteratable(..),
  IteratorFilter,
  IteratorFoldFunction,
  withIterator,
  newIterator,
  newIterator_,
  -- giveIterator - not defined - iterators don't have a refcount and
  -- are not copyable
  
  Caps(..),
  withCaps,
  newCaps,
  newCaps_,
  giveCaps,
  
  Structure(..),
  StructureForeachFunc,
  withStructure,
  newStructure,
  newStructure_,
  giveStructure,
  StructureM(..),
  StructureMRep,
  
  TagList,
  withTagList,
  newTagList,
  newTagList_,
  giveTagList,
  Tag,
  TagFlag,
  toTagFlag,
  fromTagFlag,
  TagMergeMode,
  toTagMergeMode,
  fromTagMergeMode,
  
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
import System.Glib.UTFString
import GHC.Base            ( unsafeCoerce# )
{#import Media.Streaming.GStreamer.Core.Hierarchy#}

{# context lib = "gstreamer" prefix = "gst" #}

type FourCC = Word32
type Fraction = Ratio Int

{# enum GstParseError as ParseError {underscoreToCase} with prefix = "GST" deriving (Eq) #}

--------------------------------------------------------------------

{# enum GstFormat as Format {underscoreToCase} with prefix = "GST" deriving (Eq) #}
toFormat int = (toEnum $ fromIntegral int) :: Format
fromFormat (format :: Format) = fromIntegral $ fromEnum format

data FormatDefinition = FormatDefinition { formatValue       :: Format
                                         , formatNick        :: String
                                         , formatDescription :: String
                                         , formatQuark       :: Quark }
instance Storable FormatDefinition where
    sizeOf = undefined
    alignment = undefined
    peek ptr =
        do value       <- liftM toFormat $ {# get GstFormatDefinition->value #} ptr
           nick        <- {# get GstFormatDefinition->nick #} ptr >>= peekUTFString
           description <- {# get GstFormatDefinition->description #} ptr >>= peekUTFString
           quark       <- {# get GstFormatDefinition->quark #} ptr
           return $ FormatDefinition value nick description quark
    poke = undefined
instance Iteratable FormatDefinition where
    peekIteratable = peek . castPtr
    withIteratable = with

--------------------------------------------------------------------

withObject :: Object
           -> (Ptr Object -> IO a)
           -> IO a
withObject = withForeignPtr . unObject

newObject, newObject_ :: (ObjectClass obj, ObjectClass obj')
                      => (ForeignPtr obj' -> obj')
                      -> Ptr obj
                      -> IO obj'
-- | Use 'newObject_' when a function returns an object that is owned
--   elsewhere, or objects with floating references.
newObject_ cons cObject =
    liftM (cons . castForeignPtr) $ do
      cObjectTakeOwnership $ castPtr cObject
      newForeignPtr (castPtr cObject) objectFinalizer
foreign import ccall unsafe "&gst_object_unref"
  objectFinalizer :: FunPtr (Ptr () -> IO ())
foreign import ccall unsafe "_hs_gst_object_take_ownership"
  cObjectTakeOwnership :: Ptr ()
                       -> IO ()

-- | Use 'newObject' when a function returns an object that must be
--   unreffed when you're done with it; i.e., the object is owned by
--   the caller.
newObject cons cObject =
    liftM (cons . castForeignPtr) $
        newForeignPtr (castPtr cObject) objectFinalizer

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
objectFlagLast :: Int
objectFlagLast = shiftL 1 4
instance Flags ObjectFlags

--------------------------------------------------------------------

withPad :: Pad
        -> (Ptr Pad -> IO a)
        -> IO a
withPad = withForeignPtr . unPad

newPad, newPad_ :: Ptr Pad
                -> IO Pad
newPad  = newObject Pad
newPad_ = newObject_ Pad

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

{# enum GstPadDirection  as PadDirection  {underscoreToCase} with prefix = "GST" deriving (Eq, Show) #}
toPadDirection n = (toEnum $ fromIntegral n) :: PadDirection
fromPadDirection (n :: PadDirection) = fromIntegral $ fromEnum n

{# enum GstPadPresence   as PadPresence   {underscoreToCase} with prefix = "GST" deriving (Eq, Show) #}
{# enum GstPadLinkReturn as PadLinkReturn {underscoreToCase} with prefix = "GST" deriving (Eq, Show) #}
{# enum GstFlowReturn    as FlowReturn    {underscoreToCase} with prefix = "GST" deriving (Eq, Show) #}
{# enum GstActivateMode  as ActivateMode  {underscoreToCase} with prefix = "GST" deriving (Eq, Show) #}

instance Iteratable Pad where
    peekIteratable = newPad . castPtr
    withIteratable = withPad

--------------------------------------------------------------------

withGhostPad :: GhostPad
                  -> (Ptr GhostPad -> IO a)
                  -> IO a
withGhostPad = withForeignPtr . unGhostPad

newGhostPad, newGhostPad_ :: Ptr GhostPad
                          -> IO GhostPad
newGhostPad  = newObject  GhostPad
newGhostPad_ = newObject_ GhostPad

--------------------------------------------------------------------

withPluginFeature :: PluginFeature
                  -> (Ptr PluginFeature -> IO a)
                  -> IO a
withPluginFeature = withForeignPtr . unPluginFeature

newPluginFeature, newPluginFeature_ :: Ptr PluginFeature
                                    -> IO PluginFeature
newPluginFeature  = newObject PluginFeature
newPluginFeature_ = newObject_ PluginFeature

{# enum GstPluginError as PluginError {underscoreToCase} with prefix = "GST" deriving (Eq, Show) #}

--------------------------------------------------------------------

withElementFactory :: ElementFactory
                   -> (Ptr ElementFactory -> IO a)
                   -> IO a
withElementFactory = withForeignPtr . unElementFactory

newElementFactory, newElementFactory_ :: Ptr ElementFactory
                                      -> IO ElementFactory
newElementFactory  = newObject  ElementFactory
newElementFactory_ = newObject_ ElementFactory

--------------------------------------------------------------------

withTypeFindFactory :: TypeFindFactory
                   -> (Ptr TypeFindFactory -> IO a)
                   -> IO a
withTypeFindFactory = withForeignPtr . unTypeFindFactory

newTypeFindFactory, newTypeFindFactory_ :: Ptr TypeFindFactory
                                        -> IO TypeFindFactory
newTypeFindFactory  = newObject  TypeFindFactory
newTypeFindFactory_ = newObject_ TypeFindFactory

--------------------------------------------------------------------

withIndexFactory :: IndexFactory
                 -> (Ptr IndexFactory -> IO a)
                 -> IO a
withIndexFactory = withForeignPtr . unIndexFactory

newIndexFactory, newIndexFactory_ :: Ptr IndexFactory
                                  -> IO IndexFactory
newIndexFactory  = newObject  IndexFactory
newIndexFactory_ = newObject_ IndexFactory

--------------------------------------------------------------------

withElement :: Element
            -> (Ptr Element -> IO a)
            -> IO a
withElement (Element cElement) = withForeignPtr cElement
newElement, newElement_ :: Ptr Element
                        -> IO Element
newElement  = newObject  Element
newElement_ = newObject_ Element

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

{# enum GstSeekFlags as SeekFlags {underscoreToCase} with prefix = "GST" deriving (Eq, Bounded) #}
instance Flags SeekFlags
{# enum GstSeekType as SeekType {underscoreToCase} with prefix = "GST" deriving (Eq, Bounded) #}
toSeekType int = (toEnum $ fromIntegral int) :: SeekType
fromSeekType (seekType :: SeekType) = fromIntegral $ fromEnum seekType

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
    peekIteratable = newElement . castPtr
    withIteratable = withElement

--------------------------------------------------------------------

withImplementsInterface :: ImplementsInterface
                        -> (Ptr ImplementsInterface -> IO a)
                        -> IO a
withImplementsInterface = withForeignPtr . unImplementsInterface
newImplementsInterface, newImplementsInterface_ :: Ptr ImplementsInterface
                                                -> IO ImplementsInterface
newImplementsInterface  = newObject ImplementsInterface
newImplementsInterface_ = newObject_ ImplementsInterface

--------------------------------------------------------------------

withTagSetter :: TagSetter
              -> (Ptr TagSetter -> IO a)
              -> IO a
withTagSetter = withForeignPtr . unTagSetter
newTagSetter, newTagSetter_ :: Ptr TagSetter
                                                -> IO TagSetter
newTagSetter  = newObject TagSetter
newTagSetter_ = newObject_ TagSetter

--------------------------------------------------------------------

withBin :: Bin
        -> (Ptr Bin -> IO a)
        -> IO a
withBin = withForeignPtr . unBin
newBin, newBin_ :: Ptr Bin
                -> IO Bin
newBin  = newObject Bin
newBin_ = newObject_ Bin

--------------------------------------------------------------------

withPipeline :: Pipeline
             -> (Ptr Pipeline -> IO a)
             -> IO a
withPipeline = withForeignPtr . unPipeline
newPipeline, newPipeline_ :: Ptr Pipeline
                          -> IO Pipeline
newPipeline  = newObject  Pipeline
newPipeline_ = newObject_ Pipeline

--------------------------------------------------------------------

withPlugin :: Plugin
           -> (Ptr Plugin -> IO a)
           -> IO a
withPlugin = withForeignPtr . unPlugin
newPlugin, newPlugin_ :: Ptr Plugin
                      -> IO Plugin
newPlugin  = newObject  Plugin
newPlugin_ = newObject_ Plugin

--------------------------------------------------------------------

withRegistry :: Registry
             -> (Ptr Registry -> IO a)
             -> IO a
withRegistry = withForeignPtr . unRegistry
newRegistry, newRegistry_ :: Ptr Registry
                          -> IO Registry
newRegistry  = newObject  Registry
newRegistry_ = newObject_ Registry

type PluginFilter = Plugin -> IO Bool
type PluginFeatureFilter = PluginFeature -> IO Bool

--------------------------------------------------------------------

data BusFlags = BusFlushing
                deriving (Eq, Bounded)
instance Enum BusFlags where
    toEnum n | n == (shiftL objectFlagLast 0) = BusFlushing
    fromEnum BusFlushing  = shiftL objectFlagLast 0
instance Flags BusFlags

{# enum GstBusSyncReply as BusSyncReply {underscoreToCase} with prefix = "GST" deriving (Eq) #}

type BusFunc =  Bus
             -> Message
             -> IO Bool

withBus :: Bus
        -> (Ptr Bus -> IO a)
        -> IO a
withBus = withForeignPtr . unBus
newBus, newBus_ :: Ptr Bus
                -> IO Bus
newBus  = newObject  Bus
newBus_ = newObject_ Bus

--------------------------------------------------------------------

withClock :: Clock
          -> (Ptr Clock -> IO a)
          -> IO a
withClock = withForeignPtr . unClock

newClock, newClock_ :: Ptr Clock
                    -> IO Clock
newClock  = newObject Clock
newClock_ = newObject_ Clock

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

type ClockTime = {# type GstClockTime #}
type ClockTimeDiff = {# type GstClockTimeDiff #}

{# enum GstClockReturn as ClockReturn {underscoreToCase} with prefix = "GST" deriving (Eq) #}
toClockReturn int = (toEnum $ fromIntegral int) :: ClockReturn
fromClockReturn (clockReturn :: ClockReturn) = fromIntegral $ fromEnum clockReturn

{# pointer *GstClockID as ClockID foreign newtype #}
withClockID :: ClockID
            -> (Ptr ClockID -> IO a)
            -> IO a
withClockID (ClockID clockID) = withForeignPtr clockID
newClockID, newClockID_ :: Ptr ClockID
                        -> IO ClockID
newClockID clockIDPtr =
    liftM ClockID $ newForeignPtr clockIDPtr clockIDFinalizer
newClockID_ clockIDPtr =
    do {# call clock_id_ref #} $ castPtr clockIDPtr
       newClockID clockIDPtr

foreign import ccall unsafe "&gst_clock_id_unref"
    clockIDFinalizer :: FunPtr (Ptr ClockID -> IO ())

--------------------------------------------------------------------

withSystemClock :: SystemClock
                -> (Ptr SystemClock -> IO a)
                -> IO a
withSystemClock = withForeignPtr . unSystemClock
newSystemClock, newSystemClock_ :: Ptr SystemClock
                                -> IO SystemClock
newSystemClock  = newObject  SystemClock
newSystemClock_ = newObject_ SystemClock

--------------------------------------------------------------------

withIndex :: Index
          -> (Ptr Index -> IO a)
          -> IO a
withIndex = withForeignPtr . unIndex

newIndex, newIndex_ :: Ptr Index
                    -> IO Index
newIndex  = newObject Index
newIndex_ = newObject_ Index

data IndexFlags = IndexWritable
                | IndexReadable
                    deriving (Bounded)
instance Enum IndexFlags where
    toEnum n | n == (shiftL objectFlagLast 0) = IndexWritable
             | n == (shiftL objectFlagLast 1) = IndexReadable
    fromEnum IndexWritable = (shiftL objectFlagLast 0)
    fromEnum IndexReadable = (shiftL objectFlagLast 1)
instance Flags IndexFlags

{# enum GstIndexCertainty as IndexCertainty {underscoreToCase} with prefix = "GST" deriving (Eq) #}
toIndexCertainty int = (toEnum $ fromIntegral int) :: IndexCertainty
fromIndexCertainty (certainty :: IndexCertainty) = fromIntegral $ fromEnum certainty

{# enum GstIndexEntryType as IndexEntryType {underscoreToCase} with prefix = "GST" deriving (Eq) #}
{# enum GstIndexLookupMethod as IndexLookupMethod {underscoreToCase} with prefix = "GST" deriving (Eq) #}
toIndexLookupMethod int = (toEnum $ fromIntegral int) :: IndexLookupMethod
fromIndexLookupMethod (format :: IndexLookupMethod) = fromIntegral $ fromEnum format

{# pointer *GstIndexEntry as IndexEntry foreign newtype #}

newIndexEntry :: Ptr IndexEntry
              -> IO IndexEntry
newIndexEntry ptr =
    liftM IndexEntry $ newForeignPtr ptr indexEntryFinalizer
foreign import ccall unsafe "&gst_index_entry_free"
    indexEntryFinalizer :: FunPtr (Ptr IndexEntry -> IO ())
newIndexEntry_ :: Ptr IndexEntry
               -> IO IndexEntry
newIndexEntry_ ptr =
    (liftM IndexEntry $ newForeignPtr_ ptr) >>=
        {# call index_entry_copy #} >>=
            newIndexEntry

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
           return $ IndexAssociation (toFormat format) (fromIntegral value)
    poke ptr (IndexAssociation format value) =
        do {# set GstIndexAssociation->format #} ptr $ fromFormat format
           {# set GstIndexAssociation->value #} ptr $ fromIntegral value

{# enum GstAssocFlags as AssocFlags {underscoreToCase} with prefix = "GST" deriving (Eq, Bounded) #}
instance Flags AssocFlags

--------------------------------------------------------------------

{# pointer *GstStaticPadTemplate as StaticPadTemplate newtype #}

withPadTemplate :: PadTemplate
                -> (Ptr PadTemplate -> IO a)
                -> IO a
withPadTemplate = withForeignPtr . unPadTemplate

newPadTemplate, newPadTemplate_ :: Ptr PadTemplate
                                -> IO PadTemplate
newPadTemplate  = newObject PadTemplate
newPadTemplate_ = newObject_ PadTemplate

--------------------------------------------------------------------

withTask :: Task
         -> (Ptr Task -> IO a)
         -> IO a
withTask = withForeignPtr . unTask
newTask, newTask_ :: Ptr Task
                  -> IO Task
newTask  = newObject  Task
newTask_ = newObject_ Task

--------------------------------------------------------------------

withXML :: XML
        -> (Ptr XML -> IO a)
        -> IO a
withXML = withForeignPtr . unXML
newXML, newXML_ :: Ptr XML
                -> IO XML
newXML  = newObject  XML
newXML_ = newObject_ XML

--------------------------------------------------------------------

withChildProxy :: ChildProxy
               -> (Ptr ChildProxy -> IO a)
               -> IO a
withChildProxy = withForeignPtr . unChildProxy
newChildProxy, newChildProxy_ :: Ptr ChildProxy
                -> IO ChildProxy
newChildProxy  = newObject  ChildProxy
newChildProxy_ = newObject_ ChildProxy

--------------------------------------------------------------------

withURIHandler :: URIHandler
               -> (Ptr URIHandler -> IO a)
               -> IO a
withURIHandler = withForeignPtr . unURIHandler

--------------------------------------------------------------------

{# pointer *GstMiniObject as MiniObject foreign newtype #}

mkMiniObject = MiniObject
unMiniObject (MiniObject o) = o

withMiniObject :: MiniObject
               -> (Ptr MiniObject -> IO a)
               -> IO a
withMiniObject = withForeignPtr . unMiniObject

class MiniObjectClass o

toMiniObject :: MiniObjectClass o => o -> MiniObject
toMiniObject = unsafeCoerce#
fromMiniObject :: MiniObjectClass o => MiniObject -> o
fromMiniObject = unsafeCoerce#

instance MiniObjectClass MiniObject

castToMini :: (MiniObjectClass obj, MiniObjectClass obj')
           => GType
           -> String
           -> obj
           -> obj'
castToMini gtype objTypeName obj =
    case toMiniObject obj of
      mobj@(MiniObject objFPtr)
          | typeInstanceIsA ((unsafeForeignPtrToPtr . castForeignPtr) objFPtr) gtype ->
              fromMiniObject mobj
          | otherwise ->
              error $ "Cannot cast object to " ++ objTypeName

castToMiniObject :: MiniObjectClass obj
                 => obj
                 -> MiniObject
castToMiniObject =
    castToMini {# call fun unsafe gst_mini_object_get_type #} "MiniObject"

newMiniObject, newMiniObject_ :: (MiniObjectClass obj, MiniObjectClass obj')
                              => (ForeignPtr obj' -> obj')
                              -> Ptr obj
                              -> IO obj'
-- | Use 'newMiniObject_' when a function returns an object that is owned
--   elsewhere.
newMiniObject_ cons cObject =
    liftM (cons . castForeignPtr) $ do
      cMiniObjectRef $ castPtr cObject
      newForeignPtr (castPtr cObject) miniObjectFinalizer
foreign import ccall unsafe "&gst_object_unref"
  miniObjectFinalizer :: FunPtr (Ptr () -> IO ())
foreign import ccall unsafe "gst_mini_object_ref"
  cMiniObjectRef :: Ptr ()
                 -> IO (Ptr ())

-- | Use 'newMiniObject' when a function returns an object that must be
--   unreffed when you're done with it; i.e., the object is owned by
--   the caller.
newMiniObject cons cObject =
    liftM (cons . castForeignPtr) $
        newForeignPtr (castPtr cObject) miniObjectFinalizer

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

{#pointer *GstBuffer as Buffer foreign newtype #}

mkBuffer = Buffer
unBuffer (Buffer o) = o

withBuffer :: Buffer
           -> (Ptr Buffer -> IO a)
           -> IO a
withBuffer = withForeignPtr . unBuffer

class MiniObjectClass o => BufferClass o
toBuffer   :: BufferClass o => o -> Buffer
toBuffer   = unsafeCoerce#
fromBuffer :: BufferClass o => Buffer -> o
fromBuffer = unsafeCoerce#

instance BufferClass Buffer
instance MiniObjectClass Buffer

newBuffer, newBuffer_ :: Ptr Buffer
                      -> IO Buffer
newBuffer  = newMiniObject  Buffer
newBuffer_ = newMiniObject_ Buffer

castToBuffer :: MiniObjectClass obj
             => obj
             -> Buffer
castToBuffer =
    castToMini {# call fun unsafe gst_buffer_get_type #} "Buffer"

--------------------------------------------------------------------

{#pointer *GstEvent as Event foreign newtype #}

mkEvent = Event
unEvent (Event o) = o

withEvent :: Event
          -> (Ptr Event -> IO a)
          -> IO a
withEvent = withForeignPtr . unEvent

class MiniObjectClass o => EventClass o
toEvent   :: EventClass o => o -> Event
toEvent   = unsafeCoerce#
fromEvent :: EventClass o => Event -> o
fromEvent = unsafeCoerce#

instance EventClass Event
instance MiniObjectClass Event

newEvent, newEvent_ :: Ptr Event
                    -> IO Event
newEvent  = newMiniObject  Event
newEvent_ = newMiniObject_ Event

castToEvent :: MiniObjectClass obj
            => obj
            -> Event
castToEvent =
    castToMini {# call fun unsafe gst_event_get_type #} "Event"

--------------------------------------------------------------------

{# pointer *GstMessage as Message foreign newtype #}
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

mkMessage = Message
unMessage (Message o) = o

withMessage :: Message
            -> (Ptr Message -> IO a)
            -> IO a
withMessage = withForeignPtr . unMessage

class MiniObjectClass o => MessageClass o
toMessage   :: MessageClass o => o -> Message
toMessage   = unsafeCoerce#
fromMessage :: MessageClass o => Message -> o
fromMessage = unsafeCoerce#

instance MessageClass Message
instance MiniObjectClass Message

newMessage, newMessage_ :: Ptr Message
                    -> IO Message
newMessage  = newMiniObject  Message
newMessage_ = newMiniObject_ Message

castToMessage :: MiniObjectClass obj
              => obj
              -> Message
castToMessage =
    castToMini {# call fun unsafe gst_message_get_type #} "Message"

--------------------------------------------------------------------

{#pointer *GstQuery as Query foreign newtype #}
{#enum GstQueryType as QueryType {underscoreToCase} with prefix = "GST" deriving (Eq) #}

mkQuery = Query
unQuery (Query o) = o

withQuery :: Query
          -> (Ptr Query -> IO a)
          -> IO a
withQuery = withForeignPtr . unQuery

class MiniObjectClass o => QueryClass o
toQuery   :: QueryClass o => o -> Query
toQuery   = unsafeCoerce#
fromQuery :: QueryClass o => Query -> o
fromQuery = unsafeCoerce#

instance QueryClass Query
instance MiniObjectClass Query

newQuery, newQuery_ :: Ptr Query
                    -> IO Query
newQuery  = newMiniObject  Query
newQuery_ = newMiniObject_ Query

castToQuery :: MiniObjectClass obj
            => obj
            -> Query
castToQuery =
    castToMini {# call fun unsafe gst_query_get_type #} "Query"

--------------------------------------------------------------------

{# pointer *GstIterator as PtrIterator foreign newtype #}

withPtrIterator :: PtrIterator
                -> (Ptr PtrIterator -> IO a)
                -> IO a
withPtrIterator (PtrIterator cPtrIterator) = withForeignPtr cPtrIterator

newPtrIterator, newPtrIterator_ :: Ptr PtrIterator
                                -> IO  PtrIterator
newPtrIterator ptrIteratorPtr =
    liftM PtrIterator $ newForeignPtr ptrIteratorPtr ptrIteratorFinalizer
newPtrIterator_ ptrIteratorPtr =
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

newIterator, newIterator_ :: Ptr PtrIterator
                          -> IO (Iterator a)
newIterator  cPtrIterator = mkIterator newPtrIterator  cPtrIterator
newIterator_ cPtrIterator = mkIterator newPtrIterator_ cPtrIterator

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

newCaps, newCaps_ :: Ptr Caps
                  -> IO Caps
newCaps capsPtr =
    liftM Caps $ newForeignPtr capsPtr capsFinalizer
newCaps_ capsPtr =
    cCapsRef capsPtr >>= newCaps
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

newStructure, newStructure_ :: Ptr Structure
                            -> IO Structure
newStructure =
    mkNewStructure $ flip newForeignPtr structureFinalizer
newStructure_ =
    mkNewStructure $ newForeignPtr_

giveStructure :: Structure
              -> (Structure -> IO a)
              -> IO a
giveStructure structure action =
    {# call structure_copy #} structure >>= newStructure_ >>= action

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
newTagList = newStructure
newTagList_ = newStructure
giveTagList = giveStructure

type Tag = String

{# enum GstTagFlag as TagFlag {underscoreToCase} with prefix = "GST" deriving (Eq) #}
toTagFlag int = (toEnum $ fromIntegral int) :: TagFlag
fromTagFlag (tagFlag :: TagFlag) = fromIntegral $ fromEnum tagFlag

{# enum GstTagMergeMode as TagMergeMode {underscoreToCase} with prefix = "GST" deriving (Eq) #}
toTagMergeMode int = (toEnum $ fromIntegral int) :: TagMergeMode
fromTagMergeMode (tagMergeMode :: TagMergeMode) = fromIntegral $ fromEnum tagMergeMode

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
           appliedRate <- {# get GstSegment->applied_rate #} ptr
           return $ Segment (realToFrac rate)
                            (realToFrac absRate)
                            (toFormat format)
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
                                   (fromFormat format)
           {# set GstSegment->rate #} ptr $ realToFrac rate
           {# set GstSegment->abs_rate #} ptr $ realToFrac absRate
           {# set GstSegment->format #} ptr $ fromFormat format
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

