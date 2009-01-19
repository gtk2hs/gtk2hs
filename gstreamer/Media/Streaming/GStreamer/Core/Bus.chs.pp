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
--  An asynchronous message bus subsystem.
module Media.Streaming.GStreamer.Core.Bus (
-- * Detail
  -- | The 'Bus' is resposible for delivering 'Message's in a
  --   first-in, first-out order, from the streaming threads to the
  --   application.
  --   
  --   Since the application typically only wants to deal with
  --   delivery of these messages from one thread, the 'Bus' will
  --   marshal the messages between different threads. This is
  --   important since the actual streaming of media is done in
  --   a thread separate from the application.
  --
  --   The 'Bus' provides support for 'System.Glib.MainLoop.Source'
  --   based notifications. This makes it possible to handle the
  --   delivery in the GLib 'System.Glib.MainLoop.Source'.
  --
  --   A message is posted on the bus with the 'busPost' method. With
  --   the 'busPeek' and 'busPop' methods one can look at or retrieve
  --   a previously posted message.
  --
  --   The bus can be polled with the 'busPoll' method. This methods
  --   blocks up to the specified timeout value until one of the
  --   specified messages types is posted on the bus. The application
  --   can then pop the messages from the bus to handle
  --   them. Alternatively the application can register an
  --   asynchronous bus function using 'busAddWatch'. This function
  --   will install a 'System.Glib.MainLoop.Source' in the default
  --   GLib main loop and will deliver messages a short while after
  --   they have been posted. Note that the main loop should be
  --   running for the asynchronous callbacks.
  --
  --   It is also possible to get messages from the bus without any
  --   thread marshalling with the 'busSetSyncHandler' method. This
  --   makes it possible to react to a message in the same thread that
  --   posted the message on the bus. This should only be used if the
  --   application is able to deal with messages from different
  --   threads.
  --   
  --   Every 'Pipeline' has one bus.
  --
  --   Note that a 'Pipeline' will set its bus into flushing state
  --   when changing from 'StateReady' to 'StateNull'.

-- * Types
  Bus,
  BusClass,
  -- | The result of a 'BusSyncHandler'.
  BusSyncReply,
  -- | A handler that will be invoked synchronously when a new message
  --   is injected into the bus. This function is mostly used internally.
  --   Only one sync handler may be attached to a given bus.
  BusSyncHandler,
  castToBus,
  toBus,
-- * Bus Operations
  busGetFlags,
  busSetFlags,
  busUnsetFlags,
  busNew,
  busPost,
  busHavePending,
  busPeek,
  busPop,
#if GST_CHECK_VERSION(0,10,12)
  busTimedPop,
#endif
  busSetFlushing,
  busSetSyncHandler,
  busUseSyncSignalHandler,
  busCreateWatch,
  busAddWatch,
  busDisableSyncMessageEmission,
  busEnableSyncMessageEmission,
  busAddSignalWatch,
  busRemoveSignalWatch,
  busPoll,
  
-- * Bus Signals
  busMessage,
  busSyncMessage,
  
  ) where

import Control.Monad ( liftM
                     , when )
{#import Media.Streaming.GStreamer.Core.Object#}
{#import Media.Streaming.GStreamer.Core.Types#}
{#import Media.Streaming.GStreamer.Core.Signals#}
{#import System.Glib.MainLoop#}
import System.Glib.Flags
import System.Glib.FFI
{#import System.Glib.GObject#}
{#import System.Glib.MainLoop#}

{# context lib = "gstreamer" prefix = "gst" #}

-- | Get the flags set on this bus.
busGetFlags :: BusClass busT
            => busT          -- ^ @bus@ - a 'Bus'
            -> IO [BusFlags] -- ^ the flags set on @bus@
busGetFlags = mkObjectGetFlags

-- | Set flags on this bus.
busSetFlags :: BusClass busT
            => busT       -- ^ @bus@ - a 'Bus'
            -> [BusFlags] -- ^ the flags to set on @bus@
            -> IO ()
busSetFlags = mkObjectSetFlags

-- | Unset flags on this bus.
busUnsetFlags :: BusClass busT
              => busT       -- ^ @bus@ - a 'Bus'
              -> [BusFlags] -- ^ the flags to unset on @bus@
              -> IO ()
busUnsetFlags = mkObjectUnsetFlags

-- | Create a new bus.
busNew :: IO Bus -- ^ the newly created 'Bus' object
busNew =
    {# call bus_new #} >>= takeObject

-- | Post a message to the bus.
busPost :: BusClass busT
        => busT    -- ^ @bus@ - a 'Bus'
        -> Message -- ^ @message@ - the message to post
        -> IO Bool -- ^ 'True' if the message was posted, or
                   --   'False' if the bus is flushing
busPost bus message =
    do {# call gst_mini_object_ref #} (toMiniObject message)
       liftM toBool $ {# call bus_post #} (toBus bus) message

-- | Check if there are pending messages on the bus.
busHavePending :: BusClass busT
               => busT    -- ^ @bus@ - a 'Bus'
               -> IO Bool -- ^ 'True' if there are messages
                          --   on the bus to be handled, otherwise 'False'
busHavePending bus =
    liftM toBool $ {# call bus_have_pending #} $ toBus bus

-- | Get the message at the front of the queue.  Any message returned
--   will remain on the queue.
busPeek :: BusClass busT
        => busT               -- ^ @bus@ - a 'Bus'
        -> IO (Maybe Message) -- ^ the first 'Message' on the bus, or
                              --   'Nothing' if the bus is empty
busPeek bus =
    {# call bus_peek #} (toBus bus) >>= maybePeek takeMiniObject

-- | Get the message at the front of the queue.  It will be removed
--   from the queue.
busPop :: BusClass busT
       => busT               -- ^ @bus@ - a 'Bus'
       -> IO (Maybe Message) -- ^ the first 'Message' on the bus, or
                             --   'Nothing' if the bus is empty
busPop bus =
    {# call bus_pop #} (toBus bus) >>= maybePeek takeMiniObject

#if GST_CHECK_VERSION(0,10,12)
-- | Get a message from the bus, waiting up to the specified timeout.
--   If the time given is 'Nothing', the function will wait forever.
--   If the time given is @0@, the function will behave like 'busPop'.
--   
--   Since 0.10.12.
busTimedPop :: BusClass busT
            => busT               -- ^ @bus@ - a 'Bus'
            -> Maybe ClockTime    -- ^ @timeoutM@ - the time to wait for,
                                  --   or 'Nothing' to wait forever
            -> IO (Maybe Message) -- ^ the first message recieved, or
                                  --   'Nothing' if the timeout has expired
busTimedPop bus timeoutM =
    let timeout = case timeoutM of
                    Just timeout' -> timeout'
                    Nothing       -> clockTimeNone
    in {# call bus_timed_pop #} (toBus bus) (fromIntegral timeout) >>=
           maybePeek takeMiniObject
#endif

-- | If @flushing@ is 'True', the bus will flush out any queued
--   messages, as well as any future messages, until the function is
--   called with @flushing@ set to 'False'.
busSetFlushing :: BusClass busT
               => busT  -- ^ @bus@ - a 'Bus'
               -> Bool  -- ^ @flushing@ - the new flushing state
               -> IO ()
busSetFlushing bus flushing =
    {# call bus_set_flushing #} (toBus bus) $ fromBool flushing

type CBusSyncHandler =  Ptr Bus
                     -> Ptr Message
                     -> {# type gpointer #}
                     -> IO {# type GstBusSyncReply #}
marshalBusSyncHandler :: BusSyncHandler
                      -> IO {# type GstBusSyncHandler #}
marshalBusSyncHandler busSyncHandler =
    makeBusSyncHandler cBusSyncHandler
    where cBusSyncHandler :: CBusSyncHandler
          cBusSyncHandler busPtr messagePtr _ =
              do bus <- peekObject busPtr
                 message <- peekMiniObject messagePtr
                 reply <- busSyncHandler bus message
                 when (reply == BusDrop) $
                     {# call gst_mini_object_unref #} (toMiniObject message)
                 return $ fromIntegral $ fromEnum reply
foreign import ccall "wrapper"
    makeBusSyncHandler :: CBusSyncHandler
                       -> IO {# type GstBusSyncHandler #}

-- the following mess is necessary to clean up the FunPtr after
-- busSetSyncHandler.  gstreamer doesn't give us a nice way to do this
-- (such as a DestroyNotify pointer in the argument list)
weakNotifyQuark, funPtrQuark :: Quark
weakNotifyQuark = unsafePerformIO $ quarkFromString "Gtk2HS::SyncHandlerWeakNotify"
funPtrQuark = unsafePerformIO $ quarkFromString "Gtk2HS::SyncHandlerFunPtr"

getWeakNotify :: BusClass busT
              => busT
              -> IO (Maybe GWeakNotify)
getWeakNotify = objectGetAttributeUnsafe weakNotifyQuark

setWeakNotify :: BusClass busT
              => busT
              -> Maybe GWeakNotify
              -> IO ()
setWeakNotify = objectSetAttribute weakNotifyQuark

getFunPtr :: BusClass busT
          => busT
          -> IO (Maybe {# type GstBusSyncHandler #})
getFunPtr = objectGetAttributeUnsafe funPtrQuark

setFunPtr :: BusClass busT
          => busT
          -> Maybe {# type GstBusSyncHandler #}
          -> IO ()
setFunPtr = objectSetAttribute funPtrQuark

unsetSyncHandler :: BusClass busT
                 => busT
                 -> IO ()
unsetSyncHandler bus = do
  {# call bus_set_sync_handler #} (toBus bus) nullFunPtr nullPtr
  oldWeakNotifyM <- getWeakNotify bus
  case oldWeakNotifyM of
    Just oldWeakNotify -> objectWeakunref bus oldWeakNotify
    Nothing            -> return ()
  setWeakNotify bus Nothing
  oldFunPtrM <- getFunPtr bus
  case oldFunPtrM of
    Just oldFunPtr -> freeHaskellFunPtr oldFunPtr
    Nothing        -> return ()
  setFunPtr bus Nothing

-- | Set the synchronous message handler on the bus. The function will
--   be called every time a new message is posted to the bus. Note
--   that the function will be called from the thread context of the
--   poster.
--   
--   Calling this function will replace any previously set sync
--   handler. If 'Nothing' is passed to this function, it will unset
--   the handler.
busSetSyncHandler :: BusClass busT
                  => busT                 -- ^ @bus@ - a 'Bus'
                  -> Maybe BusSyncHandler -- ^ @busSyncHandlerM@ - the new 'BusSyncHandler'
                  -> IO ()
busSetSyncHandler bus busSyncHandlerM = do
  objectWithLock bus $ do
    unsetSyncHandler bus
    case busSyncHandlerM of
      Just busSyncHandler ->
          do funPtr <- marshalBusSyncHandler busSyncHandler
             setFunPtr bus $ Just funPtr
             weakNotify <- objectWeakref bus $ freeHaskellFunPtr funPtr
             setWeakNotify bus $ Just weakNotify
             {# call bus_set_sync_handler #} (toBus bus) funPtr nullPtr
      Nothing ->
          return ()

-- | Use a synchronous message handler that converts all messages to signals.
busUseSyncSignalHandler :: BusClass busT
                        => busT  -- ^ @bus@ - a 'Bus'
                        -> IO ()
busUseSyncSignalHandler bus = do
  objectWithLock bus $ do
    unsetSyncHandler bus
    {# call bus_set_sync_handler #} (toBus bus) cBusSyncSignalHandlerPtr nullPtr
foreign import ccall unsafe "&gst_bus_sync_signal_handler"
    cBusSyncSignalHandlerPtr :: {# type GstBusSyncHandler #}

-- | Create a watch for the bus. The 'Source' will dispatch a signal
--   whenever a message is on the bus. After the signal is dispatched,
--   the message is popped off the bus.
busCreateWatch :: BusClass busT
               => busT      -- ^ @bus@ - a 'Bus'
               -> IO Source -- ^ the new event 'Source'
busCreateWatch bus =
    liftM Source $ {# call bus_create_watch #} (toBus bus) >>=
        flip newForeignPtr sourceFinalizer
foreign import ccall unsafe "&g_source_unref"
    sourceFinalizer :: FunPtr (Ptr Source -> IO ())

type CBusFunc =  Ptr Bus
              -> Ptr Message
              -> {# type gpointer #}
              -> IO {# type gboolean #}
marshalBusFunc :: BusFunc
               -> IO {# type GstBusFunc #}
marshalBusFunc busFunc =
    makeBusFunc cBusFunc
    where cBusFunc :: CBusFunc
          cBusFunc busPtr messagePtr userData =
              do bus <- peekObject busPtr
                 message <- peekMiniObject messagePtr
                 liftM fromBool $ busFunc bus message
foreign import ccall "wrapper"
    makeBusFunc :: CBusFunc
                -> IO {# type GstBusFunc #}

-- | Adds a bus watch to the default main context with the given
--   priority. This function is used to receive asynchronous messages
--   in the main loop.
--   
--   The watch can be removed by calling 'System.Glib.MainLoop.sourceRemove'.
busAddWatch :: BusClass busT
            => busT         -- ^ @bus@ - a 'Bus'
            -> Priority     -- ^ @priority@ - the priority of the watch
            -> BusFunc      -- ^ @func@ - the action to perform when a message is recieved
            -> IO HandlerId -- ^ the event source ID
busAddWatch bus priority func =
    do busFuncPtr <- marshalBusFunc func
       destroyNotify <- mkFunPtrDestroyNotify busFuncPtr
       liftM fromIntegral $
           {# call bus_add_watch_full #}
               (toBus bus)
               (fromIntegral priority)
               busFuncPtr
               nullPtr
               destroyNotify

-- | Instructs GStreamer to stop emitting the 'busSyncMessage' signal
--   for this bus. See 'busEnableSyncMessageEmission' for more
--   information.
--   
--   In the event that multiple pieces of code have called
--   'busEnableSyncMessageEmission', the sync-message
--   emissions will only be stopped after all calls to
--   'busEnableSyncMessageEmission' were "cancelled" by
--   calling this function.
busDisableSyncMessageEmission :: BusClass busT
                              => busT  -- ^ @bus@ - a 'Bus'
                              -> IO ()
busDisableSyncMessageEmission =
    {# call bus_disable_sync_message_emission #} . toBus

-- | Instructs GStreamer to emit the 'busSyncMessage' signal after
--   running the bus's sync handler. This function is here so that
--   programmers can ensure that they can synchronously receive
--   messages without having to affect what the bin's sync handler is.
--   
--   This function may be called multiple times. To clean up, the
--   caller is responsible for calling 'busDisableSyncMessageEmission'
--   as many times as this function is called.
--   
--   While this function looks similar to 'busAddSignalWatch', it is
--   not exactly the same -- this function enables synchronous
--   emission of signals when messages arrive; 'busAddSignalWatch'
--   adds an idle callback to pop messages off the bus
--   asynchronously. The 'busSyncMessage' signal comes from the thread
--   of whatever object posted the message; the 'busMessage' signal is
--   marshalled to the main thread via the main loop.
busEnableSyncMessageEmission :: BusClass busT
                             => busT  -- ^ @bus@ - a 'Bus'
                             -> IO ()
busEnableSyncMessageEmission =
    {# call bus_enable_sync_message_emission #} . toBus

-- | Adds a bus signal watch to the default main context with the
--   given priority. After calling this method, the bus will emit the
--   'busMessage' signal for each message posted on the bus.
--   
--   This function may be called multiple times. To clean up, the
--   caller is responsible for calling 'busRemoveSignalWatch' as many
--   times.
busAddSignalWatch :: BusClass busT
                  => busT     -- ^ @bus@ - a 'Bus'
                  -> Priority -- ^ @priority@ - the priority of the watch
                  -> IO ()
busAddSignalWatch bus priority =
    {# call bus_add_signal_watch_full #} (toBus bus) $ fromIntegral priority

-- | Remove the signal watch that was added with 'busAddSignalWatch'.
busRemoveSignalWatch :: BusClass busT
                     => busT  -- ^ @bus@ - a 'Bus'
                     -> IO ()
busRemoveSignalWatch =
    {# call bus_remove_signal_watch #} . toBus

-- | Poll the bus for a message. Will block while waiting for messages
--   to come. You can specify the maximum amount of time to wait with
--   the @timeout@ parameter. If @timeout@ is negative, the function
--   will wait indefinitely.
--   
--   Messages not in @events@ will be popped off the bus and ignored.
--   
--   Because 'busPoll' is implemented using the 'busMessage' signal
--   enabled by 'busAddSignalWatch', calling 'busPoll' will cause the
--   'busMessage' signal to be emitted for every message that the
--   function sees. Thus, a 'busMessage' signal handler will see every
--   message that 'busPoll' sees -- neither will steal messages from
--   the other.
--   
--   This function will run a main loop in the default main context
--   while polling.
busPoll :: BusClass busT
        => busT          -- ^ @bus@ - a 'Bus'
        -> [MessageType] -- ^ @events@ - the set of messages to poll for
        -> ClockTimeDiff -- ^ @timeout@ - the time to wait, or -1 to wait indefinitely
        -> IO Message
busPoll bus events timeout =
    {# call bus_poll #} (toBus bus)
                        (fromIntegral $ fromFlags events)
                        (fromIntegral timeout) >>=
        takeMiniObject

-- | A message has been posted on the bus. This signal is emitted from
--   a 'Source' added to the 'MainLoop', and only when it is running.
busMessage :: BusClass busT
           => Signal busT (Message -> IO ())
busMessage =
    Signal $ connect_BOXED__NONE "message" peekMiniObject

-- | A message has been posted on the bus. This signal is emitted from
--   the thread that posted the message so one has to be careful with
--   locking.
--   
--   This signal will not be emitted by default, you must first call
--   'busUseSyncSignalHandler' if you want this signal to be emitted
--   when a message is posted on the bus.
busSyncMessage :: BusClass busT
               => Signal busT (Message -> IO ())
busSyncMessage =
    Signal $ connect_BOXED__NONE "sync-message" peekMiniObject
