{-# LANGUAGE CPP #-}
{-# CFILES hsgclosure.c #-}
-- -*-haskell-*-
--  Callback installers for the GIMP Toolkit (GTK) Binding for Haskell
--
--  Author : Axel Simon
--
--  Created: 1 July 2000
--
--  Copyright (C) 2000-2005 Axel Simon, Duncan Coutts
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- #prune

--    The object system in the second version of GTK is based on GObject from
--    GLIB. This base class is rather primitive in that it only implements
--    ref and unref methods (and others that are not interesting to us). If
--    the marshall list mentions OBJECT it refers to an instance of this
--    GObject which is automatically wrapped with a ref and unref call.
--    Structures which are not derived from GObject have to be passed as
--    BOXED which gives the signal connect function a possiblity to do the
--    conversion into a proper ForeignPtr type. In special cases the signal
--    connect function use a PTR type which will then be mangled in the
--    user function directly. The latter is needed if a signal delivers a
--    pointer to a string and its length in a separate integer.
--
module System.Glib.Signals (
  Signal(Signal),
  on, after,
  SignalName,
  GSignalMatchType(..),
  ConnectAfter,
  ConnectId(ConnectId),
  signalDisconnect,
  signalBlock,
  signalBlockMatched,
  signalUnblock,
  signalStopEmission,
  disconnect,
  GClosure,
#ifdef USE_GCLOSURE_SIGNALS_IMPL
  connectGeneric,
#else
  GClosureNotify,
  mkFunPtrClosureNotify,
#endif
  ) where

import Control.Monad (liftM)
import System.Glib.FFI
import System.Glib.GType
import System.Glib.Flags
{#import System.Glib.GObject#}
#ifndef USE_GCLOSURE_SIGNALS_IMPL
import Data.IORef
#endif

{#context lib="glib" prefix="g" #}

newtype Signal object handler =
  Signal (Bool -> object -> handler -> IO (ConnectId object))

-- | Perform an action in response to a signal.
--
-- Use it like this:
--
-- > on obj sig $ do
-- >   ...
--
-- or if the signal handler takes any arguments:
--
-- > on obj sig $ \args -> do
-- >   ...
--
on ::
    object
 -> Signal object callback
 -> callback
 -> IO (ConnectId object)
on    object (Signal connect) handler = connect False object handler

-- | Perform an action in response to a signal.
--
-- * Like 'on' but the signal is executed after Gtk's default handler has
--   run.
--
after ::
    object
 -> Signal object callback
 -> callback
 -> IO (ConnectId object)
after object (Signal connect) handler = connect True  object handler

-- Specify if the handler is to run before (False) or after (True) the
-- default handler.

type ConnectAfter = Bool

type SignalName = String

-- | The type of signal handler ids. If you ever need to 'disconnect' a signal
-- handler then you will need to retain the 'ConnectId' you got when you
-- registered it.
--
data GObjectClass o => ConnectId o = ConnectId {#type gulong#} o

-- old name for backwards compatability
disconnect :: GObjectClass obj => ConnectId obj -> IO ()
disconnect = signalDisconnect
{-# DEPRECATED disconnect "use signalDisconnect instead" #-}

-- | Disconnect a signal handler. After disconnecting the handler will no
-- longer be invoked when the event occurs.
--
signalDisconnect :: GObjectClass obj => ConnectId obj -> IO ()
signalDisconnect (ConnectId handler obj) =
  withForeignPtr  ((unGObject.toGObject) obj) $ \objPtr ->
  {# call g_signal_handler_disconnect #} (castPtr objPtr) handler

-- | Block a specific signal handler.
--
-- * Blocks a handler of an instance so it will not be called during any
--   signal emissions unless it is unblocked again. Thus \"blocking\" a signal
--   handler means to temporarily deactive it, a signal handler has to be
--   unblocked exactly the same amount of times it has been blocked before
--   to become active again.
--
signalBlock :: GObjectClass obj => ConnectId obj -> IO ()
signalBlock (ConnectId handler obj) =
  withForeignPtr  ((unGObject.toGObject) obj) $ \objPtr ->
  {# call g_signal_handler_block #} (castPtr objPtr) handler

{# enum GSignalMatchType {underscoreToCase} deriving (Eq, Ord, Bounded) #}
instance Flags GSignalMatchType

-- | Blocks all handlers on an instance that match a certain selection
-- criteria. The criteria mask is passed as a list of `GSignalMatchType` flags,
-- and the criteria values are passed as arguments. Passing at least one of
-- the `SignalMatchClosure`, `SignalMatchFunc` or `SignalMatchData` match flags
-- is required for successful matches. If no handlers were found, 0 is returned,
-- the number of blocked handlers otherwise.
signalBlockMatched :: GObjectClass obj
                   => obj
                   -> [GSignalMatchType]
                   -> SignalName
                   -> GType
                   -> Quark
                   -> Maybe GClosure
                   -> Maybe (Ptr ())
                   -> Maybe (Ptr ())
                   -> IO Int
signalBlockMatched obj mask sigName gType quark closure func userData = do
  sigId <- withCString sigName $ \strPtr ->
                {# call g_signal_lookup #} strPtr gType
  liftM fromIntegral $ withForeignPtr (unGObject $ toGObject obj) $ \objPtr ->
    {# call g_signal_handlers_block_matched #}
        (castPtr objPtr)
        (fromIntegral $ fromFlags mask)
        sigId
        quark
        (maybe nullPtr (\(GClosure p) -> castPtr p) closure)
        (maybe nullPtr id func)
        (maybe nullPtr id userData)

-- | Unblock a specific signal handler.
--
-- * Undoes the effect of a previous 'signalBlock' call. A blocked handler
--   is skipped during signal emissions and will not be invoked, unblocking
--   it (for exactly the amount of times it has been blocked before) reverts
--   its \"blocked\" state, so the handler will be recognized by the signal
--   system and is called upon future or currently ongoing signal emissions
--   (since the order in which handlers are called during signal emissions
--   is deterministic, whether the unblocked handler in question is called
--   as part of a currently ongoing emission depends on how far that
--   emission has proceeded yet).
--
signalUnblock :: GObjectClass obj => ConnectId obj -> IO ()
signalUnblock (ConnectId handler obj) =
  withForeignPtr  ((unGObject.toGObject) obj) $ \objPtr ->
  {# call g_signal_handler_unblock #} (castPtr objPtr) handler

-- | Stops a signal's current emission.
--
-- * This will prevent the default method from running. The sequence in which
--   handlers are run is \"first\", \"on\", \"last\" then \"after\" where
--   Gtk-internal
--   signals are connected either at \"first\" or at \"last\". Hence this
--   function can only stop the signal processing if it is called from within
--   a handler that is connected with an \"on\" signal and if the Gtk-internal
--   handler is connected as \"last\". Gtk prints a warning if this function
--   is used on a signal which isn't being emitted.
--
signalStopEmission :: GObjectClass obj => obj -> SignalName -> IO ()
signalStopEmission obj sigName =
  withForeignPtr  ((unGObject.toGObject) obj) $ \objPtr ->
  withCString sigName $ \strPtr ->
  {# call g_signal_stop_emission_by_name #} (castPtr objPtr) strPtr

{# pointer *GClosure newtype #}

#ifdef USE_GCLOSURE_SIGNALS_IMPL

connectGeneric :: GObjectClass obj =>
    SignalName
 -> ConnectAfter
 -> obj
 -> handler
 -> IO (ConnectId obj)
connectGeneric signal after obj user = do
  sptr <- newStablePtr user
  gclosurePtr <- gtk2hs_closure_new sptr
  sigId <-
    withCString signal $ \signalPtr ->
    withForeignPtr ((unGObject.toGObject) obj) $ \objPtr ->
    {# call g_signal_connect_closure #}
      (castPtr objPtr)
      signalPtr
      (GClosure gclosurePtr)
      (fromBool after)
  return $ ConnectId sigId obj

foreign import ccall unsafe "gtk2hs_closure_new"
  gtk2hs_closure_new :: StablePtr a -> IO (Ptr GClosure)

#else

{#pointer GClosureNotify#}

foreign import ccall "wrapper" mkDestructor :: IO () -> IO GClosureNotify

mkFunPtrClosureNotify :: FunPtr a -> IO GClosureNotify
mkFunPtrClosureNotify hPtr = do
  dRef <- newIORef nullFunPtr
  dPtr <- mkDestructor $ do
    freeHaskellFunPtr hPtr
    dPtr <- readIORef dRef
    freeHaskellFunPtr dPtr
  writeIORef dRef dPtr
  return dPtr

#endif
