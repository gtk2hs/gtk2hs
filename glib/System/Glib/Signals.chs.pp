-- -*-haskell-*-
--  Callback installers for the GIMP Toolkit (GTK) Binding for Haskell
--
--  Author : Axel Simon
--
--  Created: 1 July 2000
--
--  Version $Revision: 1.5 $ from $Date: 2005/07/23 01:09:51 $
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
-- #hide

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
  SignalName,
  ConnectAfter,
  ConnectId(ConnectId),
  disconnect,
  GClosure,
#ifdef USE_GCLOSUE_SIGNALS_IMPL
  connectGeneric,
#else
  GClosureNotify,
  mkFunPtrClosureNotify,
#endif
  ) where

import Data.IORef              (newIORef, readIORef, writeIORef)

import System.Glib.FFI
import System.Glib.UTFString   (peekUTFString, newUTFString)
import System.Glib.GError      (failOnGError)
{#import System.Glib.GObject#}

{#context lib="glib" prefix="g" #}


-- Specify if the handler is to run before (False) or after (True) the
-- default handler.

type ConnectAfter = Bool

type SignalName = String

data GObjectClass o => ConnectId o = ConnectId {#type gulong#} o

disconnect :: GObjectClass obj => ConnectId obj -> IO ()
disconnect (ConnectId handler obj) =
  withForeignPtr  ((unGObject.toGObject) obj) $ \objPtr ->
  {# call g_signal_handler_disconnect #} (castPtr objPtr) handler

{# pointer *GClosure newtype #}

#ifdef USE_GCLOSUE_SIGNALS_IMPL

connectGeneric :: GObjectClass obj =>
    SignalName
 -> ConnectAfter
 -> obj
 -> handler
 -> IO (ConnectId obj)
connectGeneric signal after obj user = do
  sptr <- newStablePtr user
  gclosurePtr <- hsg_closure_new sptr
  sigId <- 
    withCString signal $ \signalPtr -> 
    withForeignPtr ((unGObject.toGObject) obj) $ \objPtr ->
    {# call g_signal_connect_closure #}
      (castPtr objPtr)
      signalPtr
      (GClosure gclosurePtr)
      (fromBool after)
  return $ ConnectId sigId obj

foreign import ccall unsafe "hsg_closure_new"
  hsg_closure_new :: StablePtr a -> IO (Ptr GClosure)

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
