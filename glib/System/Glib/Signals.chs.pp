-- -*-haskell-*-
--  Callback installers for the GIMP Toolkit (GTK) Binding for Haskell
--
--  Author : Axel Simon
--          
--  Created: 1 July 2000
--
--  Version $Revision: 1.1 $ from $Date: 2005/05/14 02:01:49 $
--
--  Copyright (c) 2000 Axel Simon
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- #hide

-- |  The object system in the second version of GTK is based on GObject from
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
  ConnectId,
  connectGeneric,
  disconnect
  ) where

import System.Glib.FFI
import System.Glib.UTFString   (peekUTFString, newUTFString)
import System.Glib.GError      (failOnGError)
{#import System.Glib.GObject#}

{#context lib="glib" prefix="g" #}


-- Specify if the handler is to run before (False) or after (True) the
-- default handler.

type ConnectAfter = Bool

type SignalName = String

data GObjectClass o => ConnectId o = ConnectID {#type gulong#} o

disconnect :: GObjectClass obj => ConnectId obj -> IO ()
disconnect (ConnectID handler obj) =
  withForeignPtr  ((unGObject.toGObject) obj) $ \objPtr ->
  {# call unsafe g_signal_handler_disconnect #} (castPtr objPtr) handler

{# pointer *GClosure newtype #}

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
  return $ ConnectID sigId obj

foreign import ccall unsafe "hsg_closure_new"
  hsg_closure_new :: StablePtr a -> IO (Ptr GClosure)
