-- GIMP Toolkit (GTK) Binding for Haskell: binding to libgnomevfs   -*-haskell-*-
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
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module System.Gnome.VFS.Monitor (
  
-- * Types
  MonitorHandle,
  MonitorCallback,
  
-- * Operations
  monitorAdd,
  monitorCancel
  
  ) where

import Control.Monad (liftM)
import System.Glib.FFI
import System.Glib.UTFString
{#import System.Gnome.VFS.Marshal#}
{#import System.Gnome.VFS.Types#}

{# context lib = "gnomevfs" prefix = "gnome_vfs" #}

type CMonitorCallback =  Ptr MonitorHandle
                      -> CString
                      -> CString
                      -> {# type GnomeVFSMonitorEventType #}
                      -> {# type gpointer #}
                      -> IO ()

-- | Watch the object at @textURI@ for changes, and call @callback@
--   when a change occurs.
monitorAdd :: String           -- ^ @textURI@ - 
           -> MonitorType      -- ^ @monitorType@ - 
           -> MonitorCallback  -- ^ @callback@ - 
           -> IO MonitorHandle -- ^ a handle to the new monitor
monitorAdd textURI monitorType callback  =
    do cTestURI <- newUTFString textURI
       let cMonitorType = cFromEnum monitorType
       cCallback <- monitorCallbackMarshal callback
       newObjectResultMarshal
           (\cMonitorHandle ->
            MonitorHandle (cMonitorHandle, cCallback))
           (\cMonitorHandlePtr ->
            {# call monitor_add #} (castPtr cMonitorHandlePtr) cTestURI cMonitorType cCallback nullPtr)

monitorCallbackMarshal :: MonitorCallback
                        -> IO {# type GnomeVFSMonitorCallback #}
monitorCallbackMarshal callback =
    let cCallback :: CMonitorCallback
        cCallback cHandle cMonitorURI cInfoURI cEventType cUserData =
            do handle <- liftM (\cHandle -> MonitorHandle (cHandle, castPtrToFunPtr cUserData)) $
                         newForeignPtr_ cHandle
               monitorURI <- peekUTFString cMonitorURI
               infoURI <- peekUTFString cInfoURI
               let eventType = cToEnum cEventType
               callback handle monitorURI infoURI eventType
    in makeMonitorCallback cCallback
foreign import ccall safe "wrapper"
  makeMonitorCallback :: CMonitorCallback
                      -> IO {# type GnomeVFSMonitorCallback #}

-- | Cancels the monitor referred to by @monitorHandle@.
monitorCancel :: MonitorHandle -- ^ @monitorHandle@
              -> IO ()
monitorCancel (MonitorHandle (cMonitorHandle, cCallback)) =
    do freeHaskellFunPtr cCallback
       withForeignPtr cMonitorHandle $
                      voidResultMarshal . {# call monitor_cancel #} . castPtr
