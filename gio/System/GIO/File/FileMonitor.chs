{-# LANGUAGE CPP #-}
--  GIMP Toolkit (GTK) Binding for Haskell: binding to gio -*-haskell-*-
--
--  Author : Andy Stewart
--  Created: 30-Apirl-2010
--
--  Copyright (c) 2010 Andy Stewart
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
--  GIO, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original GIO documentation.
--
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module System.GIO.File.FileMonitor (
-- * Details
--
-- | Monitors a file or directory for changes.
--
-- To obtain a 'FileMonitor' for a file or directory, use 'fileMonitor', or
-- 'fileMonitorDirectory' .
--
-- To get informed about changes to the file or directory you are monitoring, connect to the "changed"
-- signal. The signal will be emitted in the thread-default main context of the thread that the monitor
-- was created in (though if the global default main context is blocked, this may cause notifications
-- to be blocked even if the thread-default context is still running).

-- * Types
   FileMonitor(..),
   FileMonitorClass,

-- * Enums
   FileMonitorEvent(..),

-- * Methods
    fileMonitorCancel,
    fileMonitorIsCancelled,

-- * Attributes
    fileMonitorCancelled,
    fileMonitorRateLimit,

-- * Signals
    fileMonitorChanged,
    ) where

import Control.Monad
import System.GIO.Enums
import System.Glib.Attributes
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
import System.Glib.GObject
import System.Glib.Properties
import System.Glib.Signals
import System.Glib.UTFString
{#import System.GIO.Signals#}
{#import System.GIO.Types#}

--------------------
-- Methods
-- | Cancels a file monitor.
fileMonitorCancel :: FileMonitorClass monitor => monitor
                  -> IO Bool  -- ^ returns 'True' if monitor was cancelled.
fileMonitorCancel monitor =
  liftM toBool $ {#call g_file_monitor_cancel #} (toFileMonitor monitor)

-- | Returns whether the monitor is canceled.
fileMonitorIsCancelled :: FileMonitorClass monitor => monitor
                       -> IO Bool -- ^ returns 'True' if monitor is canceled. 'False' otherwise.
fileMonitorIsCancelled monitor =
  liftM toBool $ {#call g_file_monitor_is_cancelled#} (toFileMonitor monitor)

--------------------
-- Attributes
-- | Whether the monitor has been cancelled.
--
-- Default value: 'False'
fileMonitorCancelled :: FileMonitorClass monitor => ReadAttr monitor Bool
fileMonitorCancelled = readAttrFromBoolProperty "cancelled"

-- | The limit of the monitor to watch for changes, in milliseconds.
--
-- Allowed values: >= 0
--
-- Default value: 800
fileMonitorRateLimit :: FileMonitorClass monitor => Attr monitor Int
fileMonitorRateLimit = newAttrFromIntProperty "rate-limit"

--------------------
-- Signals
fileMonitorChanged :: FileMonitorClass monitor => Signal monitor (Maybe File -> Maybe File -> FileMonitorEvent -> IO ())
fileMonitorChanged = Signal (connect_MOBJECT_MOBJECT_ENUM__NONE "changed")

