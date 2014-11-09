{-# LANGUAGE CPP #-}
--  GIMP Toolkit (GTK) Binding for Haskell: binding to gio -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 13-Oct-2008
--
--  Copyright (c) 2008 Peter Gavin
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
module System.GIO.Async.Cancellable (
-- * Details
-- | 'Cancellable' is a thread-safe operation cancellation stack used throughout GIO to allow for
-- cancellation of synchronous and asynchronous operations.

-- * Types
    Cancellable (..),
    CancellableClass,

-- * Methods
    cancellableNew,
    cancellableIsCancelled,
    cancellableThrowErrorIfCancelled,
    cancellableGetCurrent,
    cancellablePopCurrent,
    cancellablePushCurrent,
    cancellableReset,
    cancellableCancel,

-- * Signals
    cancellableCancelled
    ) where

import Control.Monad
import Data.Maybe (fromMaybe)
import System.Glib.FFI
import System.Glib.GError
import System.Glib.GObject
import System.Glib.Signals
{#import System.GIO.Signals#}
{#import System.GIO.Types#}

{# context lib = "gio" prefix = "g" #}

-- | Creates a new 'Cancellable' object.
--
-- Applications that want to start one or more operations that should be cancellable should create a
-- 'Cancellable' and pass it to the operations.
--
-- One 'Cancellable' can be used in multiple consecutive operations, but not in multiple concurrent
-- operations.
cancellableNew :: IO Cancellable
cancellableNew =
    wrapNewGObject mkCancellable $
    {# call cancellable_new #}

-- | Checks if a cancellable job has been cancelled.
cancellableIsCancelled :: Cancellable
 -> IO Bool -- ^ returns 'True' if cancellable is cancelled, 'False' if called with 'Nothing' or if item is not cancelled.
cancellableIsCancelled =
    liftM toBool . {# call cancellable_is_cancelled #}

-- | If the cancellable is cancelled, throws a 'GError' to notify that the operation was cancelled.
cancellableThrowErrorIfCancelled :: Cancellable -> IO ()
cancellableThrowErrorIfCancelled cancellable =
    propagateGError $ \gErrorPtr -> do
      {# call cancellable_set_error_if_cancelled #} cancellable gErrorPtr
      return ()

-- | Gets the top cancellable from the stack.
cancellableGetCurrent ::
  IO (Maybe Cancellable)  -- ^ returns a 'Cancellable' from the top of the stack, or 'Nothing' if the stack is empty.
cancellableGetCurrent =
    maybeNull (makeNewGObject mkCancellable) $
    {# call cancellable_get_current #}

-- | Pops cancellable off the cancellable stack (verifying that cancellable is on the top of the stack).
cancellablePopCurrent :: Maybe Cancellable -> IO ()
cancellablePopCurrent cancellable =
      {# call cancellable_pop_current #}
         (fromMaybe (Cancellable nullForeignPtr) cancellable)

-- | Pushes cancellable onto the cancellable stack. The current cancllable can then be recieved using
-- 'cancellableGetCurrent' .
--
-- This is useful when implementing cancellable operations in code that does not allow you to pass down
-- the cancellable object.
--
-- This is typically called automatically by e.g. 'File' operations, so you rarely have to call this
-- yourself.
cancellablePushCurrent :: Maybe Cancellable -> IO ()
cancellablePushCurrent cancellable =
      {# call cancellable_push_current #}
         (fromMaybe (Cancellable nullForeignPtr) cancellable)

-- | Resets cancellable to its uncancelled state.
cancellableReset :: Cancellable -> IO ()
cancellableReset = {# call cancellable_reset #}

-- | Will set cancellable to cancelled, and will emit the "cancelled" signal. (However, see the warning
-- about race conditions in the documentation for that signal if you are planning to connect to it.)
--
-- This function is thread-safe. In other words, you can safely call it from a thread other than the
-- one running the operation that was passed the cancellable.
--
-- The convention within gio is that cancelling an asynchronous operation causes it to complete
-- asynchronously. That is, if you cancel the operation from the same thread in which it is running,
-- then the operation's 'AsyncReadyCallback' will not be invoked until the application returns to the
-- main loop.
cancellableCancel :: Cancellable -> IO ()
cancellableCancel = {# call cancellable_cancel #}

-- | Emitted when the operation has been cancelled.
--
-- Can be used by implementations of cancellable operations. If the operation is cancelled from another
-- thread, the signal will be emitted in the thread that cancelled the operation, not the thread that
-- is running the operation.
--
-- Note that disconnecting from this signal (or any signal) in a multi-threaded program is prone to
-- race conditions. For instance it is possible that a signal handler may be invoked even after a call
-- to 'signalHandlerDisconnect' for that handler has already returned.
--
-- There is also a problem when cancellation happen right before connecting to the signal. If this
-- happens the signal will unexpectedly not be emitted, and checking before connecting to the signal
-- leaves a race condition where this is still happening.
cancellableCancelled :: Signal Cancellable (IO ())
cancellableCancelled =
    Signal $ connect_NONE__NONE "cancelled"
