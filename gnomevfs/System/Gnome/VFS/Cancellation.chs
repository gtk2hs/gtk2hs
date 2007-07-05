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
module System.Gnome.VFS.Cancellation (
  
-- * Types
  Cancellation,

-- * Cancellation creation
  cancellationNew,

-- * Cancellation notification
  cancellationCancel,
  cancellationCheck,
  cancellationAck,

-- * Other Operations
  cancellationGetFD
  
  ) where

import Control.Monad      (liftM)
import System.Glib.FFI
{#import System.Gnome.VFS.Types#}
import System.Posix.Types (Fd)

{# context lib = "gnomevfs" prefix = "gnome_vfs" #}

-- | Create a new 'Cancellation' object for reporting
--   cancellation to a gnome-vfs module.
cancellationNew :: IO Cancellation -- ^ a new 'Cancellation' object
cancellationNew =
    {# call cancellation_new #} >>= newCancellation

-- | Send a cancellation request through a 'Cancellation' object.
cancellationCancel :: Cancellation -- ^ @cancellation@ - the object to request cancellation through
                   -> IO ()
cancellationCancel cancellation =
    {# call cancellation_cancel #} cancellation

-- | Check for pending cancellation.
cancellationCheck :: Cancellation -- ^ @cancellation@ - the object to check for cancellation
                  -> IO Bool      -- ^ 'True' if cancellation has been requested, 'False' otherwise
cancellationCheck cancellation =
    liftM toBool $ {# call cancellation_check #} cancellation

-- | Acknowledge a cancellation. This should be called if
--   'cancellationCheck' returns 'True'.
cancellationAck :: Cancellation -- ^ @cancellation@ - the object to achnowledge cancellation
                -> IO ()
cancellationAck cancellation =
    {# call cancellation_ack #} cancellation

-- | Get a file descriptor-based notificator for cancellation. When
--   cancellation receives a cancellation request, a character will be
--   made available on the returned file descriptor for input.
--   
--   This is very useful for detecting cancellation during I\/O
--   operations: you can use the select() call to check for available
--   input\/output on the file you are reading\/writing, and on the
--   notificator's file descriptor at the same time. If a data is
--   available on the notificator's file descriptor, you know you have
--   to cancel the read\/write operation.
cancellationGetFD :: Cancellation -- ^ @cancellation@ - the object to get a file descriptor for
                  -> IO Fd        -- ^ the file descriptor
cancellationGetFD cancellation =
    liftM fromIntegral $ {# call cancellation_get_fd #} cancellation
