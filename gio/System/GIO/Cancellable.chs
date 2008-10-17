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
--  this library is based on the original GIO documentation,
--  Copyright (c) 2001 Seth Nickell <snickell@stanford.edu>. The
--  documentation is covered by the GNU Free Documentation License,
--  version 1.2.
--  
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module System.GIO.Cancellable (
    Cancellable,
    cancellableNew,
    cancellableIsCancelled,
    cancellableThrowErrorIfCancelled,
    cancellableGetCurrent,
    cancellablePopCurrent,
    cancellablePushCurrent,
    cancellableReset,
    cancellableCancel,
    cancellableCancelled
    ) where

import Control.Monad

import System.Glib.FFI
import System.Glib.GError
import System.Glib.Signals

import System.GIO.Base
{#import System.GIO.Types#}
{#import System.GIO.Signals#}

{# context lib = "gio" prefix = "g" #}

cancellableNew :: IO Cancellable
cancellableNew =
    {# call cancellable_new #} >>= takeGObject

cancellableIsCancelled :: Cancellable -> IO Bool
cancellableIsCancelled =
    liftM toBool . {# call cancellable_is_cancelled #}

cancellableThrowErrorIfCancelled :: Cancellable -> IO ()
cancellableThrowErrorIfCancelled cancellable =
    propagateGError $ \gErrorPtr -> do
      {# call cancellable_set_error_if_cancelled #} cancellable gErrorPtr
      return ()

cancellableGetCurrent :: IO (Maybe Cancellable)
cancellableGetCurrent =
    {# call cancellable_get_current #} >>= maybePeek takeGObject

cancellablePopCurrent :: Maybe Cancellable -> IO ()
cancellablePopCurrent cancellable =
    maybeWith withGObject cancellable g_cancellable_pop_current
    where
      _ = {# call cancellable_pop_current #}

cancellablePushCurrent :: Maybe Cancellable -> IO ()
cancellablePushCurrent cancellable =
    maybeWith withGObject cancellable g_cancellable_push_current
    where
      _ = {# call cancellable_push_current #}

cancellableReset :: Cancellable -> IO ()
cancellableReset = {# call cancellable_reset #}

cancellableCancel :: Cancellable -> IO ()
cancellableCancel = {# call cancellable_cancel #}

cancellableCancelled :: Signal Cancellable (IO ())
cancellableCancelled =
    Signal $ connect_NONE__NONE "cancelled"
