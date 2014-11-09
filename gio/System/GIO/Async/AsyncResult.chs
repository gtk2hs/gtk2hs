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
module System.GIO.Async.AsyncResult (
-- * Types
    AsyncResult,
    AsyncResultClass,
    AsyncReadyCallback,

-- * Methods
    marshalAsyncReadyCallback,
    ) where

import Control.Monad
import System.Glib.FFI
import System.Glib.GObject
{#import System.GIO.Types#}

type AsyncReadyCallback = GObject -> AsyncResult -> IO ()

{#pointer GAsyncReadyCallback#}

foreign import ccall "wrapper" mkAsyncReadyCallback ::
    (Ptr () -> Ptr AsyncResult -> Ptr () -> IO ()) -> IO GAsyncReadyCallback

marshalAsyncReadyCallback :: AsyncReadyCallback -> IO GAsyncReadyCallback
marshalAsyncReadyCallback asyncReadyCallback =
    mkAsyncReadyCallback $ \ cObject cAsyncResult cCallback -> do
      object <- (makeNewGObject mkGObject . return) (castPtr cObject)
      asyncResult <- (makeNewGObject mkAsyncResult . return) cAsyncResult
      asyncReadyCallback object asyncResult
      freeHaskellFunPtr (castPtrToFunPtr cCallback)
