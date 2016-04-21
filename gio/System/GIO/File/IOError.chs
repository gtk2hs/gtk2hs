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
module System.GIO.File.IOError (
-- * Details
--
-- | Contains helper functions for reporting errors to the user.

-- * Enums
   IOErrorEnum(..),

-- * Methods
   ioErrorFromErrno,
   ) where

import Control.Monad
import System.GIO.Enums
import System.Glib.FFI

--------------------
-- Methods

-- | Converts errno.h error codes into GIO error codes.
ioErrorFromErrno :: Int  -- ^ @err@ Error number as defined in errno.h.
                 -> IO IOErrorEnum  -- ^ returns 'IOErrorEnum' value for the given errno.h error number.
ioErrorFromErrno err =
  liftM (toEnum . fromIntegral) $ {#call g_io_error_from_errno#} (fromIntegral err)
