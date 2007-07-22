--  GIMP Toolkit (GTK) Binding for Haskell: binding to libgnomevfs -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
--
--  Copyright (c) 2007 Peter Gavin
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
--  GnomeVFS, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original GnomeVFS documentation,
--  Copyright (c) 2001 Seth Nickell <snickell@stanford.edu>. The
--  documentation is covered by the GNU Free Documentation License,
--  version 1.2.
--  
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module System.Gnome.VFS.Error (
  
  Error(..),
  
  error,
  errors,
  eofErrors,
  
  ) where

import Control.Monad (join)
import qualified Control.Exception as E
import Data.Dynamic
import System.Gnome.VFS.Types
import Prelude hiding (error)

error :: Result
      -> IO a
error = E.throwDyn . Error

errors :: E.Exception
       -> Maybe Error
errors =
    join . (fmap fromDynamic) . E.dynExceptions

eofErrors :: E.Exception
          -> Maybe Error
eofErrors exception =
    let vfsError = errors exception in
    case vfsError of
      Just (Error ErrorEof) -> vfsError
      _                     -> Nothing
