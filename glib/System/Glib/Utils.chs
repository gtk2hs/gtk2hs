-- -*-haskell-*-
--  GIMP Toolkit (GTK) Miscellaneous utilities
--
--  Author : John Millikin
--
--  Created: 15 November 2009
--
--  Copyright (C) 2009 John Millikin
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
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- This module binds GLib-specific utility procedures.
--
module System.Glib.Utils
  ( getApplicationName
  , setApplicationName
  , getProgramName
  , setProgramName
  ) where

import System.Glib.FFI
import System.Glib.UTFString

{# context lib="glib" prefix="g" #}

-- |
-- Gets a human-readable name for the application, as set by
-- 'setApplicationName'. This name should be localized if possible, and is
-- intended for display to the user. Contrast with 'getProgramName', which
-- gets a non-localized name. If 'setApplicationName' has not been performed,
-- returns the result of 'getProgramName' (which may be 'Nothing' if
-- 'setProgramName' has also not been performed).
--
getApplicationName :: GlibString string => IO (Maybe string)
getApplicationName = {#call unsafe get_application_name #} >>= maybePeek peekUTFString

-- |
-- Sets a human-readable name for the application. This name should be
-- localized if possible, and is intended for display to the user. Contrast
-- with 'setProgramName', which sets a non-localized name. 'setProgramName'
-- will be performed automatically by 'initGUI', but 'setApplicationName'
-- will not.
--
-- Note that for thread safety reasons, this computation can only be performed
-- once.
--
-- The application name will be used in contexts such as error messages, or
-- when displaying an application's name in the task list.
--
setApplicationName :: GlibString string => string -> IO ()
setApplicationName = flip withUTFString {#call unsafe set_application_name #}

-- |
-- Gets the name of the program. This name should /not/ be localized, contrast
-- with 'getApplicationName'. If you are using GDK or GTK+, the program name
-- is set in 'initGUI' to the last component of argv[0].
--
getProgramName :: GlibString string => IO (Maybe string)
getProgramName = {#call unsafe get_prgname #} >>= maybePeek peekUTFString

-- |
-- Sets the name of the program. This name should /not/ be localized, contrast
-- with 'setApplicationName'. Note that for thread-safety reasons this
-- computation can only be performed once.
--
setProgramName :: GlibString string => string -> IO ()
setProgramName = flip withUTFString {#call unsafe set_prgname #}
