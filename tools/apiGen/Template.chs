-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget @OBJECT_NAME@
--
--  Author : [Insert your full name here]
--
--  Created: @DATE@
--
--  Copyright (C) @YEAR@ [Insert your full name here]
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
-- Maintainer  : gtk2hs-users\@lists.sourceforge.net
-- Stability   : provisional
-- Portability : non-portable (uses gtk+ C library)
--
-- @DESCRIPTION@@TODO@
--
module @MODULE_NAME@ (
@DOCUMENTATION@
@EXPORTS@
  ) where

import Monad (liftM)

import System.Glib.FFI
@IMPORTS@
{# context lib="@CONTEXT_LIB@" prefix="@CONTEXT_PREFIX@" #}

@MODULE_BODY@
