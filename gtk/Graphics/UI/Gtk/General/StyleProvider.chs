{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Styles
--
--  Author : Axel Simon
--
--  Created: 13 February 2003
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- Interface to provide style information to @StyleContext@
--
module Graphics.UI.Gtk.General.StyleProvider (
-- * Description
--
-- | @StyleProvider@ is an interface used to provide style information
-- to a @StyleContext@. See @styleContextAddProvider@ and
-- @styleContextAddProviderForScreen@.
--
-- @StyleProvider@ is implemented by @CssProvider@ and @Settings@.
#if GTK_MAJOR_VERSION >= 3
-- * Typess
  StyleProvider,
  StyleProviderClass,
  castToStyleProvider, gTypeStyleProvider,
  toStyleProvider,
#endif

  ) where

{# context prefix ="gtk" #}

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Types#}
import System.Glib.GError (GError(..), GErrorClass(..), GErrorDomain,
                           propagateGError)

#if GTK_MAJOR_VERSION >= 3
#endif
