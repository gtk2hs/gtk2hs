-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget VSeparator
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/12 17:19:24 $
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
-- The VSeparator widget is a horizontal separator, used to group the 
-- widgets within a window. It displays a horizontal line with a shadow 
-- to make it appear sunken into the interface.
--
-- * This has nothing to do with a menu separator.
--
module Graphics.UI.Gtk.Ornaments.VSeparator (
  VSeparator,
  VSeparatorClass,
  castToVSeparator,
  vSeparatorNew
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

-- methods

vSeparatorNew :: IO VSeparator
vSeparatorNew = makeNewObject mkVSeparator $ 
  liftM castPtr {#call unsafe vseparator_new#}
