-- -*-haskell-*-
--  GIMP Toolkit (GTK) CellRendererPixbuf
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/12 17:19:26 $
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
-- Renders a pixbuf in a cell
--
module Graphics.UI.Gtk.TreeList.CellRendererPixbuf (
  CellRendererPixbuf,
  CellRendererPixbufClass,
  castToCellRendererPixbuf,
  cellRendererPixbufNew,
--  cellPixbuf
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object		(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.TreeList.CellRenderer	(Attribute(..))
import Graphics.UI.Gtk.Display.Image		(imageNewFromPixbuf,
						 imageGetPixbuf)
import System.Glib.StoreValue   (GenericValue(..), TMType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new CellRendererPixbuf object.
--
cellRendererPixbufNew :: IO CellRendererPixbuf
cellRendererPixbufNew  = makeNewObject mkCellRendererPixbuf $ liftM castPtr $
  {#call unsafe cell_renderer_pixbuf_new#}

-- | Define the attribute that specifies the
-- 'Pixbuf' to be rendered.
--
--cellPixbuf :: Attribute CellRendererPixbuf Image
--cellPixbuf  = Attribute ["pixbuf"] [TMobject]
--  (liftM ((\x -> [x]) . GVobject . toGObject) . imageGetPixbuf)
--  (\[GVobject obj] -> imageNewFromPixbuf (fromGObject obj))
