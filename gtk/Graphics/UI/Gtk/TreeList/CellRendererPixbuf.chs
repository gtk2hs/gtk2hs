-- -*-haskell-*-
--  GIMP Toolkit (GTK) CellRendererPixbuf
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
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
-- * This module and all other modules in 'Graphics.UI.Gtk.TreeList' are
--   deprecated. Please use the modules in 'Graphics.UI.Gtk.ModelView'.
--
module Graphics.UI.Gtk.TreeList.CellRendererPixbuf (
-- * Detail
-- 
-- | A 'CellRendererPixbuf' can be used to render an image in a cell. It
-- allows to render either a given 'Pixbuf' (set via the pixbuf property) or a
-- stock icon (set via the stock-id property).
--
-- To support the tree view, 'CellRendererPixbuf' also supports rendering
-- two alternative pixbufs, when the is-expander property is @True@. If the
-- is-expanded property is @True@ and the pixbuf-expander-open property is set
-- to a pixbuf, it renders that pixbuf, if the is-expanded property is @False@
-- and the pixbuf-expander-closed property is set to a pixbuf, it renders that
-- one.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'CellRenderer'
-- |               +----CellRendererPixbuf
-- @

-- * Types
  CellRendererPixbuf,
  CellRendererPixbufClass,
  castToCellRendererPixbuf, gTypeCellRendererPixbuf,
  toCellRendererPixbuf,

-- * Constructors
  cellRendererPixbufNew,
--  cellPixbuf
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object		(makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new CellRendererPixbuf object.
--
cellRendererPixbufNew :: IO CellRendererPixbuf
cellRendererPixbufNew =
  makeNewObject mkCellRendererPixbuf $
  liftM (castPtr :: Ptr CellRenderer -> Ptr CellRendererPixbuf) $
  {# call unsafe cell_renderer_pixbuf_new #}

--------------------
-- Properties

-- | Define the attribute that specifies the
-- 'Pixbuf' to be rendered.
--
--cellPixbuf :: Attribute CellRendererPixbuf Image
--cellPixbuf  = Attribute ["pixbuf"] [TMobject]
--  (liftM ((\x -> [x]) . GVobject . toGObject) . imageGetPixbuf)
--  (\[GVobject obj] -> imageNewFromPixbuf (fromGObject obj))
