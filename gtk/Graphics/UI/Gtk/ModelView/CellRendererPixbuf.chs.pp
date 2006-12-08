-- -*-haskell-*-
--  GIMP Toolkit (GTK) CellRendererPixbuf
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.6 $ from $Date: 2005/11/12 15:10:37 $
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
  castToCellRendererPixbuf,
  toCellRendererPixbuf,

-- * Constructors
  cellRendererPixbufNew,
-- * Attributes
  cellPixbuf,
  cellPixbufExpanderOpen,
  cellPixbufExpanderClosed,
  cellPixbufStockId,
  cellPixbufStockSize,
  cellPixbufStockDetail,
#if GTK_CHECK_VERSION(2,8,0)
  cellPixbufIconName,
  cellPixbufFollowState,
#endif
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.Attributes                   (Attr)
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object		(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

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
-- Attributes

-- | The pixbuf to render.
--
cellPixbuf :: CellRendererPixbufClass self => Attr self Pixbuf
cellPixbuf = newAttrFromObjectProperty "pixbuf"
  {# call pure unsafe gdk_pixbuf_get_type #}

-- | Pixbuf for open expander.
--
cellPixbufExpanderOpen :: CellRendererPixbufClass self => Attr self Pixbuf
cellPixbufExpanderOpen = newAttrFromObjectProperty "pixbuf-expander-open"
  {# call pure unsafe gdk_pixbuf_get_type #}
  
-- | Pixbuf for closed expander.
--
cellPixbufExpanderClosed :: CellRendererPixbufClass self => Attr self Pixbuf
cellPixbufExpanderClosed = newAttrFromObjectProperty "pixbuf-expander-closed"
  {# call pure unsafe gdk_pixbuf_get_type #}

-- | The stock ID of the stock icon to render.
--
-- Default value: @\"\"@
--
cellPixbufStockId :: CellRendererPixbufClass self => Attr self String
cellPixbufStockId = newAttrFromStringProperty "stock-id"

-- | The 'IconSize' value that specifies the size of the rendered icon.
--
-- Default value: 1
--
cellPixbufStockSize :: CellRendererPixbufClass self => Attr self Int
cellPixbufStockSize = newAttrFromUIntProperty "stock-size"

-- | Render detail to pass to the theme engine.
--
-- Default value: @\"\"@
--
cellPixbufStockDetail :: CellRendererPixbufClass self => Attr self String
cellPixbufStockDetail = newAttrFromStringProperty "stock-detail"

#if GTK_CHECK_VERSION(2,8,0)
-- | The name of the themed icon to display. This property only has an effect
-- if not overridden by 'cellPixbufStockId' or 'cellPixbuf' attributes.
--
-- Default value: @\"\"@
--
cellPixbufIconName :: CellRendererPixbufClass self => Attr self String
cellPixbufIconName = newAttrFromStringProperty "icon-name"

-- | Specifies whether the rendered pixbuf should be colorized according to
-- the 'CellRendererState'.
--
-- Default value: @False@
--
cellPixbufFollowState :: CellRendererPixbufClass self => Attr self Bool
cellPixbufFollowState = newAttrFromBoolProperty "follow-state"
#endif
