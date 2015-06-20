{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget CellView
--
--  Author : Duncan Coutts
--
--  Created: 4 April 2005
--
--  Copyright (C) 2005 Duncan Coutts
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
-- A widget displaying a single row of a 'TreeModel'
--
-- * Module available since Gtk+ version 2.6
--
module Graphics.UI.Gtk.ModelView.CellView (
-- * Detail
--
-- | A 'CellView' displays a single row of a 'TreeModel', using cell renderers
-- just like 'TreeView'. 'CellView' doesn't support some of the more complex
-- features of 'TreeView', like cell editing and drag and drop.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----CellView
-- @

#if GTK_CHECK_VERSION(2,6,0)
-- * Types
  CellView,
  CellViewClass,
  castToCellView, gTypeCellView,
  toCellView,

-- * Constructors
  cellViewNew,
  cellViewNewWithMarkup,
  cellViewNewWithPixbuf,
  cellViewNewWithText,

-- * Methods
  cellViewSetModel,
  cellViewGetSizeOfRow,
  cellViewSetBackgroundColor,
#if GTK_MAJOR_VERSION < 3
  cellViewGetCellRenderers,
#endif

-- * Attributes
  cellViewBackground
#endif
  ) where

import Control.Monad    (liftM)
import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties                   (writeAttrFromStringProperty)
#if GTK_MAJOR_VERSION < 3
{#import System.Glib.GList#}
#endif
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.Abstract.Object          (makeNewObject)
{#import Graphics.UI.Gtk.ModelView.Types#}
import Graphics.UI.Gtk.General.Structs          (Color, Requisition)

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,6,0)
--------------------
-- Constructors

-- | Creates a new 'CellView' widget.
--
cellViewNew :: IO CellView
cellViewNew =
  makeNewObject mkCellView $
  liftM (castPtr :: Ptr Widget -> Ptr CellView) $
  {# call gtk_cell_view_new #}

-- | Creates a new 'CellView' widget, adds a 'CellRendererText' to it, and
-- makes its show @markup@. The text can be marked up with the Pango
-- text markup language.
--
cellViewNewWithMarkup :: GlibString string
 => string      -- ^ @markup@ - the text to display in the cell view
 -> IO CellView
cellViewNewWithMarkup markup =
  makeNewObject mkCellView $
  liftM (castPtr :: Ptr Widget -> Ptr CellView) $
  withUTFString markup $ \markupPtr ->
  {# call gtk_cell_view_new_with_markup #}
    markupPtr

-- | Creates a new 'CellView' widget, adds a 'CellRendererPixbuf' to it, and
-- makes its show @pixbuf@.
--
cellViewNewWithPixbuf ::
    Pixbuf      -- ^ @pixbuf@ - the image to display in the cell view
 -> IO CellView
cellViewNewWithPixbuf pixbuf =
  makeNewObject mkCellView $
  liftM (castPtr :: Ptr Widget -> Ptr CellView) $
  {# call gtk_cell_view_new_with_pixbuf #}
    pixbuf

-- | Creates a new 'CellView' widget, adds a 'CellRendererText' to it, and
-- makes its show @text@.
--
cellViewNewWithText :: GlibString string
 => string      -- ^ @text@ - the text to display in the cell view
 -> IO CellView
cellViewNewWithText text =
  makeNewObject mkCellView $
  liftM (castPtr :: Ptr Widget -> Ptr CellView) $
  withUTFString text $ \textPtr ->
  {# call gtk_cell_view_new_with_text #}
    textPtr

--------------------
-- Methods

-- | Sets the model for @cellView@. If @cellView@ already has a model set, it
-- will remove it before setting the new model. If @model@ is @Nothing@, then
-- it will unset the old model.
--
cellViewSetModel :: (CellViewClass self, TreeModelClass model) => self
 -> Maybe model -- ^ @model@ - a 'TreeModel'
 -> IO ()
cellViewSetModel self model =
  {# call gtk_cell_view_set_model #}
    (toCellView self)
    (maybe (TreeModel nullForeignPtr) toTreeModel model)

-- | Returns the size needed by the cell view to display the model
-- row pointed to by @path@.
--
cellViewGetSizeOfRow :: CellViewClass self => self
 -> TreePath            -- ^ @path@ - a 'TreePath'
 -> IO Requisition      -- ^ returns the size requisition
cellViewGetSizeOfRow self path =
  alloca $ \requisitionPtr ->
  withTreePath path $ \path -> do
  {# call gtk_cell_view_get_size_of_row #}
    (toCellView self)
    path
    (castPtr requisitionPtr)
  peek requisitionPtr

-- | Sets the background color of @view@.
--
cellViewSetBackgroundColor :: CellViewClass self => self
 -> Color -- ^ @color@ - the new background color
 -> IO ()
cellViewSetBackgroundColor self color =
  with color $ \colorPtr ->
  {# call gtk_cell_view_set_background_color #}
    (toCellView self)
    (castPtr colorPtr)

#if GTK_MAJOR_VERSION < 3
-- | Returns the cell renderers which have been added to @cellView@.
--
-- Removed in Gtk3.
cellViewGetCellRenderers :: CellViewClass self => self -> IO [CellRenderer]
cellViewGetCellRenderers self =
  {# call gtk_cell_view_get_cell_renderers #}
    (toCellView self)
  >>= fromGList
  >>= mapM (\elemPtr -> makeNewObject mkCellRenderer (return elemPtr))
#endif

--------------------
-- Attributes

-- | Background color as a string.
--
-- Default value: @\"\"@
--
cellViewBackground :: (CellViewClass self, GlibString string) => WriteAttr self string
cellViewBackground = writeAttrFromStringProperty "background"

#endif
