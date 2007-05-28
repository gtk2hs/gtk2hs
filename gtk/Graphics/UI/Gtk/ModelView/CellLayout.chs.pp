-- -*-haskell-*-
--  GIMP Toolkit (GTK) Interface CellLayout
--
--  Author : Axel Simon
--
--  Created: 23 January 2006
--
--  Copyright (C) 2006 Axel Simon
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
-- TODO: the following varargs functions were not bound
--   gtk_cell_layout_set_attributes
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- An interface for packing cells
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.ModelView.CellLayout (
-- * Detail
-- 
-- | 'CellLayout' is an interface which is implemented by all objects which
-- provide a 'TreeViewColumn' API for packing cells, setting attributes and data funcs.

-- * Class Hierarchy
-- |
-- @
-- |  Interface CellLayout
-- |   +----'TreeViewColumn'
-- |   +----'CellView'
-- |   +----'IconView'
-- |   +----'EntryCompletion'
-- |   +----'ComboBox'
-- |   +----'ComboBoxEntry'
-- @

#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  CellLayoutClass,
  toCellLayout,

-- * Methods
  cellLayoutPackStart,
  cellLayoutPackEnd,
  cellLayoutReorder,
  cellLayoutClear,
  cellLayoutSetAttributes,
  cellLayoutClearAttributes
#endif
  ) where

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.GObject (mkFunPtrDestroyNotify)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.ModelView.Types#}
{#import Graphics.UI.Gtk.ModelView.TreeModel#}
{#import Graphics.UI.Gtk.TreeList.TreeIter#}
{#import Graphics.UI.Gtk.ModelView.CustomStore#} (treeModelGetRow)

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,4,0)

instance CellLayoutClass CellView
instance CellLayoutClass EntryCompletion
instance CellLayoutClass TreeViewColumn
instance CellLayoutClass ComboBox
instance CellLayoutClass IconView
instance CellLayoutClass ComboBoxEntry

--------------------
-- Methods

-- | Packs the @cell@ into the beginning of the cell layout. If @expand@ is
-- @False@, then the @cell@ is allocated no more space than it needs. Any
-- unused space is divided evenly between cells for which @expand@ is @True@.
--
-- Note that reusing the same cell renderer is not supported.
--
cellLayoutPackStart :: (CellLayoutClass self, CellRendererClass cell) => self
 -> cell  -- ^ @cell@ - A 'CellRenderer'.
 -> Bool  -- ^ @expand@ - @True@ if @cell@ is to be given extra space
          -- allocated to @cellLayout@.
 -> IO ()
cellLayoutPackStart self cell expand =
  {# call gtk_cell_layout_pack_start #}
    (toCellLayout self)
    (toCellRenderer cell)
    (fromBool expand)

-- | Adds the @cell@ to the end of @cellLayout@. If @expand@ is @False@, then
-- the @cell@ is allocated no more space than it needs. Any unused space is
-- divided evenly between cells for which @expand@ is @True@.
--
-- Note that reusing the same cell renderer is not supported.
--
cellLayoutPackEnd :: (CellLayoutClass self, CellRendererClass cell) => self
 -> cell  -- ^ @cell@ - A 'CellRenderer'.
 -> Bool  -- ^ @expand@ - @True@ if @cell@ is to be given extra space
          -- allocated to @cellLayout@.
 -> IO ()
cellLayoutPackEnd self cell expand =
  {# call gtk_cell_layout_pack_end #}
    (toCellLayout self)
    (toCellRenderer cell)
    (fromBool expand)

-- | Re-inserts @cell@ at @position@. Note that @cell@ has already to be
-- packed into @cellLayout@ for this to function properly.
--
cellLayoutReorder :: (CellLayoutClass self, CellRendererClass cell) => self
 -> cell  -- ^ @cell@ - A 'CellRenderer' to reorder.
 -> Int   -- ^ @position@ - New position to insert @cell@ at.
 -> IO ()
cellLayoutReorder self cell position =
  {# call gtk_cell_layout_reorder #}
    (toCellLayout self)
    (toCellRenderer cell)
    (fromIntegral position)

-- | Remove all renderers from the cell layout.
--
cellLayoutClear :: CellLayoutClass self => self -> IO ()
cellLayoutClear self =
  {# call gtk_cell_layout_clear #}
    (toCellLayout self)

-- | Insert a 'CellRenderer' @cell@ into the layout and specify how a
--   row of the @store@ defines the attributes of this renderer.
--
cellLayoutSetAttributes :: (CellLayoutClass self,
			     CellRendererClass cell,
			     TreeModelClass (model row),
			     TypedTreeModelClass model)
 => self
 -> cell   -- ^ @cell@ - A 'CellRenderer'.
 -> model row -- ^ @model@ - A model containing rows of type @row@.
 -> (row -> [AttrOp cell]) -- ^ Function to set attributes on the cell renderer.
 -> IO ()
cellLayoutSetAttributes self cell model attributes = do
  fPtr <- mkSetAttributeFunc $ \_ cellPtr' modelPtr' iterPtr _ -> do
    iter <- peek iterPtr
    let (TreeModel modelPtr) = toTreeModel model
    let (CellRenderer cellPtr) = toCellRenderer cell
    if unsafeForeignPtrToPtr modelPtr /= modelPtr' ||
       unsafeForeignPtrToPtr cellPtr  /= cellPtr' then
      error ("cellLayoutSetAttributes: attempt to set attributes of "++
	     "CellRenderer from different model.")
      else do
    row <- treeModelGetRow model iter
    set cell (attributes row)
  destroy <- mkFunPtrDestroyNotify fPtr
  {#call gtk_cell_layout_set_cell_data_func #} (toCellLayout self)
    (toCellRenderer cell) fPtr nullPtr destroy

{#pointer CellLayoutDataFunc#}

foreign import ccall "wrapper" mkSetAttributeFunc ::
  (Ptr CellLayout -> Ptr CellRenderer -> Ptr TreeModel -> Ptr TreeIter ->
   Ptr () -> IO ()) -> IO CellLayoutDataFunc

-- | Clears all existing attributes previously set with
-- 'cellLayoutSetAttributes'.
--
cellLayoutClearAttributes :: (CellLayoutClass self, CellRendererClass cell) => self
 -> cell  -- ^ @cell@ - A 'CellRenderer' to clear the attribute mapping on.
 -> IO ()
cellLayoutClearAttributes self cell =
  {# call gtk_cell_layout_clear_attributes #}
    (toCellLayout self)
    (toCellRenderer cell)

#endif
