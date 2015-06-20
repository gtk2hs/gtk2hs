{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget TreeModelFilter
--
--  Author : Axel Simon
--
--  Created: 14 January 2008
--
--  Copyright (C) 2008 Axel Simon
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
-- A 'TreeModel' which hides parts of an underlying tree model
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.ModelView.TreeModelFilter (

-- * Detail
--
-- | A 'TreeModelFilter' is a tree model which wraps another tree model, and
-- can do the following things:
--
-- * Filter specific rows, based on a function that examines each row
-- indicating whether the row should be shown or not, or
-- based on the return value of a visibility function, which is passed
-- the 'TreeIter' of the row and returns a Boolean indicating whether the row should
-- be shown or not.
--
-- * Set a different root node, also known as a \"virtual root\". You can
-- pass in a 'TreePath' indicating the root node for the filter at construction
-- time.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----TreeModelFilter
-- |   +----TypedTreeModelFilter
-- @

#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  TreeModelFilter,
  TypedTreeModelFilter,
  TreeModelFilterClass,
  castToTreeModelFilter, gTypeTreeModelFilter,
  toTreeModelFilter,

-- * Constructors
  treeModelFilterNew,

-- * Methods
  treeModelFilterSetVisibleFunc,
  treeModelFilterSetVisibleColumn,
  treeModelFilterGetModel,
  treeModelFilterConvertChildIterToIter,
  treeModelFilterConvertIterToChildIter,
  treeModelFilterConvertChildPathToPath,
  treeModelFilterConvertPathToChildPath,
  treeModelFilterRefilter,
  treeModelFilterClearCache,

-- * Attributes
  treeModelFilterChildModel,
  treeModelFilterVirtualRoot,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.ModelView.TreeModel#}
{#import Graphics.UI.Gtk.ModelView.Types#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,4,0)
--------------------
-- Interfaces

instance TreeModelClass (TypedTreeModelFilter a)
instance TreeModelFilterClass (TypedTreeModelFilter a)
instance GObjectClass (TypedTreeModelFilter a) where
  toGObject (TypedTreeModelFilter tm) = GObject (castForeignPtr tm)
  unsafeCastGObject = TypedTreeModelFilter . castForeignPtr . unGObject

--------------------
-- Constructors

-- %hash c:81e3 d:42cf
-- | Creates a new 'TreeModel', with @childModel@ as the child model and
-- @root@ as the virtual root.
--
treeModelFilterNew :: (TreeModelClass (childModel row),
                       TypedTreeModelClass childModel) =>
    childModel row -- ^ @childModel@ - A 'TreeModel'.
 -> TreePath   -- ^ @root@ - A 'TreePath' or @[]@.
 -> IO (TypedTreeModelFilter row)
treeModelFilterNew childModel [] =
  liftM unsafeTreeModelFilterToGeneric $
  wrapNewGObject mkTreeModelFilter $
  liftM (castPtr :: Ptr TreeModel -> Ptr TreeModelFilter) $
  {# call gtk_tree_model_filter_new #}
    (toTreeModel childModel)
    (NativeTreePath nullPtr)
treeModelFilterNew childModel root =
  liftM unsafeTreeModelFilterToGeneric $
  wrapNewGObject mkTreeModelFilter $
  liftM (castPtr :: Ptr TreeModel -> Ptr TreeModelFilter) $
  withTreePath root $ \root ->
  {# call gtk_tree_model_filter_new #}
    (toTreeModel childModel)
    root



--------------------
-- Methods

-- %hash c:2349 d:864a
-- | Sets the visible function used when filtering the rows to be @func@.
-- The function should return @True@ if the given row should be visible and
-- @False@ otherwise. The passed-in iterator is an iterator of the child
-- model, not of the 'TreeModelFilter' model that is passed in as the first
-- argument to this function.
--
-- If the condition calculated by the function changes over time (e.g.
-- because it depends on some global parameters), you must call
-- 'treeModelFilterRefilter' to keep the visibility information of the model
-- up to date.
--
treeModelFilterSetVisibleFunc :: TreeModelFilterClass self => self
 -> (TreeIter -> IO Bool)         -- ^ @func@ - The visible function
 -> IO ()
treeModelFilterSetVisibleFunc self func = do
  funcPtr <- mkTreeModelFilterVisibleFunc $ \_ tiPtr _ -> do
    ti <- peekTreeIter tiPtr
    liftM fromBool $ func ti
  {# call gtk_tree_model_filter_set_visible_func #}
    (toTreeModelFilter self) funcPtr (castFunPtrToPtr funcPtr) destroyFunPtr

{#pointer TreeModelFilterVisibleFunc #}

foreign import ccall "wrapper" mkTreeModelFilterVisibleFunc ::
  (Ptr TreeModel -> Ptr TreeIter -> Ptr () -> IO {#type gboolean#}) ->
  IO TreeModelFilterVisibleFunc

-- %hash c:a56d d:b42e
-- | Sets @column@ of the child model to be the column where the filter model
--   should look for visibility information. A row containing @True@ means
--   that this row should be shown.
--
treeModelFilterSetVisibleColumn ::
 (TreeModelFilterClass (self row),
  TypedTreeModelClass self)
 => self row
 -> ColumnId row Bool -- ^ @column@ - A column of Booleans that determines
                      -- if a row is visible
 -> IO ()
treeModelFilterSetVisibleColumn self col =
  {# call gtk_tree_model_filter_set_visible_column #}
    (toTreeModelFilter self)
    ((fromIntegral . columnIdToNumber) col)

-- %hash c:85fb d:a36
-- | Returns a pointer to the child model of @filter@.
--
treeModelFilterGetModel :: TreeModelFilterClass self => self
 -> IO (Maybe TreeModel) -- ^ returns a 'TreeModel'.
treeModelFilterGetModel self =
  maybeNull (makeNewGObject mkTreeModel) $
  {# call gtk_tree_model_filter_get_model #}
    (toTreeModelFilter self)

-- %hash c:1b93 d:5689
-- | Return an iterator in the sorted model that points to the row pointed to
-- by the given iter from the unfiltered model.
--
treeModelFilterConvertChildIterToIter :: TreeModelFilterClass self => self
 -> TreeIter
 -> IO TreeIter
treeModelFilterConvertChildIterToIter self childIter =
  with childIter $ \childIterPtr ->
  alloca $ \filterIterPtr -> do
  {# call tree_model_filter_convert_child_iter_to_iter #}
    (toTreeModelFilter self)
    filterIterPtr
    childIterPtr
  peek filterIterPtr

-- %hash c:c754 d:c058
-- | Return an iterator in the unfiltered model that points to the row pointed to
-- by the given iter from the filtered model.
--
treeModelFilterConvertIterToChildIter :: TreeModelFilterClass self => self
 -> TreeIter
 -> IO TreeIter
treeModelFilterConvertIterToChildIter self filteredIter =
  with filteredIter $ \filteredIterPtr ->
  alloca $ \childIterPtr -> do
  {# call tree_model_filter_convert_iter_to_child_iter #}
    (toTreeModelFilter self)
    childIterPtr
    filteredIterPtr
  peek childIterPtr

-- %hash c:e4e3 d:57be
-- | Converts the given path to a path relative to the given filtered model.
--
-- * The given path points to a row in the child model. The returned path will
-- point to the same row in the filtered model.
--
treeModelFilterConvertChildPathToPath :: TreeModelFilterClass self => self
 -> TreePath
 -> IO TreePath
treeModelFilterConvertChildPathToPath self [] = return []
treeModelFilterConvertChildPathToPath self childPath =
  withTreePath childPath $ \childPath ->
  {# call unsafe tree_model_filter_convert_child_path_to_path #}
    (toTreeModelFilter self)
    childPath
  >>= fromTreePath

-- %hash c:446d d:db70
-- | Converts path in the filtered model to a path on the unfiltered model on which
-- the given 'TreeModelFilter' is based. That is, the given path points to a
-- location in the given 'TreeModelFilter'. The returned path will point to the
-- same location in the underlying unfiltered model.
--
treeModelFilterConvertPathToChildPath :: TreeModelFilterClass self => self
 -> TreePath
 -> IO TreePath
treeModelFilterConvertPathToChildPath self [] = return []
treeModelFilterConvertPathToChildPath self filteredPath =
  withTreePath filteredPath $ \filteredPath ->
  {# call tree_model_filter_convert_path_to_child_path #}
    (toTreeModelFilter self)
    filteredPath
  >>= fromTreePath

-- %hash c:ed0b d:1a19
-- | Emits 'rowChanged' for each row in the child model, which causes the
-- filter to re-evaluate whether a row is visible or not.
--
treeModelFilterRefilter :: TreeModelFilterClass self => self -> IO ()
treeModelFilterRefilter self =
  {# call gtk_tree_model_filter_refilter #}
    (toTreeModelFilter self)

-- %hash c:ae64 d:a3b3
-- | This function should almost never be called. It clears the @filter@ of
-- any cached iterators that haven't been reffed with 'treeModelRefNode'. This
-- might be useful if the child model being filtered is static (and doesn't
-- change often) and there has been a lot of unreffed access to nodes. As a
-- side effect of this function, all unreffed iters will be invalid.
--
treeModelFilterClearCache :: TreeModelFilterClass self
 => self -- ^ @filter@ - the filter model
 -> IO ()
treeModelFilterClearCache self =
  {# call gtk_tree_model_filter_clear_cache #}
    (toTreeModelFilter self)

--------------------
-- Attributes

-- %hash c:8630 d:81a7
-- | The model for the filtermodel to filter.
--
treeModelFilterChildModel :: TreeModelFilterClass self => ReadAttr self TreeModel
treeModelFilterChildModel = readAttrFromObjectProperty "child-model"
                              {# call pure unsafe gtk_tree_model_get_type #}

-- %hash c:263d d:2dd5
-- | The virtual root (relative to the child model) for this filtermodel.
--
treeModelFilterVirtualRoot :: TreeModelFilterClass self => ReadAttr self TreePath
treeModelFilterVirtualRoot = readAttrFromBoxedOpaqueProperty (peekTreePath . castPtr)
                             "virtual-root"
                             {#call pure unsafe gtk_tree_path_get_type#}
#endif
