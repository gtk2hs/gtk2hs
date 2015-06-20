{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) TreeModel
--
--  Author : Axel Simon
--
--  Created: 8 May 2001
--
--  Copyright (C) 1999-2007 Axel Simon
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
-- The tree interface used by 'TreeView'.
--
module Graphics.UI.Gtk.ModelView.TreeModel (
-- * Detail
--
-- | The 'TreeModel' interface defines a generic storage object for use by the
-- 'TreeView' and similar widgets. Specifically, the functions in defined here
-- are used by Gtk's widgets to access the stored data. Thus, rather than
-- calling these functions, an application programmer has to implement them.
-- While the module "Graphics.UI.Gtk.ModelView.CustomStore" provides the
-- necessary functions to implement the 'TreeMode' interface, it is often
-- sufficient to use the wo implementations that come with Gtk2Hs, namely are
-- 'ListStore' and 'TreeStore'.
--
-- The model is represented as a hierarchical tree of values. It is important
-- to note that this interface only provides a way of examining a model and
-- observing changes. The implementation of each individual model decides how
-- and if changes are made.
--
-- Two generic models are provided that implement the 'TreeModel' interface:
-- the 'TreeStore' and the 'ListStore'. To use these, the developer simply
-- inserts data into these models as necessary. These models provide the data
-- structure as well as the 'TreeModel' interface. In fact, they implement
-- other interfaces, making drag and drop and storing data trivial.
--
-- A 'TreeModel' stores records of the same type. Each record is referred to
-- as row, just like in a relational database. Defining how the information of
-- a row is displayed can be done in two ways: If the widget displays data
-- using 'Graphics.UI.Gtk.ModelView.CellRenderer.CellRenderer' or one of its
-- derivatives, it is possible to state how a row is mapped to the attributes
-- of a renderer using the
-- 'Graphics.UI.Gtk.ModelView.CellLayout.cellLayoutSetAttributes' function.
-- Some widgets do not use
-- 'Graphics.UI.Gtk.ModelView.CellRenderer.CellRenderer's to display their
-- data. In this case an extraction function can be defined that maps a row to
-- one of a few basic types (like 'String's or 'Int's). This extraction
-- function is associated with a 'ColumnId' using
-- 'Graphics.UI.Gtk.ModelView.CustomStore.treeModelSetColumn'. The latter can
-- be set in the widget for the property that should be set. The widget then
-- uses the function 'treeModelGetValue' and the 'ColumnId' to extract the
-- value from the model. As the name suggests, using 'ColumnId's creates a
-- view of the data as if each row were divided into a well-defined set of
-- columns, again, like a relational database.
--
-- Models are accessed on a node level of granularity. There are two index
-- types used to reference a particular node in a model. They are the
-- 'TreePath' and the 'TreeIter'. Most of the interface consists of operations
-- on a 'TreeIter'.
--
-- A path is essentially a potential node. It is a location on a model that
-- may or may not actually correspond to a node on a specific model. A
-- 'TreePath' is in fact a synonym for a list of 'Int's and hence are easy to
-- manipulate. Each number refers to the offset at that level. Thus, the path
-- @[0]@ refers to the root node and the path @[2,4]@ refers to the fifth
-- child of the third node.
--
-- By contrast, a 'TreeIter' is a reference to a specific node on a specific
-- model. It is an abstract data type filled in by the model. One can convert
-- a path to an iterator by calling 'treeModelGetIter'. These iterators are
-- the primary way of accessing a model and are similar to the iterators used
-- by 'TextBuffer'. The model interface defines a set of operations using them
-- for navigating the model. Iterators are expected to always be valid for as
-- long as the model is unchanged (and doesn't emit a signal).
--

-- * Class Hierarchy
-- |
-- @
-- |  GInterface
-- |   +----TreeModel
-- |   +--------TypedTreeModel
-- @

-- * Types
  TreeModel,
  TreeModelClass,
  castToTreeModel, gTypeTreeModel,
  toTreeModel,

  TypedTreeModel,
  TypedTreeModelClass,
  toTypedTreeModel,

  TreeIter(..),
  TreePath,

  ColumnId,

-- * Constructors
  makeColumnIdInt,
  makeColumnIdBool,
  makeColumnIdString,
  makeColumnIdPixbuf,
  invalidColumnId,

-- * Methods
  columnIdToNumber,
  stringToTreePath,
  treeModelGetFlags,
  treeModelGetIter,
  treeModelGetIterFromString,
  treeModelGetIterFirst,
  treeModelGetPath,
  treeModelGetValue,
  treeModelIterNext,
  treeModelIterChildren,
  treeModelIterHasChild,
  treeModelIterNChildren,
  treeModelIterNthChild,
  treeModelIterParent,
  treeModelForeach,
#if GTK_CHECK_VERSION(2,2,0)
  treeModelGetStringFromIter,
#endif
  treeModelRefNode,
  treeModelUnrefNode,
  treeModelRowChanged,
  treeModelRowInserted,
  treeModelRowHasChildToggled,
  treeModelRowDeleted,
  treeModelRowsReordered,

-- * Signals
  rowChanged,
  rowInserted,
  rowHasChildToggled,
  rowDeleted,
  rowsReordered,

  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Flags                (toFlags)
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.Types#}
{#import System.Glib.GValue#}           (GValue(GValue), allocaGValue)
{#import Graphics.UI.Gtk.ModelView.CustomStore#} (TreeModelFlags(..))
{#import Graphics.UI.Gtk.ModelView.Types#}  (TypedTreeModel,
                                             TypedTreeModelClass,
                                             toTypedTreeModel,
                                             TreeIter(..),
                                             receiveTreeIter,
                                             peekTreeIter,
                                             TreePath,
                                             NativeTreePath(..),
                                             withTreePath,
                                             fromTreePath,
                                             peekTreePath,
                                             stringToTreePath,
                                             ColumnId(..),
                                             ColumnAccess(..))
{#import System.Glib.GValueTypes#} ( valueGetInt, valueGetBool,
                                     valueGetString, valueGetGObject )
{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors


-- | Create a 'ColumnId' to extract an integer.
makeColumnIdInt :: Int -> ColumnId row Int
makeColumnIdInt = ColumnId valueGetInt CAInt

-- | Create a 'ColumnId' to extract an Boolean.
makeColumnIdBool :: Int -> ColumnId row Bool
makeColumnIdBool = ColumnId valueGetBool CABool

-- | Create a 'ColumnId' to extract an string.
makeColumnIdString :: GlibString string => Int -> ColumnId row string
makeColumnIdString = ColumnId valueGetString CAString

-- | Create a 'ColumnId' to extract an 'Pixbuf'.
makeColumnIdPixbuf :: Int -> ColumnId row Pixbuf
makeColumnIdPixbuf = ColumnId valueGetGObject CAPixbuf

-- | Convert a 'ColumnId' to a bare number.
columnIdToNumber :: ColumnId row ty -> Int
columnIdToNumber (ColumnId _ _ i) = i

-- | The invalid 'ColumnId'. Widgets use this value if no column id has
--   been set.
invalidColumnId :: ColumnId row ty
invalidColumnId = ColumnId (error "invalidColumnId: no GValue extractor")
  (error "invalidColumnId: no access type") (-1)

instance Eq (ColumnId row ty) where
  (ColumnId _ _ i1) == (ColumnId _ _ i2) = i1==i2

instance Show (ColumnId row ty) where
  show (ColumnId _ _ i) = show i


--------------------
-- Methods

-- %hash d:35ea
-- | Returns a set of flags supported by this interface.
--
-- The flags supported should not
-- change during the lifecycle of the tree_model.
--
treeModelGetFlags :: TreeModelClass self => self -> IO [TreeModelFlags]
treeModelGetFlags self =
  liftM (toFlags . fromIntegral) $
  {# call gtk_tree_model_get_flags #}
    (toTreeModel self)

-- %hash c:35a1 d:49a2
-- | Turn a 'String' into a 'TreeIter'.
--
-- * Returns @Nothing@ if the string is not a colon separated list of numbers
--   that references a valid node.
--
treeModelGetIterFromString :: (TreeModelClass self, GlibString string) => self
 -> string   -- ^ @pathString@ - A string representation of a 'TreePath'.
 -> IO (Maybe TreeIter)
treeModelGetIterFromString self pathString =
  receiveTreeIter $ \iterPtr ->
  withUTFString pathString $ \pathStringPtr ->
  {# call tree_model_get_iter_from_string #}
    (toTreeModel self)
    iterPtr
    pathStringPtr

-- %hash c:4cd2 d:ad96
-- | Turn a 'TreePath' into a 'TreeIter'.
--
-- Returns @Nothing@ if the given 'TreePath' was invalid. The empty list
-- is always invalid. The root node of a tree can be accessed by passing
-- @[0]@ as @path@.
--
treeModelGetIter :: TreeModelClass self => self
 -> TreePath -- ^ @path@ - The 'TreePath'.
 -> IO (Maybe TreeIter)
treeModelGetIter _  [] = return Nothing
treeModelGetIter self path =
  receiveTreeIter $ \iterPtr ->
  withTreePath path $ \path ->
  {# call tree_model_get_iter #}
    (toTreeModel self)
    iterPtr
    path

-- %hash c:103f d:8041
-- | Retrieves an 'TreeIter' to the first entry.
--
-- Returns @Nothing@ if the table is empty.
--
treeModelGetIterFirst :: TreeModelClass self => self
 -> IO (Maybe TreeIter)
treeModelGetIterFirst self =
  receiveTreeIter $ \iterPtr ->
  {# call tree_model_get_iter_first #}
    (toTreeModel self)
    iterPtr

-- %hash c:ec20 d:d43e
-- | Turn an abstract 'TreeIter' into a 'TreePath'.
--
-- In case the given 'TreeIter' was invalid, an empty list is returned.
--
treeModelGetPath :: TreeModelClass self => self
 -> TreeIter -> IO TreePath
treeModelGetPath self iter =
  with iter $ \iterPtr ->
  {# call tree_model_get_path #}
    (toTreeModel self)
    iterPtr
  >>= fromTreePath

-- | Read the value of at a specific column and 'TreeIter'.
--
treeModelGetValue :: TreeModelClass self => self
 -> TreeIter
 -> ColumnId row ty         -- ^ @column@ - The column to lookup the value at.
 -> IO ty
treeModelGetValue self iter (ColumnId getter _ colId) =
  allocaGValue $ \gVal ->
  with iter $ \iterPtr -> do
  {# call tree_model_get_value #}
    (toTreeModel self)
    iterPtr
    (fromIntegral colId)
    gVal
  getter gVal

-- %hash c:5c12 d:d7db
-- | Retrieve an iterator to the node following it at the current level. If
-- there is no next node, @Nothing@ is returned.
--
treeModelIterNext :: TreeModelClass self => self -> TreeIter -> IO (Maybe TreeIter)
treeModelIterNext self iter =
  receiveTreeIter $ \iterPtr -> do
  poke iterPtr iter
  {# call tree_model_iter_next #}
    (toTreeModel self)
    iterPtr

-- %hash c:7eba d:27e8
-- | Retrieve an iterator to the first child of @parent@. If @parent@ has no
-- children, @Nothing@.
--
treeModelIterChildren :: TreeModelClass self => self
 -> TreeIter -- ^ @parent@ - a pointer to the parent
 -> IO (Maybe TreeIter)
treeModelIterChildren self parent =
  receiveTreeIter $ \iterPtr ->
  with parent $ \parentPtr ->
  {# call tree_model_iter_children #}
    (toTreeModel self)
    iterPtr
    parentPtr

-- %hash c:dcc3
-- | Returns @True@ if @iter@ has children, @False@ otherwise.
--
treeModelIterHasChild :: TreeModelClass self => self
 -> TreeIter -- ^ @iter@ - The 'TreeIter' to test for children.
 -> IO Bool  -- ^ returns @True@ if @iter@ has children.
treeModelIterHasChild self iter =
  liftM toBool $
  with iter $ \iterPtr ->
  {# call tree_model_iter_has_child #}
    (toTreeModel self)
    iterPtr

-- %hash c:eed
-- | Returns the number of children that @iter@ has. As a special case, if
-- @iter@ is @Nothing@, then the number of toplevel nodes is returned.
--
treeModelIterNChildren :: TreeModelClass self => self
 -> Maybe TreeIter -- ^ @iter@ - The 'TreeIter', or @Nothing@.
 -> IO Int         -- ^ returns The number of children of @iter@.
treeModelIterNChildren self iter =
  liftM fromIntegral $
  maybeWith with iter $ \iterPtr ->
  {# call tree_model_iter_n_children #}
    (toTreeModel self)
    iterPtr

-- %hash c:6950 d:6f4d
-- | Retrieve the @n@th child of @parent@, counting from zero. If @n@ is too
-- big or @parent@ has no children, @Nothing@ is returned. If @Nothing@ is
-- specified for the @parent@ argument, the function will return the @n@th
-- root node.
--
treeModelIterNthChild :: TreeModelClass self => self
 -> Maybe TreeIter -- ^ @parent@ - The 'TreeIter' to get the child from, or
                   -- @Nothing@.
 -> Int            -- ^ @n@ - Then index of the desired child.
 -> IO (Maybe TreeIter)
treeModelIterNthChild self parent n =
  receiveTreeIter $ \iterPtr ->
  maybeWith with parent $ \parentPtr ->
  {# call tree_model_iter_nth_child #}
    (toTreeModel self)
    iterPtr
    parentPtr
    (fromIntegral n)

-- %hash c:8f01 d:70ff
-- | Retrieve the parent of this iterator.
--
treeModelIterParent :: TreeModelClass self => self
 -> TreeIter
 -> IO (Maybe TreeIter)
treeModelIterParent self child =
  receiveTreeIter $ \iterPtr ->
  with child $ \childPtr ->
  {# call tree_model_iter_parent #}
    (toTreeModel self)
    iterPtr
    childPtr

-- %hash c:154f d:a6d
-- | Maps a function over each node in model in a depth-first fashion. If it
-- returns @True@, then the tree ceases to be walked, and 'treeModelForeach'
-- returns.
--
treeModelForeach :: TreeModelClass self => self -> (TreeIter -> IO Bool) -> IO ()
treeModelForeach self fun = do
  fPtr <- mkTreeModelForeachFunc (\_ _ iterPtr _ -> do
    -- make a deep copy of the iterator. This makes it possible to store this
    -- iterator in Haskell land somewhere. The TreeModel parameter is not
    -- passed to the function due to performance reasons. But since it is
    -- a constant this does not matter.
    iter <- peek iterPtr
    liftM (fromIntegral.fromBool) $ fun iter
    )
  {# call tree_model_foreach #}
    (toTreeModel self)
    fPtr
    nullPtr
  freeHaskellFunPtr fPtr

{#pointer TreeModelForeachFunc#}

foreign import ccall "wrapper"  mkTreeModelForeachFunc ::
  (Ptr TreeModel -> Ptr NativeTreePath -> Ptr TreeIter -> Ptr () -> IO CInt) ->
  IO TreeModelForeachFunc

#if GTK_CHECK_VERSION(2,2,0)
-- %hash c:f04a d:94fd
-- | Generates a string representation of the iter. This string is a \':\'
-- separated list of numbers. For example, \"4:10:0:3\" would be an acceptable
-- return value for this string.
--
-- * Available since Gtk+ version 2.2
--
treeModelGetStringFromIter :: (TreeModelClass self, GlibString string) => self
 -> TreeIter  -- ^ @iter@ - An 'TreeIter'.
 -> IO string -- ^ the returned string representation
treeModelGetStringFromIter self iter = with iter $ \iter ->
  {# call gtk_tree_model_get_string_from_iter #}
    (toTreeModel self)
    iter
  >>= readUTFString
#endif

-- %hash c:228e d:304e
-- | Lets the tree ref the node. This is an optional method for models to
-- implement. To be more specific, models may ignore this call as it exists
-- primarily for performance reasons.
--
-- This function is primarily meant as a way for views to let caching model
-- know when nodes are being displayed (and hence, whether or not to cache that
-- node.) For example, a file-system based model would not want to keep the
-- entire file-hierarchy in memory, just the sections that are currently being
-- displayed by every current view.
--
-- A model should be expected to be able to get an iter independent of its
-- reffed state.
--
treeModelRefNode :: TreeModelClass self => self
 -> TreeIter -- ^ @iter@ - The 'TreeIter'.
 -> IO ()
treeModelRefNode self iter = with iter $ \iter ->
  {# call gtk_tree_model_ref_node #}
    (toTreeModel self)
    iter

-- %hash c:f5d7 d:22a6
-- | Lets the tree unref the node. This is an optional method for models to
-- implement. To be more specific, models may ignore this call as it exists
-- primarily for performance reasons.
--
-- For more information on what this means, see 'treeModelRefNode'. Please
-- note that nodes that are deleted are not unreffed.
--
treeModelUnrefNode :: TreeModelClass self => self
 -> TreeIter -- ^ @iter@ - The 'TreeIter'.
 -> IO ()
treeModelUnrefNode self iter = with iter $ \iter ->
  {# call gtk_tree_model_unref_node #}
    (toTreeModel self)
    iter

-- %hash c:8d25 d:a7c9
-- | Emits the 'rowChanged' signal on the model.
--
-- * This function is only necessary to implement a custom tree model. When
--   using 'Graphics.UI.Gtk.ModelView.ListStore' or
--   'Graphics.UI.Gtk.ModelView.TreeStore', this function is called
--   automatically.
--
treeModelRowChanged :: TreeModelClass self => self
 -> TreePath -- ^ @path@ - A 'TreePath' pointing to the changed row
 -> TreeIter -- ^ @iter@ - A valid 'TreeIter' pointing to the changed row
 -> IO ()
treeModelRowChanged self path iter =
  with iter $ \iter ->
  withTreePath path $ \path ->
  {# call gtk_tree_model_row_changed #}
    (toTreeModel self)
    path
    iter

-- %hash c:f809 d:57af
-- | Emits the 'rowInserted' signal on the model.
--
-- * This function is only necessary to implement a custom tree model. When
--   using 'Graphics.UI.Gtk.ModelView.ListStore' or
--   'Graphics.UI.Gtk.ModelView.TreeStore', this function is called
--   automatically.
--
treeModelRowInserted :: TreeModelClass self => self
 -> TreePath -- ^ @path@ - A 'TreePath' pointing to the inserted row
 -> TreeIter -- ^ @iter@ - A valid 'TreeIter' pointing to the inserted row
 -> IO ()
treeModelRowInserted self path iter =
  with iter $ \iter ->
  withTreePath path $ \path ->
  {# call gtk_tree_model_row_inserted #}
    (toTreeModel self)
    path
    iter

-- %hash c:e917 d:6534
-- | Emits the 'rowHasChildToggled' signal on the model. This should be
-- called by models after the child state of a node changes.
--
-- * This function is only necessary to implement a custom tree model. When
--   using 'Graphics.UI.Gtk.ModelView.ListStore' or
--   'Graphics.UI.Gtk.ModelView.TreeStore', this function is called
--   automatically.
--
treeModelRowHasChildToggled :: TreeModelClass self => self
 -> TreePath -- ^ @path@ - A 'TreePath' pointing to the changed row
 -> TreeIter -- ^ @iter@ - A valid 'TreeIter' pointing to the changed row
 -> IO ()
treeModelRowHasChildToggled self path iter =
  with iter $ \iter ->
  withTreePath path $ \path ->
  {# call gtk_tree_model_row_has_child_toggled #}
    (toTreeModel self)
    path
    iter

-- %hash c:c0a2 d:7ca6
-- | Emits the 'rowDeleted' signal on the model. This should be called by
-- models after a row has been removed. The location pointed to by @path@
-- should be the location that the row previously was at. It may not be a
-- valid location anymore.
--
-- * This function is only necessary to implement a custom tree model. When
--   using 'Graphics.UI.Gtk.ModelView.ListStore' or
--   'Graphics.UI.Gtk.ModelView.TreeStore', this function is called
--   automatically.
--
treeModelRowDeleted :: TreeModelClass self => self
 -> TreePath -- ^ @path@ - A 'TreePath' pointing to the previous location of
             -- the deleted row.
 -> IO ()
treeModelRowDeleted self path =
  withTreePath path $ \path ->
  {# call gtk_tree_model_row_deleted #}
    (toTreeModel self)
    path

-- %hash c:f0f3 d:a8c5
-- | Emits the 'rowsReordered' signal on the model. This should be called by
-- models when their rows have been reordered. The length  of @newOrder@ must
-- be equal to the value returned by @treeModelIterNChildren self iter@.
--
-- * This function is only necessary to implement a custom tree model. When
--   using 'Graphics.UI.Gtk.ModelView.ListStore' or
--   'Graphics.UI.Gtk.ModelView.TreeStore', this function is called
--   automatically.
--
treeModelRowsReordered :: TreeModelClass self => self
 -> TreePath -- ^ @path@ - A 'TreePath' pointing to the tree node whose
             -- children have been reordered
 -> Maybe TreeIter -- ^ @iter@ - A valid 'TreeIter' pointing to the node whose
                   -- children have been reordered, or @Nothing@ if
                   -- @path@ is @[]@.
 -> [Int]   -- ^ @newOrder@ - a list of integers giving the previous position
            -- of each node at this hierarchy level.

 -> IO ()
treeModelRowsReordered self path iter array = do
  n <- treeModelIterNChildren self iter
  let l = length array
  if n/=l then error ("treeModelRowsReordered: passed-in array is of size "
                      ++show l++" but there are "++show n++
                      " children at path "++show path) else
    withTreePath path $ \path ->
    maybeWith with iter $ \iter ->
    withArray (map fromIntegral array) $ \newOrderPtr ->
    {# call gtk_tree_model_rows_reordered #}
      (toTreeModel self)
      path
      iter
      newOrderPtr

--------------------
-- Signals

-- %hash c:50c7 d:8da5
-- | This signal is emitted when a row in the model has changed.
--
rowChanged :: TreeModelClass self => Signal self (TreePath -> TreeIter -> IO ())
rowChanged = Signal (connect_BOXED_BOXED__NONE "row-changed" peekTreePath peekTreeIter)

-- %hash c:f31a d:3c6b
-- | This signal is emitted when a new row has been inserted in the model.
--
--
rowInserted :: TreeModelClass self => Signal self (TreePath -> TreeIter -> IO ())
rowInserted = Signal (connect_BOXED_BOXED__NONE "row-inserted" peekTreePath peekTreeIter)

-- %hash c:7279 d:5ef
-- | This signal is emitted when a row has gotten the first child row or lost
-- its last child row.
--
rowHasChildToggled :: TreeModelClass self => Signal self (TreePath -> TreeIter -> IO ())
rowHasChildToggled = Signal (connect_BOXED_BOXED__NONE "row-has-child-toggled" peekTreePath peekTreeIter)

-- %hash c:f669 d:367f
-- | This signal is emitted when a row has been deleted.
--
-- Note that no iterator is passed to the signal handler, since the row is
-- already deleted.
--
-- Implementations of 'TreeModel' must emit row-deleted /before/ removing the
-- node from its internal data structures. This is because models and views
-- which access and monitor this model might have references on the node which
-- need to be released in the 'rowDeleted' handler.
--
rowDeleted :: TreeModelClass self => Signal self (TreePath -> IO ())
rowDeleted = Signal (connect_BOXED__NONE "row-deleted" peekTreePath)

-- %hash c:46dd d:b2e5
-- | This signal is emitted when the children of a node in the 'TreeModel'
-- have been reordered. See 'treeModelRowsReordered' for more information
-- about the parameters that this signal carries.
--
-- Note that this signal is /not/ emitted when rows are reordered by DND,
-- since this is implemented by removing and then reinserting the row.
--
rowsReordered :: TreeModelClass self =>
                 Signal self (TreePath -> Maybe TreeIter -> [Int] -> IO ())
rowsReordered = Signal $ \after model user ->
  connect_BOXED_BOXED_PTR__NONE "rows-reordered" peekTreePath
    (maybePeek peekTreeIter) after model $ \path iter arrayPtr -> do
      n <- treeModelIterNChildren model iter
      -- hopefully the model is never buggy, otherwise this can segfault
      newOrder <- peekArray n arrayPtr
      user path iter (map fromIntegral (newOrder :: [{#type gint#}]))
