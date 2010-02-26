{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) TreeModel
--
--  Author : Axel Simon
--
--  Created: 8 May 2001
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
-- The tree interface used by 'TreeView'
--
-- * This module and all other modules in 'Graphics.UI.Gtk.TreeList' are
--   deprecated. Please use the modules in 'Graphics.UI.Gtk.ModelView'.
--
module Graphics.UI.Gtk.TreeList.TreeModel (
-- * Detail
-- 
-- | The 'TreeModel' interface defines a generic storage object for use by the
-- 'TreeView' widget. It is purely abstract, concrete implementations that
-- store data for a list or tree are 'ListStore' and 'TreeStore'.
--
-- The model is represented as a hierarchical tree of strongly-typed,
-- columned data. In other words, the model can be seen as a tree where every
-- node has different values depending on which column is being queried. The
-- type of data found in a column can be arbitrary, ranging from basic
-- types like 'String's or 'Int' to user specific types. The types are
-- homogeneous per column across all nodes. It is important to note that this
-- interface only provides a way of examining a model and observing changes.
-- The implementation of each individual model decides how and if changes are
-- made.
--
-- Two generic models are provided that implement the 'TreeModel' interface:
-- the
-- 'TreeStore' and the 'ListStore'. To use these, the developer simply pushes
-- data into these models as necessary. These models provide the data 
-- structure as well as the 'TreeModel' interface. In fact, they implement
-- other interfaces making drag
-- and drop, sorting, and storing data trivial.
--
-- Models are accessed on a node\/column level of granularity. One can query
-- for the value of a model at a certain node and a certain column on that
-- node. There are two structures used to reference a particular node in a
-- model. They are the 'TreePath' and the 'TreeIter' Most of the interface
-- consists of operations on a 'TreeIter'.
--
-- A path is essentially a potential node. It is a location on a model that
-- may or may not actually correspond to a node on a specific model. A
-- 'TreePath' is in fact just a list of 'Int's and hence are easy to
-- manipulate. Each number refers to the offset at that level. Thus, the 
-- path @[0]@ refers to the
-- root node and the path @[2,4]@ refers to the fifth child of the third node.
--
-- By contrast, a 'TreeIter' is a reference to a specific node on a specific
-- model. It is an abstract data type filled in by the model. One can convert a
-- path to an iterator by calling 'treeModelGetIter'. These iterators are the
-- primary way of accessing a model and are similar to the iterators used by
-- 'TextBuffer'. The model interface defines a set of operations using
-- them for navigating the model.
--
-- The lifecycle of an iterator can be a little confusing at first.
-- Iterators are expected to always be valid for as long as the model is
-- unchanged (and doesn't emit a signal). 
-- Additionally, the 'TreeStore' and 'ListStore' models guarantee that 
-- an iterator is valid for as long as the node it refers to is valid.
-- Although generally uninteresting, as one
-- always has to allow for the case where iterators do not persist beyond a
-- signal, some very important performance enhancements were made in the sort
-- model. As a result, the 'TreeModelItersPersist' flag was added to indicate
-- this behavior.
--

-- * Class Hierarchy
-- |
-- @
-- |  GInterface
-- |   +----TreeModel
-- @

-- * Types
  TreeModel,
  TreeModelClass,
  castToTreeModel,
  toTreeModel,
  TreeModelFlags(..),
  TreePath,
  TreeIter,

-- * Methods
  treeModelGetFlags,
  treeModelGetNColumns,
  treeModelGetColumnType,
  treeModelGetValue,
  treeModelGetIter,
  treeModelGetIterFromString,
  gtk_tree_model_get_iter_from_string,	-- internal
  treeModelGetIterFirst,
  treeModelGetPath,
  treeModelIterNext,
  treeModelIterChildren,
  treeModelIterHasChild,
  treeModelIterNChildren,
  treeModelIterNthChild,
  treeModelIterParent,
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.Flags		(Flags, toFlags)
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Types#}
import System.Glib.StoreValue		(TMType, GenericValue,
					 valueGetGenericValue)
{#import System.Glib.GValue#}		(GValue(GValue), allocaGValue)
{#import Graphics.UI.Gtk.TreeList.TreeIter#}
{#import Graphics.UI.Gtk.TreeList.TreePath#}

{# context lib="gtk" prefix="gtk" #}

-- | These flags indicate various properties of a 'TreeModel'.
--
-- * If a model has "TreeModelItersPersist" set, iterators remain valid
--   after a "TreeModel" signal was emitted.
--
-- * The "TreeModelListOnly" flag is set if the rows are arranged in a
--   simple flat list. This is set in the "ListStore" implementation.
--
{#enum TreeModelFlags {underscoreToCase} deriving(Bounded)#}

instance Flags TreeModelFlags

--------------------
-- Methods

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

-- | Returns the number of columns supported by the tree model.
--
treeModelGetNColumns :: TreeModelClass self => self
 -> IO Int -- ^ returns The number of columns.
treeModelGetNColumns self =
  liftM fromIntegral $
  {# call gtk_tree_model_get_n_columns #}
    (toTreeModel self)

-- | Returns the type of the column.
--
treeModelGetColumnType :: TreeModelClass self => self
 -> Int          -- ^ @index@ - The column index.
 -> IO TMType
treeModelGetColumnType self index =
  liftM (toEnum.fromIntegral) $
  {# call tree_model_get_column_type #}
    (toTreeModel self)
    (fromIntegral index)

-- | Read the value of at a specific column and 'TreeIter'.
--
treeModelGetValue :: TreeModelClass self => self
 -> TreeIter
 -> Int         -- ^ @column@ - The column to lookup the value at.
 -> IO GenericValue
treeModelGetValue self iter column =
  allocaGValue $ \vaPtr ->
  with iter $ \iterPtr -> do
  {# call tree_model_get_value #}
    (toTreeModel self)
    iterPtr
    (fromIntegral column)
    vaPtr
  valueGetGenericValue vaPtr

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
  (Ptr () -> Ptr () -> Ptr TreeIter -> Ptr () -> IO CInt) -> IO TreeModelForeachFunc

-- | Turn a 'String' into a 'TreeIter'.
--
-- * Returns @Nothing@ if the string is not a colon separated list of numbers
--   that references a valid node.
--
treeModelGetIterFromString :: TreeModelClass self => self
 -> String   -- ^ @pathString@ - A string representation of a 'TreePath'.
 -> IO (Maybe TreeIter)
treeModelGetIterFromString self pathString =
  receiveTreeIter $ \iterPtr ->
  withUTFString pathString $ \pathStringPtr ->
  {# call tree_model_get_iter_from_string #}
    (toTreeModel self)
    iterPtr
    pathStringPtr

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

-- | Retrieve an iterator to the next child.
--
treeModelIterNext :: TreeModelClass self => self -> TreeIter -> IO (Maybe TreeIter)
treeModelIterNext self iter =
  receiveTreeIter $ \iterPtr -> do
  poke iterPtr iter
  {# call tree_model_iter_next #}
    (toTreeModel self)
    iterPtr

-- | Retrieve an iterator to the first child.
--
treeModelIterChildren :: TreeModelClass self => self
 -> TreeIter
 -> IO (Maybe TreeIter)
treeModelIterChildren self parent =
  receiveTreeIter $ \iterPtr ->
  with parent $ \parentPtr ->
  {# call tree_model_iter_children #}
    (toTreeModel self)
    iterPtr
    parentPtr

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

-- | Retrieve the @n@th child.
--
-- If @Nothing@ is specified for the @self@ argument, the function will work
-- on toplevel elements.
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

