-- -*-haskell-*-
--  GIMP Toolkit (GTK) TreeModel
--
--  Author : Axel Simon
--
--  Created: 8 May 2001
--
--  Version $Revision: 1.6 $ from $Date: 2005/02/25 22:53:42 $
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
module Graphics.UI.Gtk.TreeList.TreeModel (
-- * Description
-- 
-- | The 'TreeModel' interface defines a generic storage object for use by the
-- 'TreeView' widget. It is purely abstract, concrete implementations that
-- store data for a list or tree widget are e.g. 'ListStore' and 'TreeStore'.
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
  TreeModelFlags(..),
  TreePath,
  NativeTreePath(NativeTreePath),	-- internal
  TreeRowReference(..),
  TreeIter(..),

-- * Methods
  treeModelGetFlags,
  treeModelGetNColumns,
  treeModelGetColumnType,
  treeModelGetValue,
  nativeTreePathFree,			-- internal
  nativeTreePathNew,			-- internal
  withTreePath,				-- internal
  nativeTreePathGetIndices,		-- internal
  fromTreePath,				-- internal
  treeRowReferenceNew,
  treeRowReferenceGetPath,
  treeRowReferenceValid,
  createTreeIter,			-- internal
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
  treeModelIterParent
  ) where

import Monad	(liftM, when)
import Maybe	(fromMaybe)
import List   (intersperse)

import System.Glib.FFI
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Structs	(treeIterSize)
import Graphics.UI.Gtk.Gdk.Enums	(Flags(..))
import System.Glib.StoreValue		(TMType)
{#import System.Glib.GValue#}		(GValue, GenericValue, valueUnset)

{# context lib="gtk" prefix="gtk" #}

-- | Tree Iterator : A pointer to an entry in a 'TreeStore' or 'ListStore'.
--
{#pointer * TreeIter foreign newtype#}

-- | TreePath : a list of indices to specify a subtree or node in the
-- hierarchical 'TreeStore' database.
--
type TreePath = [Int]

{#pointer * TreePath as NativeTreePath newtype#}

-- | Tree Row Reference : like a 'TreePath' it points to a subtree or node, but
-- it is persistent. It identifies the same node (so long as it exists) even
-- when items are added, removed, or reordered.
--
{#pointer * TreeRowReference foreign newtype#}

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
-- * The returned flags do not
-- change during the lifecycle of the tree_model.
-- 
treeModelGetFlags :: TreeModelClass self => self -> IO [TreeModelFlags]
treeModelGetFlags self =
  liftM (toFlags . fromIntegral) $
  {# call unsafe gtk_tree_model_get_flags #}
     (toTreeModel self)

-- | Returns the number of columns supported by the "TreeModel".
-- 
treeModelGetNColumns :: TreeModelClass self => self
 -> IO Int -- ^ returns The number of columns.
treeModelGetNColumns self =
  liftM fromIntegral $
  {# call gtk_tree_model_get_n_columns #}
     (toTreeModel self)

-- | Retrieves the type of a specific column.
--
treeModelGetColumnType :: TreeModelClass tm => tm -> Int -> IO TMType
treeModelGetColumnType tm col = liftM (toEnum.fromIntegral) $
  {#call unsafe tree_model_get_column_type#} (toTreeModel tm) 
  (fromIntegral col)

-- | Read the value of at a specific column and 'Iterator'.
--
treeModelGetValue :: TreeModelClass tm => tm -> TreeIter -> Int ->
                     IO GenericValue
treeModelGetValue tm iter col = alloca $ \vaPtr -> do
  -- don't know if this is necessary, see treeList/StoreValue.hsc
  poke (castPtr vaPtr) (0:: {#type GType#})
  {#call unsafe tree_model_get_value#} (toTreeModel tm) iter 
    (fromIntegral col) vaPtr
  val <- peek vaPtr
  valueUnset vaPtr
  return val

-- | Maps a function over each node in model in a depth-first fashion. If the
-- function returns True, the tree walk stops.
--
treeModelForeach :: TreeModelClass tm => tm -> (TreeIter -> IO Bool) -> IO ()
treeModelForeach tm fun = do
  fPtr <- mkTreeModelForeachFunc (\_ _ ti _ -> do
    -- make a deep copy of the iterator. This makes it possible to store this
    -- iterator in Haskell land somewhere. The TreeModel parameter is not
    -- passed to the function due to performance reasons. But since it is
    -- a constant this does not matter.
    iterPtr <- mallocBytes treeIterSize
    copyBytes iterPtr ti treeIterSize
    iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
    liftM (fromIntegral.fromBool) $ fun iter
    )
  {#call tree_model_foreach#} (toTreeModel tm) fPtr nullPtr
  freeHaskellFunPtr fPtr

{#pointer TreeModelForeachFunc#}

foreign import ccall "wrapper"  mkTreeModelForeachFunc ::
  (Ptr () -> Ptr () -> Ptr TreeIter -> Ptr () -> IO CInt) -> IO TreeModelForeachFunc

-- utilities related to tree models

nativeTreePathFree :: NativeTreePath -> IO ()
nativeTreePathFree = {#call unsafe tree_path_free#}

nativeTreePathNew :: IO NativeTreePath
nativeTreePathNew = liftM NativeTreePath {#call unsafe tree_path_new#} 

withTreePath :: TreePath -> (NativeTreePath -> IO a) -> IO a
withTreePath tp act = do
  nativePath <- nativeTreePathNew
  mapM_ ({#call unsafe tree_path_append_index#} nativePath . fromIntegral) tp
  res <- act nativePath
  nativeTreePathFree nativePath
  return res

nativeTreePathGetIndices :: NativeTreePath -> IO [Int]
nativeTreePathGetIndices tp = do
  depth <- liftM fromIntegral $ {#call unsafe tree_path_get_depth#} tp
  arrayPtr <- {#call unsafe tree_path_get_indices#} tp
  if (depth==0 || arrayPtr==nullPtr) then return [] else
    sequence [liftM fromIntegral $ peekElemOff arrayPtr e 
	     | e <- [0..depth-1]]

fromTreePath :: Ptr NativeTreePath -> IO TreePath
fromTreePath tpPtr | tpPtr==nullPtr = return []
		   | otherwise = do
  path <- nativeTreePathGetIndices (NativeTreePath tpPtr)
  nativeTreePathFree (NativeTreePath tpPtr)
  return path

-- | Creates a row reference based on a path. This reference will keep pointing
-- to the node pointed to by the given path, so long as it exists.
--
treeRowReferenceNew :: TreeModelClass tm => tm -> NativeTreePath ->
		       IO TreeRowReference
treeRowReferenceNew tm path = do
  rowRefPtr <- throwIfNull "treeRowReferenceNew: invalid path given" $
    {#call unsafe gtk_tree_row_reference_new#} (toTreeModel tm) path
  liftM TreeRowReference $
    newForeignPtr rowRefPtr (tree_row_reference_free rowRefPtr)

#if __GLASGOW_HASKELL__>=600

foreign import ccall unsafe "&gtk_tree_row_reference_free"
  tree_row_reference_free' :: FinalizerPtr TreeRowReference

tree_row_reference_free :: Ptr TreeRowReference -> FinalizerPtr TreeRowReference
tree_row_reference_free _ = tree_row_reference_free'

#else

foreign import ccall unsafe "gtk_tree_row_reference_free"
  tree_row_reference_free :: Ptr TreeRowReference -> IO ()

#endif

-- | Returns a path that the row reference currently points to.
--
-- * The returned path may be the empty list if the reference was invalid.
--
treeRowReferenceGetPath :: TreeRowReference -> IO TreePath
treeRowReferenceGetPath ref = do
  tpPtr <- {#call unsafe tree_row_reference_get_path#} ref
  if tpPtr==nullPtr then return [] else do
  path <- nativeTreePathGetIndices (NativeTreePath tpPtr)
  nativeTreePathFree (NativeTreePath tpPtr)
  return path

-- | Returns True if the reference refers to a current valid path.
--
treeRowReferenceValid :: TreeRowReference -> IO Bool
treeRowReferenceValid ref = liftM toBool $
  {#call unsafe tree_row_reference_valid#} ref

createTreeIter :: Ptr TreeIter -> IO TreeIter
createTreeIter tiPtr = do
  tiPtr' <- tree_iter_copy tiPtr
  liftM TreeIter $ newForeignPtr tiPtr' (tree_iter_free tiPtr')


#if __GLASGOW_HASKELL__>=504

foreign import ccall unsafe "gtk_tree_iter_copy"
  tree_iter_copy :: Ptr TreeIter -> IO (Ptr TreeIter)

#else

foreign import ccall "gtk_tree_iter_copy" unsafe
  tree_iter_copy :: Ptr TreeIter -> IO (Ptr TreeIter)

#endif

#if __GLASGOW_HASKELL__>=600

foreign import ccall unsafe "&gtk_tree_iter_free"
  tree_iter_free' :: FinalizerPtr TreeIter

tree_iter_free :: Ptr TreeIter -> FinalizerPtr TreeIter
tree_iter_free _ = tree_iter_free'

#elif __GLASGOW_HASKELL__>=504

foreign import ccall unsafe "gtk_tree_iter_free"
  tree_iter_free :: Ptr TreeIter -> IO ()

#else

foreign import ccall "gtk_tree_iter_free" unsafe
  tree_iter_free :: Ptr TreeIter -> IO ()

#endif

-- | Turn a @String@ into a 'TreeIter'.
--
-- * Returns @Nothing@ if the string is not a colon separated list of numbers
--   that references a valid node.
--
treeModelGetIterFromString :: TreeModelClass tm => tm -> String ->
                                                   IO (Maybe TreeIter)
treeModelGetIterFromString tm str = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  res <- withUTFString str $ \strPtr ->
    {#call unsafe tree_model_get_iter_from_string#} (toTreeModel tm) iter
      strPtr
  return $ if (toBool res) then Just iter else Nothing

-- | Turn a 'TreePath' into a 'TreeIter'.
--
-- * Returns "Nothing" if the given "TreePath" was invalid. The empty list
--   is always invalid. The root node of a tree can be accessed by passing
--   @[0]@ as "TreePath".
--
treeModelGetIter :: TreeModelClass tm => tm -> TreePath -> IO (Maybe TreeIter)
treeModelGetIter _  [] = return Nothing
treeModelGetIter tm tp = withTreePath tp $ \nativePath -> do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  res <- {#call unsafe tree_model_get_iter#} (toTreeModel tm) iter nativePath
  return $ if (toBool res) then Just iter else Nothing
  
-- | Retrieves an 'TreeIter' to the
-- first entry.
--
-- * Returns @Nothing@ if the table is empty.
--
treeModelGetIterFirst :: TreeModelClass tm => tm -> IO (Maybe TreeIter)
treeModelGetIterFirst tm = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  res <- {#call unsafe tree_model_get_iter_first#} (toTreeModel tm) iter
  return $ if (toBool res) then Just iter else Nothing

-- | Turn an abstract "TreeIter" into a "TreePath".
--
-- * In case the given "TreeIter" was invalid, an empty list is returned.
--
treeModelGetPath :: TreeModelClass tm => tm -> TreeIter -> IO TreePath
treeModelGetPath tm iter =  do
  tpPtr <- {#call unsafe tree_model_get_path#} (toTreeModel tm) iter
  if tpPtr==nullPtr then return [] else do
  path <- nativeTreePathGetIndices (NativeTreePath tpPtr)
  nativeTreePathFree (NativeTreePath tpPtr)
  return path

-- | Advance the iterator to the next element.
--
-- * If there is no other element on this hierarchy level, return 
--   @False@.
--
treeModelIterNext :: TreeModelClass tm => tm -> TreeIter -> IO Bool
treeModelIterNext tm iter = liftM toBool $
 {#call unsafe tree_model_iter_next#} (toTreeModel tm) iter

-- | Retrieve an iterator to the first child.
--
treeModelIterChildren :: TreeModelClass tm => tm -> TreeIter -> 
					      IO (Maybe TreeIter)
treeModelIterChildren tm parent = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  res <- {#call unsafe tree_model_iter_children#} (toTreeModel tm) iter 
    parent
  return $ if (toBool res) then Just iter else Nothing

-- | Test if this is the last hierarchy level.
treeModelIterHasChild :: TreeModelClass tm => tm -> TreeIter -> IO Bool
treeModelIterHasChild tm iter = liftM toBool $
  {#call unsafe tree_model_iter_has_child#} (toTreeModel tm) iter

-- | Return the number of children.
--
-- * If @Nothing@ is specified for the @tm@ argument, the
--   function will work on toplevel elements.
--
treeModelIterNChildren :: TreeModelClass tm => tm -> Maybe TreeIter -> IO Int
treeModelIterNChildren tm iter = liftM fromIntegral $
  {#call unsafe tree_model_iter_n_children#} (toTreeModel tm) 
    (fromMaybe (TreeIter nullForeignPtr) iter)

-- | Retrieve the @n@th child.
--
-- * If @Nothing@ is specified for the @tm@ argument, the
--   function will work on toplevel elements.
--
treeModelIterNthChild :: TreeModelClass tm => 
  tm -> Maybe TreeIter -> Int -> IO (Maybe TreeIter)
treeModelIterNthChild tm parent n = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  res <- {#call unsafe tree_model_iter_nth_child#} (toTreeModel tm) iter 
    (fromMaybe (TreeIter nullForeignPtr) parent) (fromIntegral n)
  return $ if (toBool res) then Just iter else Nothing

-- | Retrieve the parent of this iterator.
--
treeModelIterParent :: TreeModelClass tm => tm -> 
  TreeIter -> IO (Maybe TreeIter)
treeModelIterParent tm child = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  res <- {#call unsafe tree_model_iter_parent#} (toTreeModel tm) iter child
  return $ if (toBool res) then Just iter else Nothing

