-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget TreeSelection
--
--  Author : Axel Simon
--
--  Created: 8 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/25 01:11:37 $
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
-- The selection object for 'TreeView'
--
module Graphics.UI.Gtk.TreeList.TreeSelection (
-- * Description
-- 
-- | The 'TreeSelection' object is a helper object to manage the selection for
-- a 'TreeView' widget. The 'TreeSelection' object is automatically created
-- when a new 'TreeView' widget is created, and cannot exist independentally of
-- this widget. The primary reason the 'TreeSelection' objects exists is for
-- cleanliness of code and API. That is, there is no conceptual reason all
-- these functions could not be methods on the 'TreeView' widget instead of a
-- separate function.
--
-- The 'TreeSelection' object is gotten from a 'TreeView' by calling
-- 'treeViewGetSelection'. It can be manipulated to check the selection status
-- of the tree, as well as select and deselect individual rows. Selection is
-- done completely on the 'TreeView' side.
-- As a result, multiple views of the same model can
-- have completely different selections. Additionally, you cannot change the
-- selection of a row on the model that is not currently displayed by the view
-- without expanding its parents first.
--
-- One of the important things to remember when monitoring the selection of
-- a view is that the \"changed\" signal is mostly a hint. That is, it may only
-- emit one signal when a range of rows is selected. Additionally, it may on
-- occasion emit a \"changed\" signal when nothing has happened (mostly as a
-- result of programmers calling select_row on an already selected row).

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----TreeSelection
-- @

-- * Types
  TreeSelection,
  TreeSelectionClass,
  castToTreeSelection,
  SelectionMode(..),
  TreeSelectionCB,
  TreeSelectionForeachCB,

-- * Methods
  treeSelectionSetMode,
  treeSelectionGetMode,
  treeSelectionSetSelectFunction,
  treeSelectionGetTreeView,
  treeSelectionGetSelected,
  treeSelectionSelectedForeach,
#if GTK_CHECK_VERSION(2,2,0)
  treeSelectionGetSelectedRows,
  treeSelectionCountSelectedRows,
#endif
  treeSelectionSelectPath,
  treeSelectionUnselectPath,
  treeSelectionPathIsSelected,
  treeSelectionSelectIter,
  treeSelectionUnselectIter,
  treeSelectionIterIsSelected,
  treeSelectionSelectAll,
  treeSelectionUnselectAll,
  treeSelectionSelectRange,
#if GTK_CHECK_VERSION(2,2,0)
  treeSelectionUnselectRange,
#endif

-- * Signals
  onSelectionChanged,
  afterSelectionChanged
  ) where

import Monad	(liftM)
import Data.IORef (newIORef, readIORef, writeIORef)

import System.Glib.FFI
import System.Glib.GList                (GList, fromGList, toGList)
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums    (SelectionMode(..))
{#import Graphics.UI.Gtk.TreeList.TreeModel#}
import Graphics.UI.Gtk.General.Structs	(treeIterSize)
import Graphics.UI.Gtk.General.General	(mkDestructor)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Set single or multiple choice.
--
treeSelectionSetMode :: (TreeSelectionClass ts) => ts -> SelectionMode -> IO ()
treeSelectionSetMode ts sm = {#call tree_selection_set_mode#}
  (toTreeSelection ts) ((fromIntegral.fromEnum) sm)

-- | Gets the selection mode.
--
treeSelectionGetMode :: (TreeSelectionClass ts) => ts -> IO SelectionMode
treeSelectionGetMode ts = liftM (toEnum.fromIntegral) $
  {#call unsafe tree_selection_get_mode#} (toTreeSelection ts)

-- | Set a callback function if selection changes.
--
-- * If set, this function is called before any
-- node is selected or unselected, giving some control over which nodes are
-- selected. The select function should return @True@ if the state of the node
-- may be toggled, and @False@ if the state of the node should be left
-- unchanged.
treeSelectionSetSelectFunction :: (TreeSelectionClass ts) => ts ->
                                  TreeSelectionCB -> IO ()
treeSelectionSetSelectFunction ts fun = do
  fPtr <- mkTreeSelectionFunc (\_ _ tp _ -> do
    path <- nativeTreePathGetIndices (NativeTreePath (castPtr tp))
    liftM fromBool $ fun path
    )
  dRef <- newIORef nullFunPtr
  dPtr <- mkDestructor $ do
    dPtr <- readIORef dRef
    freeHaskellFunPtr dPtr
    freeHaskellFunPtr fPtr
  writeIORef dRef dPtr
  {#call tree_selection_set_select_function#} (toTreeSelection ts) fPtr 
    nullPtr dPtr

-- | Callback type for a function that is called everytime the selection
-- changes. This function is set with 'treeSelectionSetSelectFunction'.
--
type TreeSelectionCB = TreePath -> IO Bool
{#pointer TreeSelectionFunc#}

foreign import ccall "wrapper"  mkTreeSelectionFunc ::
  (Ptr () -> Ptr () -> Ptr TreePath -> Ptr () -> IO CInt)->
  IO TreeSelectionFunc

-- | Retrieve the 'TreeView' widget that this 'TreeSelection' works on.
--
treeSelectionGetTreeView :: (TreeSelectionClass ts) => ts -> IO TreeView
treeSelectionGetTreeView ts = makeNewObject mkTreeView $
  {#call unsafe tree_selection_get_tree_view#} (toTreeSelection ts)

-- | Retrieves the selection of a single choice 'TreeSelection'.
--
treeSelectionGetSelected :: (TreeSelectionClass ts) => ts ->
                            IO (Maybe TreeIter)
treeSelectionGetSelected ts = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  res <- {#call tree_selection_get_selected#} (toTreeSelection ts) 
    (nullPtr) iter
  return $ if (toBool res) then Just iter else Nothing

-- | Execute a function for each selected node.
--
-- * Note that you cannot modify the tree or selection from within this
--   function. Hence, "treeSelectionGetSelectedRows" might be more useful.
--
treeSelectionSelectedForeach :: (TreeSelectionClass ts) => ts ->
                                TreeSelectionForeachCB -> IO ()
treeSelectionSelectedForeach ts fun = do
  fPtr <- mkTreeSelectionForeachFunc (\_ ti _ -> do
    -- make a deep copy of the iterator. This makes it possible to store this
    -- iterator in Haskell land somewhere. The TreeModel parameter is not
    -- passed to the function due to performance reasons. But since it is
    -- a constant member of Selection this does not matter.
    iterPtr <- mallocBytes treeIterSize
    copyBytes iterPtr ti treeIterSize
    iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
    fun iter
    )
  {#call tree_selection_selected_foreach#} (toTreeSelection ts) fPtr nullPtr
  freeHaskellFunPtr fPtr

-- | Callback function type for 'treeSelectionSelectedForeach'.
--
type TreeSelectionForeachCB = TreeIter -> IO ()
{#pointer TreeSelectionForeachFunc#}

foreign import ccall "wrapper"  mkTreeSelectionForeachFunc ::
  (Ptr () -> Ptr TreeIter -> Ptr () -> IO ()) -> IO TreeSelectionForeachFunc

#if GTK_CHECK_VERSION(2,2,0)
-- | Creates a list of paths of all selected rows. 
--
-- *  Additionally, if you are
-- planning on modifying the model after calling this function, you may want to
-- convert the returned list into a list of "TreeRowReference"s. To do this,
-- you can use "treeRowReferenceNew".
--
-- * Available since Gtk version 2.2
-- 
treeSelectionGetSelectedRows :: TreeSelectionClass self => self
 -> IO [TreePath] -- ^ returns a list containing a "TreePath" for
		  -- each selected row.
treeSelectionGetSelectedRows self =
  {# call gtk_tree_selection_get_selected_rows #} 
  (toTreeSelection self) nullPtr
  >>= fromGList >>= mapM fromTreePath

-- | Returns the number of rows that are selected.
--
-- * Available since Gtk version 2.2
-- 
treeSelectionCountSelectedRows :: TreeSelectionClass self => self
 -> IO Int -- ^ returns The number of rows selected.
treeSelectionCountSelectedRows self =
  liftM fromIntegral $
  {# call gtk_tree_selection_count_selected_rows #}
     (toTreeSelection self)
#endif



-- | Select a specific item by 'TreePath'.
--
treeSelectionSelectPath :: (TreeSelectionClass ts) => ts -> TreePath -> IO ()
treeSelectionSelectPath ts [] = return ()
treeSelectionSelectPath ts tp = do
  nativePath <- nativeTreePathNew
  mapM_ ({#call unsafe tree_path_append_index#} nativePath . fromIntegral) tp
  {#call tree_selection_select_path#} (toTreeSelection ts) nativePath
  nativeTreePathFree nativePath

-- | Deselect a specific item by 'TreePath'.
--
treeSelectionUnselectPath :: (TreeSelectionClass ts) => ts -> TreePath -> IO ()
treeSelectionUnselectPath ts tp = do
  nativePath <- nativeTreePathNew
  mapM_ ({#call unsafe tree_path_append_index#} nativePath . fromIntegral) tp
  {#call tree_selection_unselect_path#} (toTreeSelection ts) nativePath
  nativeTreePathFree nativePath

-- | Returns True if the row at the given path is currently selected.
--
treeSelectionPathIsSelected :: (TreeSelectionClass ts) => ts -> TreePath -> IO Bool
treeSelectionPathIsSelected ts tp = do
  nativePath <- nativeTreePathNew
  mapM_ ({#call unsafe tree_path_append_index#} nativePath . fromIntegral) tp
  res <- {#call unsafe tree_selection_path_is_selected#} (toTreeSelection ts)
    nativePath
  nativeTreePathFree nativePath
  return (toBool res)


-- | Select a specific item by 'TreeIter'.
--
treeSelectionSelectIter :: (TreeSelectionClass ts) => ts -> TreeIter -> IO ()
treeSelectionSelectIter ts ti =
  {#call tree_selection_select_iter#} (toTreeSelection ts) ti

-- | Deselect a specific item by 'TreeIter'.
--
treeSelectionUnselectIter :: (TreeSelectionClass ts) => ts -> TreeIter -> IO ()
treeSelectionUnselectIter ts ti =
  {#call tree_selection_unselect_iter#} (toTreeSelection ts) ti

-- | Returns True if the row at the given iter is currently selected.
--
treeSelectionIterIsSelected :: (TreeSelectionClass ts) => ts -> TreeIter ->
			       IO Bool
treeSelectionIterIsSelected ts ti = liftM toBool $
  {#call unsafe tree_selection_iter_is_selected#} (toTreeSelection ts) ti

-- | Select everything.
--
treeSelectionSelectAll :: (TreeSelectionClass ts) => ts -> IO ()
treeSelectionSelectAll ts = 
  {#call tree_selection_select_all#} (toTreeSelection ts)

-- | Deselect everything.
--
treeSelectionUnselectAll :: (TreeSelectionClass ts) => ts -> IO ()
treeSelectionUnselectAll ts = 
  {#call tree_selection_unselect_all#} (toTreeSelection ts)


-- | Select a range specified by two 'TreePath's.
--
treeSelectionSelectRange :: (TreeSelectionClass ts) => ts -> TreePath ->
                            TreePath -> IO ()
treeSelectionSelectRange ts start end = do
  nP1 <- nativeTreePathNew
  mapM_ ({#call unsafe tree_path_append_index#} nP1 . fromIntegral) start
  nP2 <- nativeTreePathNew
  mapM_ ({#call unsafe tree_path_append_index#} nP2 . fromIntegral) end
  {#call tree_selection_select_range#} (toTreeSelection ts) nP1 nP2
  nativeTreePathFree nP1
  nativeTreePathFree nP2

#if GTK_CHECK_VERSION(2,2,0)
-- | Unselects a range of nodes.
--
-- * Available since Gtk version 2.2
-- 
treeSelectionUnselectRange :: TreeSelectionClass self => self
 -> TreePath
 -> TreePath
 -> IO ()
treeSelectionUnselectRange ts start end = do
  nP1 <- nativeTreePathNew
  mapM_ ({#call unsafe tree_path_append_index#} nP1 . fromIntegral) start
  nP2 <- nativeTreePathNew
  mapM_ ({#call unsafe tree_path_append_index#} nP2 . fromIntegral) end
  {#call tree_selection_unselect_range#} (toTreeSelection ts) nP1 nP2
  nativeTreePathFree nP1
  nativeTreePathFree nP2
#endif

--------------------
-- Signals

-- | Emitted each time the user changes the selection.
--
onSelectionChanged, afterSelectionChanged :: TreeSelectionClass ts => ts -> 
					     IO () -> IO (ConnectId ts)
onSelectionChanged = connect_NONE__NONE "changed" False
afterSelectionChanged = connect_NONE__NONE "changed" True

