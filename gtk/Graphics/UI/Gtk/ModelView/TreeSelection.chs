{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget TreeSelection
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
-- The selection object for 'TreeView'
--
module Graphics.UI.Gtk.ModelView.TreeSelection (
-- * Detail
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
-- 'treeViewGetSelection'. It can be
-- manipulated to check the selection status of the tree, as well as select
-- and deselect individual rows. Selection is done completely on the
-- 'TreeView' side. As a result, multiple views of the same model can
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
  castToTreeSelection, gTypeTreeSelection,
  toTreeSelection,
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

-- * Attributes
  treeSelectionMode,

-- * Signals
  treeSelectionSelectionChanged,

#ifndef DISABLE_DEPRECATED
-- * Deprecated
  onSelectionChanged,
  afterSelectionChanged
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.GList                (fromGList)
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums    (SelectionMode(..))
{#import Graphics.UI.Gtk.ModelView.TreeModel#}
{#import Graphics.UI.Gtk.ModelView.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Set single or multiple choice.
--
treeSelectionSetMode :: TreeSelectionClass self => self
 -> SelectionMode
 -> IO ()
treeSelectionSetMode self type_ =
  {# call tree_selection_set_mode #}
    (toTreeSelection self)
    ((fromIntegral . fromEnum) type_)

-- | Gets the selection mode.
--
treeSelectionGetMode :: TreeSelectionClass self => self
 -> IO SelectionMode
treeSelectionGetMode self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe tree_selection_get_mode #}
    (toTreeSelection self)

-- | Set a callback function if selection changes.
--
-- * If set, this function is called before any
-- node is selected or unselected, giving some control over which nodes are
-- selected. The select function should return @True@ if the state of the node
-- may be toggled, and @False@ if the state of the node should be left
-- unchanged.
treeSelectionSetSelectFunction :: TreeSelectionClass self => self
 -> TreeSelectionCB -> IO ()
treeSelectionSetSelectFunction ts fun = do
  fPtr <- mkTreeSelectionFunc (\_ _ tp _ _ -> do
    path <- peekTreePath (castPtr tp)
    liftM fromBool $ fun path
    )
  {# call tree_selection_set_select_function #}
    (toTreeSelection ts)
    fPtr
    (castFunPtrToPtr fPtr)
    destroyFunPtr

-- | Callback type for a function that is called everytime the selection
-- changes. This function is set with 'treeSelectionSetSelectFunction'.
--
type TreeSelectionCB = TreePath -> IO Bool
{#pointer TreeSelectionFunc#}

foreign import ccall "wrapper"  mkTreeSelectionFunc ::
  (Ptr TreeSelection -> Ptr TreeModel -> Ptr NativeTreePath -> {#type gint#} -> Ptr () -> IO CInt)->
  IO TreeSelectionFunc

-- | Retrieve the 'TreeView' widget that this 'TreeSelection' works on.
--
treeSelectionGetTreeView :: TreeSelectionClass self => self -> IO TreeView
treeSelectionGetTreeView self =
  makeNewObject mkTreeView $
  {# call unsafe tree_selection_get_tree_view #}
    (toTreeSelection self)

-- | Retrieves the selection of a single choice 'TreeSelection'.
--
treeSelectionGetSelected :: TreeSelectionClass self => self ->
                            IO (Maybe TreeIter)
treeSelectionGetSelected self =
  receiveTreeIter $ \iterPtr ->
  {# call tree_selection_get_selected #}
    (toTreeSelection self)
    nullPtr
    iterPtr

-- | Execute a function for each selected node.
--
-- * Note that you cannot modify the tree or selection from within this
--   function. Hence, 'treeSelectionGetSelectedRows' might be more useful.
--
treeSelectionSelectedForeach :: TreeSelectionClass self => self
 -> TreeSelectionForeachCB
 -> IO ()
treeSelectionSelectedForeach self fun = do
  fPtr <- mkTreeSelectionForeachFunc (\_ _ iterPtr _ -> do
    -- make a deep copy of the iterator. This makes it possible to store this
    -- iterator in Haskell land somewhere. The TreeModel parameter is not
    -- passed to the function due to performance reasons. But since it is
    -- a constant member of Selection this does not matter.
    iter <- peek iterPtr
    fun iter
    )
  {# call tree_selection_selected_foreach #}
    (toTreeSelection self)
    fPtr
    nullPtr
  freeHaskellFunPtr fPtr

-- | Callback function type for 'treeSelectionSelectedForeach'.
--
type TreeSelectionForeachCB = TreeIter -> IO ()
{#pointer TreeSelectionForeachFunc#}

foreign import ccall "wrapper"  mkTreeSelectionForeachFunc ::
  (Ptr TreeModel -> Ptr NativeTreePath -> Ptr TreeIter -> Ptr () -> IO ()) -> IO TreeSelectionForeachFunc

#if GTK_CHECK_VERSION(2,2,0)
-- | Creates a list of paths of all selected rows.
--
-- *  Additionally, if you are
-- planning on modifying the model after calling this function, you may want to
-- convert the returned list into a list of 'TreeRowReference's. To do this,
-- you can use 'treeRowReferenceNew'.
--
-- * Available since Gtk+ version 2.2
--
treeSelectionGetSelectedRows :: TreeSelectionClass self => self
 -> IO [TreePath] -- ^ returns a list containing a 'TreePath' for
                  -- each selected row.
treeSelectionGetSelectedRows self =
  {# call gtk_tree_selection_get_selected_rows #}
    (toTreeSelection self)
    nullPtr
  >>= fromGList
  >>= mapM fromTreePath

-- | Returns the number of rows that are selected.
--
-- * Available since Gtk+ version 2.2
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
treeSelectionSelectPath :: TreeSelectionClass self => self
 -> TreePath
 -> IO ()
treeSelectionSelectPath self [] = return ()
treeSelectionSelectPath self path =
  withTreePath path $ \path ->
  {# call tree_selection_select_path #}
    (toTreeSelection self)
    path

-- | Deselect a specific item by 'TreePath'.
--
treeSelectionUnselectPath :: TreeSelectionClass self => self
 -> TreePath
 -> IO ()
treeSelectionUnselectPath self path =
  withTreePath path $ \path ->
  {# call tree_selection_unselect_path #}
    (toTreeSelection self)
    path

-- | Returns True if the row at the given path is currently selected.
--
treeSelectionPathIsSelected :: TreeSelectionClass self => self
 -> TreePath -> IO Bool
treeSelectionPathIsSelected self path =
  liftM toBool $
  withTreePath path $ \path ->
  {# call tree_selection_path_is_selected #}
    (toTreeSelection self)
    path

-- | Select a specific item by 'TreeIter'.
--
treeSelectionSelectIter :: TreeSelectionClass self => self -> TreeIter -> IO ()
treeSelectionSelectIter self iter =
  with iter $ \iterPtr ->
  {# call tree_selection_select_iter #}
    (toTreeSelection self)
    iterPtr

-- | Deselect a specific item by 'TreeIter'.
--
treeSelectionUnselectIter :: TreeSelectionClass self => self -> TreeIter -> IO ()
treeSelectionUnselectIter self iter =
  with iter $ \iterPtr ->
  {# call tree_selection_unselect_iter #}
    (toTreeSelection self)
    iterPtr

-- | Returns True if the row at the given iter is currently selected.
--
treeSelectionIterIsSelected :: TreeSelectionClass self => self
 -> TreeIter
 -> IO Bool
treeSelectionIterIsSelected self iter =
  liftM toBool $
  with iter $ \iterPtr ->
  {# call tree_selection_iter_is_selected #}
    (toTreeSelection self)
    iterPtr

-- | Selects all the nodes. The tree selection must be set to
-- 'SelectionMultiple' mode.
--
treeSelectionSelectAll :: TreeSelectionClass self => self -> IO ()
treeSelectionSelectAll self =
  {# call tree_selection_select_all #}
    (toTreeSelection self)

-- | Unselects all the nodes.
--
treeSelectionUnselectAll :: TreeSelectionClass self => self -> IO ()
treeSelectionUnselectAll self =
  {# call tree_selection_unselect_all #}
    (toTreeSelection self)

-- | Selects a range of nodes, determined by @startPath@ and @endPath@
-- inclusive. @selection@ must be set to 'SelectionMultiple' mode.
--
treeSelectionSelectRange :: TreeSelectionClass self => self
 -> TreePath -- ^ @startPath@ - The initial node of the range.
 -> TreePath -- ^ @endPath@ - The final node of the range.
 -> IO ()
treeSelectionSelectRange self startPath endPath =
  withTreePath endPath $ \endPath ->
  withTreePath startPath $ \startPath ->
  {# call tree_selection_select_range #}
    (toTreeSelection self)
    startPath
    endPath

#if GTK_CHECK_VERSION(2,2,0)
-- | Unselects a range of nodes, determined by @startPath@ and @endPath@
-- inclusive.
--
-- * Available since Gtk+ version 2.2
--
treeSelectionUnselectRange :: TreeSelectionClass self => self
 -> TreePath -- ^ @startPath@ - The initial node of the range.
 -> TreePath -- ^ @endPath@ - The initial node of the range.
 -> IO ()
treeSelectionUnselectRange self startPath endPath =
  withTreePath endPath $ \endPath ->
  withTreePath startPath $ \startPath ->
  {# call tree_selection_unselect_range #}
    (toTreeSelection self)
    startPath
    endPath
#endif

--------------------
-- Attributes

-- | \'mode\' property. See 'treeSelectionGetMode' and 'treeSelectionSetMode'
--
treeSelectionMode :: TreeSelectionClass self => Attr self SelectionMode
treeSelectionMode = newAttr
  treeSelectionGetMode
  treeSelectionSetMode

--------------------
-- Signals

-- | Emitted whenever the selection has (possibly) changed. Please note that
-- this signal is mostly a hint. It may only be emitted once when a range of
-- rows are selected, and it may occasionally be emitted when nothing has
-- happened.
--
treeSelectionSelectionChanged :: TreeSelectionClass self => Signal self (IO ())
treeSelectionSelectionChanged = Signal (connect_NONE__NONE "changed")

#ifndef DISABLE_DEPRECATED
--------------------
-- Deprecated Signals

onSelectionChanged, afterSelectionChanged :: TreeSelectionClass self => self
 -> IO ()
 -> IO (ConnectId self)
onSelectionChanged = connect_NONE__NONE "changed" False
afterSelectionChanged = connect_NONE__NONE "changed" True
#endif
