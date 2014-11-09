{-# LANGUAGE CPP, OverloadedStrings #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Interface DragSource and DragDest
--
--  Author : Axel Simon
--
--  Created: 24 July 2007
--
--  Copyright (C) 2007 Axel Simon
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
-- Interfaces for drag-and-drop support in 'Graphics.UI.Gtk.ModelView.TreeView'.
--
module Graphics.UI.Gtk.ModelView.TreeDrag (

-- * Detail
--
-- | 'Graphics.UI.Gtk.ModelView.TreeView's provide special support for
-- Drag-and-Drop such as hover-to-open-rows or autoscrolling. This module
-- implements two utility functions that set and get a path and a model in a
-- 'Graphics.UI.Gtk.General.Selection.Selection' structure. These functions
-- are thus useful to implement drag-and-drop functionality in a
-- 'Graphics.UI.Gtk.ModelView.TreeModel'. In fact, they are used as part of
-- the default drag-and-drop interfaces of
-- 'Graphics.UI.Gtk.ModelView.ListStore' and
-- 'Graphics.UI.Gtk.ModelView.TreeStore' that allows to permute rows and move
-- them between hierarchy levels.

-- * DND information for exchanging a model and a path.
  treeModelEqual,
  targetTreeModelRow,
  treeGetRowDragData,
  treeSetRowDragData,
  ) where

-- I've decided not to bind the DragSource and DragDest interfaces. They seem
-- to be useful if you (a) write your own 'TreeView' widget or (b) if you
-- can't be bothered to implement a special variant of these interfaces in
-- ListStore and TreeStore. In the latter case the interfaces are useful to
-- "simulate" a drag-and-drop that looks like a row-permutation which is the
-- interface that Gtk's ListStore and TreeStore support by default. Since
-- overriding or augmenting the dnd interfaces for ListStore and TreeStore is
-- so easy in Gtk2Hs, I think we can do without the cheat way.

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.ModelView.Types#}      (TreePath, fromTreePath, withTreePath,
                                                 NativeTreePath(..))
import Graphics.UI.Gtk.General.DNDTypes         (SelectionDataM,
                                                 TargetTag, atomNew)
import Control.Monad                            (liftM)
import Control.Monad.Trans                      (liftIO)
import Control.Monad.Reader                     (ask)

{# context lib="gtk" prefix="gtk" #}

-- this function is not necessary anymore since the models can be compared
-- using equality ==
treeModelEqual :: (TreeModelClass tm1, TreeModelClass tm2) => tm1 -> tm2 -> Bool
treeModelEqual tm1 tm2 = unTreeModel (toTreeModel tm1) == unTreeModel (toTreeModel tm2)

-- | The 'SelectionTag', 'TargetTag' and 'SelectionTypeTag' of the DND
-- mechanism of 'Graphics.UI.Gtk.ModelView.ListStore' and
-- 'Graphics.UI.Gtk.ModelView.TreeStore'. This tag is used by
-- 'treeGetRowDragData' and 'treeSetRowDragData' to store a store and a
-- 'TreePath' in a 'SelectionDataM'. This target should be added to a
-- 'Graphics.UI.Gtk.General.Selection.TargetList' using
-- 'Graphics.UI.Gtk.General.Seleciton.TargetSameWidget' flag and an
-- 'Graphics.UI.Gtk.General.Selection.InfoId' of @0@.
--
targetTreeModelRow :: TargetTag
targetTreeModelRow = unsafePerformIO $ atomNew ("GTK_TREE_MODEL_ROW"::DefaultGlibString)

-- %hash c:8dcb d:af3f
-- | Obtains a 'TreeModel' and a path from 'SelectionDataM' whenever the target is
-- 'targetTreeModelRow'. Normally called from a 'treeDragDestDragDataReceived' handler.
--
treeGetRowDragData :: SelectionDataM (Maybe (TreeModel, TreePath))
treeGetRowDragData = ask >>= \selPtr -> liftIO $ alloca $ \tmPtrPtr -> alloca $ \pathPtrPtr -> do
  isValid <- liftM toBool $
    {# call unsafe gtk_tree_get_row_drag_data #} selPtr (castPtr tmPtrPtr) (castPtr pathPtrPtr)
  if isValid then do
        tmPtr <- peek tmPtrPtr
        pathPtr <- peek pathPtrPtr
        tm <- makeNewGObject mkTreeModel (return tmPtr)
        path <- fromTreePath pathPtr
        return (Just (tm, path))
    else return Nothing

-- %hash c:e3e3 d:af3f
-- | Sets selection data with the target 'targetTreeModelRow', consisting
-- of a 'TreeModel' and a 'TreePath'. Normally used in a
-- 'treeDragSourceDragDataGet' handler.
--
-- * Returns @True@ if setting the data was successful.
--
treeSetRowDragData :: TreeModelClass treeModel => treeModel -> TreePath -> SelectionDataM Bool
treeSetRowDragData treeModel path = do
  selPtr <- ask
  liftM toBool $ liftIO $ withTreePath path $ \path ->
    {# call unsafe gtk_tree_set_row_drag_data #} selPtr
    (toTreeModel treeModel)
    path
