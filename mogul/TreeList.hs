-- -*-haskell-*-
--  The Monad GUI Library (Mogul): The TreeList collection.
--
--  Author : Axel Simon
--          
--  Created: 2 June 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
--
--  Copyright (c) 2001 Axel Simon
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
-- * This module provides all object for a widget displaying data organized
--   in a table. There are currently two flavors: A simple list organizes
--   data in rows and a table which provides the possibility to impose a
--   hierarchical structure on the entries of one column.
--
-- * The widget is composed of two parts: A database holding rows of data and
--   the widget object itself which displays items based on the database.
--   Several widgets may use a single storage object. The data in the database
--   may be what is directly displayed like strings and images or it may be
--   some meta information like the padding or color of an item. Both the
--   database and the widget have columns, allthough several columns in the
--   storage object may be used to display one column in the widget. The
--   database is called store, specifically for simple lists it's @ListStore
--   and for hierachical data it is called @TreeStore. The widget that displays
--   the data and that be inserted like any other Gtk widget in a container is
--   called @TreeView. Similary there are @StoreColumn@s and @TreeViewColumn@s
--   to handle the association between storage and display. Each 
--   @TreeViewColumn is parameterized by a derivation of @CellRenderer which
--   can be one of @CellRendererText, @CellRendererTextPixmap, 
--   @CellRendererPixmap or @CellRendererToggle. Each of these have some 
--   @Attribute@s which can be set. 
--   The procedures is as follows: After creating an empty database 
--   (either @TreeStore or @ListStore) @Attribute@s can be added to the
--   store. Each added @Attribute will result in a @StoreColumn. On the
--   widget side one needs to create a new @TreeView. Through the 
--   functions @treeViewCreateTextColumn, @treeViewCreatePixmapColumn, etc.
--   a @TreeViewColumn can be created. Through @treeViewColumnAssociate the
--   generated @StoreColumn@s can be added.
--   
--
--- DOCU ----------------------------------------------------------------------
--
--  * This module is currently broken, because the Gtk 2 changed the functions
--    to build the tables....
--
--- TODO ----------------------------------------------------------------------

module TreeList(
{-
  treeStoreCreateColumn,
  listStoreCreateColumn,
  StoreColumn,		-- abstract
  storeColumnSetValue,
  storeColumnGetValue,
  newTreeViewTextColumn,
  newTreeViewTextColumnAt,
  newTreeViewPixbufColumn,
  newTreeViewPixbufColumnAt,
  newTreeViewTextPixbufColumn,
  newTreeViewTextPixbufColumnAt,
  newTreeViewToggleColumn,
  newTreeViewToggleColumnAt,
  treeViewRemoveColumn,
  treeViewGetColumn,
  ViewColumn,		-- abstract
  viewColumnAssociate,
  viewColumnSetVisible,
  viewColumnGetVisible,
  viewColumnSetSizing,
  viewColumnGetSizing,
  viewColumnGetWidth,
  viewColumnSetWidth,
  viewColumnSetMinWidth,
  viewColumnGetMinWidth,
  viewColumnSetMaxWidth,
  viewColumnGetMaxWidth,
  viewColumnSetTitle,
  viewColumnGetTitle,
  viewColumnSetClickable,
  viewColumnSetWidget,
  viewColumnGetWidget,
  viewColumnSetAlignment,
  viewColumnGetAlignment,
  viewColumnClicked,
  CellRenderer,
  CellRendererClass,
  TreePath,
  treeModelGetIter,
  treeModelGetPath
-}
  ) where

{-
import Monad	(liftM)
import Hierarchy
import GType	(typeInstanceIsA)
import Gtk	hiding (
  -- TreeModel
  treeModelGetNColumns,
  treeModelGetColumnType,
  treeModelGetValue,
  TreePath,
  treeModelGetIter,
  treeModelGetPath,
  -- TreeStore
  treeStoreSetNColumns,
  treeStoreSetColumnType,
  treeStoreSetValue,
  -- ListStore
  listStoreSetNColumns,
  listStoreSetColumnType,
  listStoreSetValue,
  -- TreeView
  treeViewAppendColumn,
  treeViewInsertColumn,
  treeViewRemoveColumn,
  treeViewGetColumn)

obj # meth = meth obj

-- The abstractly exported @StoreColumn data type. (EXPORTED)
--
-- * Take a predefined @Attribute @cr @argType from a @CellRenderer @cr and 
--   call @treeStoreCreateColumn or @listStoreCreateColumn to get a 
--   @StoreColumn @cr @argType. This can then be used with 
--   @storeColumnSetValue to store some data value of type @argType.
--
data (CellRendererClass cr) => StoreColumn cr argType
  = StoreColumn Int TreeModel 
    (TreeIter -> Int -> GenericValue -> TreeModel -> IO ()) 
    (Attribute cr argType)


-- Create a new column in the @TreeStore database. (EXPORTED)
--
-- * The type of the column is determined by the @Attribute of the 
--   @ViewColumn which should be stored here. It is possible to associate
--   this column with several @ViewColumn@s.
--
treeStoreCreateColumn :: (TreeStoreClass ts, CellRendererClass cr) =>
  Attribute cr argTy -> ts -> IO (StoreColumn cr argTy)
treeStoreCreateColumn (attr@(AttrSingle _ ty _ _)) ts = do
  col <- ts # Gtk.treeModelGetNColumns
  ts # Gtk.treeStoreSetNColumns (col+1)
  ts # Gtk.treeStoreSetColumnType col ty
  return $ StoreColumn col (toTreeModel ts) (\iter col val tm ->
	     Gtk.treeStoreSetValue iter col val 
	     (fromTreeModel tm :: TreeStore)) attr
treeStoreCreateColumn (attr@(AttrDouble _ ty1 _ ty2 _ _)) ts = do
  col <- ts # Gtk.treeModelGetNColumns
  ts # Gtk.treeStoreSetNColumns (col+2)
  ts # Gtk.treeStoreSetColumnType col ty1
  ts # Gtk.treeStoreSetColumnType (col+1) ty2
  return $ StoreColumn col (toTreeModel ts) (\iter col val tm ->
	     Gtk.treeStoreSetValue iter col val 
	     (fromTreeModel tm :: TreeStore)) attr

-- Create a new column in the @ListStore database. (EXPORTED)
--
-- * The type of the column is determined by the @Attribute of the 
--   @ViewColumn which should be stored here. It is possible to associate
--   this column with several @ViewColumn@s.
--
listStoreCreateColumn :: (ListStoreClass ls, CellRendererClass cr) =>
  Attribute cr argTy -> ls -> IO (StoreColumn cr argTy)
listStoreCreateColumn (attr@(AttrSingle _ ty _ _)) ls = do
  col <- ls # Gtk.treeModelGetNColumns
  ls # Gtk.listStoreSetNColumns (col+1)
  ls # Gtk.listStoreSetColumnType col ty
  return $ StoreColumn col (toTreeModel ls) (\iter col val tm ->
	     Gtk.listStoreSetValue iter col val 
	     (fromTreeModel tm :: ListStore)) attr
listStoreCreateColumn (attr@(AttrDouble _ ty1 _ ty2 _ _)) ls = do
  col <- ls # Gtk.treeModelGetNColumns
  ls # Gtk.listStoreSetNColumns (col+2)
  ls # Gtk.listStoreSetColumnType col ty1
  ls # Gtk.listStoreSetColumnType (col+1) ty2
  return $ StoreColumn col (toTreeModel ls) (\iter col val tm ->
	     Gtk.listStoreSetValue iter col val 
	     (fromTreeModel tm :: ListStore)) attr


-- Set a value in a @StoreColumn. (EXPORTED)
--
storeColumnSetValue :: (CellRendererClass cr) => 
  TreeIter -> a -> StoreColumn cr a -> IO ()
storeColumnSetValue iter val (StoreColumn col tm store
  (AttrSingle _ _ toGen _)) = do
  gval <- toGen val
  tm # store iter col gval
storeColumnSetValue iter val (StoreColumn col tm store
  (AttrDouble _ _ _ _ toGen _)) = do
  (gval1, gval2) <- toGen val
  tm # store iter col gval1
  tm # store iter (col+1) gval2
   
-- Get the value from a @StoreColumn. (EXPORTED)
--
storeColumnGetValue :: (CellRendererClass cr) =>
  TreeIter -> StoreColumn cr a -> IO a
storeColumnGetValue iter (StoreColumn col tm _
  (AttrSingle _ _ _ fromGen)) = do
  gval <- tm # Gtk.treeModelGetValue iter col
  fromGen gval
storeColumnGetValue iter (StoreColumn col tm _
  (AttrDouble _ _ _ _ _ fromGen)) = do
  gval1 <- tm # Gtk.treeModelGetValue iter col
  gval2 <- tm # Gtk.treeModelGetValue iter (col+1)
  fromGen gval1 gval2

-- A @TreeView can hold an arbitrary number of @ViewColumn@s which are defined
-- here. (EXPORTED)
--
-- * A @ViewColumn is parameterized over the @CellRenderer it uses to display
--   the cells.
--
newtype (CellRendererClass cr) => ViewColumn cr = ViewColumn TreeViewColumn

-- Create a new @ViewColumn that displays text. (EXPORTED)
--
-- * Appends the column. To insert the column at a special place use the
--   @newTreeViewTextColumnAt function.
--
newTreeViewTextColumn :: TreeView -> IO (ViewColumn CellRendererText)
newTreeViewTextColumn tv = do
  cr <- cellRendererTextNew
  tvc <- Gtk.treeViewColumnNew
  tvc # Gtk.treeViewColumnSetCellRenderer cr
  tv # Gtk.treeViewAppendColumn tvc
  return $ ViewColumn tvc

-- Insert a new @ViewColumn at a specific position. (EXPORTED)
--
newTreeViewTextColumnAt :: Int -> TreeView -> IO (ViewColumn CellRendererText)
newTreeViewTextColumnAt pos tv = do
  cr <- cellRendererTextNew
  tvc <- Gtk.treeViewColumnNew
  tvc # Gtk.treeViewColumnSetCellRenderer cr
  tv # Gtk.treeViewInsertColumn tvc pos
  return $ ViewColumn tvc

-- Create a new @ViewColumn that displays pixbuf. (EXPORTED)
--
-- * Appends the column. To insert the column at a special place use the
--   @newTreeViewPixbufColumnAt function.
--
newTreeViewPixbufColumn :: TreeView -> IO (ViewColumn CellRendererPixbuf)
newTreeViewPixbufColumn tv = do
  cr <- cellRendererPixbufNew
  tvc <- Gtk.treeViewColumnNew
  tvc # Gtk.treeViewColumnSetCellRenderer cr
  tv # Gtk.treeViewAppendColumn tvc
  return $ ViewColumn tvc

-- Insert a new @ViewColumn at a specific position. (EXPORTED)
--
newTreeViewPixbufColumnAt :: Int -> TreeView -> 
  IO (ViewColumn CellRendererPixbuf)
newTreeViewPixbufColumnAt pos tv = do
  cr <- cellRendererPixbufNew
  tvc <- Gtk.treeViewColumnNew
  tvc # Gtk.treeViewColumnSetCellRenderer cr
  tv # Gtk.treeViewInsertColumn tvc pos
  return $ ViewColumn tvc

-- Create a new @ViewColumn that displays textPixbuf. (EXPORTED)
--
-- * Appends the column. To insert the column at a special place use the
--   @newTreeViewTextPixbufColumnAt function.
--
newTreeViewTextPixbufColumn :: TreeView -> 
  IO (ViewColumn CellRendererTextPixbuf)
newTreeViewTextPixbufColumn tv = do
  cr <- cellRendererTextPixbufNew
  tvc <- Gtk.treeViewColumnNew
  tvc # Gtk.treeViewColumnSetCellRenderer cr
  tv # Gtk.treeViewAppendColumn tvc
  return $ ViewColumn tvc

-- Insert a new @ViewColumn at a specific position. (EXPORTED)
--
newTreeViewTextPixbufColumnAt :: Int -> TreeView -> 
  IO (ViewColumn CellRendererTextPixbuf)
newTreeViewTextPixbufColumnAt pos tv = do
  cr <- cellRendererTextPixbufNew
  tvc <- Gtk.treeViewColumnNew
  tvc # Gtk.treeViewColumnSetCellRenderer cr
  tv # Gtk.treeViewInsertColumn tvc pos
  return $ ViewColumn tvc

-- Create a new @ViewColumn that displays toggle. (EXPORTED)
--
-- * Appends the column. To insert the column at a special place use the
--   @newTreeViewToggleColumnAt function.
--
newTreeViewToggleColumn :: TreeView ->IO (ViewColumn CellRendererToggle)
newTreeViewToggleColumn tv = do
  cr <- cellRendererToggleNew
  tvc <- Gtk.treeViewColumnNew
  tvc # Gtk.treeViewColumnSetCellRenderer cr
  tv # Gtk.treeViewAppendColumn tvc
  return $ ViewColumn tvc

-- Insert a new @ViewColumn at a specific position. (EXPORTED)
--
newTreeViewToggleColumnAt :: Int -> TreeView -> 
  IO (ViewColumn CellRendererToggle)
newTreeViewToggleColumnAt pos tv = do
  cr <- cellRendererToggleNew
  tvc <- Gtk.treeViewColumnNew
  tvc # Gtk.treeViewColumnSetCellRenderer cr
  tv # Gtk.treeViewInsertColumn tvc pos
  return $ ViewColumn tvc

-- Remove a @ViewColumn from a @TreeView widget. The number of remaining
-- columns is returned. (EXPORTED)
--
treeViewRemoveColumn :: (CellRendererClass cr) => 
  ViewColumn cr -> TreeView -> IO Int
treeViewRemoveColumn (ViewColumn tvc) tv =
  Gtk.treeViewRemoveColumn tvc tv

-- Retrieve the @ViewColumn at a specific position. (EXPORTED)
--
-- * The return value is of type @ViewColumn @CellRenderer so the only use
--   for this function is to call @treeViewRemoveColumn afterwards.
--
treeViewGetColumn :: Int -> TreeView -> IO (Maybe (ViewColumn CellRenderer))
treeViewGetColumn pos tv = 
  liftM (fmap ViewColumn) $ Gtk.treeViewGetColumn pos tv

-- Use the information in @StoreColumn to alter the displayed cell. (EXPORTED)
--
viewColumnAssociate :: (CellRendererClass cr) => 
  StoreColumn cr a -> ViewColumn cr -> IO ()
viewColumnAssociate (StoreColumn col _ _ (AttrSingle attr _ _ _)) 
  (ViewColumn tvc) = do
    tvc # treeViewColumnAddAttribute attr col
viewColumnAssociate (StoreColumn col _ _ (AttrDouble attr1 _ attr2 _ _ _)) 
  (ViewColumn tvc) = do
    tvc # treeViewColumnAddAttribute attr1 col
    tvc # treeViewColumnAddAttribute attr2 (col+1)

-- @see treeViewColumnSetVisible
--
viewColumnSetVisible :: CellRendererClass cr => Bool -> ViewColumn cr -> IO ()
viewColumnSetVisible vis (ViewColumn tvc) = treeViewColumnSetVisible vis tvc

-- @see treeViewColumnGetVisible
--
viewColumnGetVisible :: CellRendererClass cr => ViewColumn cr -> IO Bool
viewColumnGetVisible (ViewColumn tvc) = treeViewColumnGetVisible tvc

-- @see treeViewColumnSetSizing
--
viewColumnSetSizing :: CellRendererClass cr => 
  TreeViewColumnSizing -> ViewColumn cr -> IO ()
viewColumnSetSizing size (ViewColumn tvc) = treeViewColumnSetSizing size tvc

-- @see treeViewColumnGetSizing
--
viewColumnGetSizing :: CellRendererClass cr => 
  ViewColumn cr -> IO TreeViewColumnSizing
viewColumnGetSizing (ViewColumn tvc) = treeViewColumnGetSizing tvc

-- @see treeViewColumnGetWidth
--
viewColumnGetWidth :: CellRendererClass cr => ViewColumn cr -> IO Int
viewColumnGetWidth (ViewColumn tvc) = treeViewColumnGetWidth tvc

-- @see treeViewColumnSetWidth
--
viewColumnSetWidth :: CellRendererClass cr => ViewColumn cr -> Int -> IO ()
viewColumnSetWidth (ViewColumn tvc) = treeViewColumnSetWidth tvc

-- @see treeViewColumnSetMinWidth
--
viewColumnSetMinWidth :: CellRendererClass cr => Int -> ViewColumn cr -> IO ()
viewColumnSetMinWidth w (ViewColumn tvc) = treeViewColumnSetMinWidth w tvc

-- @see treeViewColumnGetMinWidth
--
viewColumnGetMinWidth :: CellRendererClass cr => ViewColumn cr -> IO Int
viewColumnGetMinWidth (ViewColumn tvc) = treeViewColumnGetMinWidth tvc

-- @see treeViewColumnSetMaxWidth
--
viewColumnSetMaxWidth :: CellRendererClass cr => Int -> ViewColumn cr -> IO ()
viewColumnSetMaxWidth w (ViewColumn tvc) = treeViewColumnSetMaxWidth w tvc

-- @see treeViewColumnGetMaxWidth
--
viewColumnGetMaxWidth :: CellRendererClass cr => ViewColumn cr -> IO Int
viewColumnGetMaxWidth (ViewColumn tvc) = treeViewColumnGetMaxWidth tvc

-- @see treeViewColumnSetTitle
--
viewColumnSetTitle :: CellRendererClass cr => String -> ViewColumn cr -> IO ()
viewColumnSetTitle t (ViewColumn tvc) = treeViewColumnSetTitle t tvc

-- @see treeViewColumnGetTitle
--
viewColumnGetTitle :: CellRendererClass cr => 
  ViewColumn cr -> IO (Maybe String)
viewColumnGetTitle (ViewColumn tvc) = treeViewColumnGetTitle tvc

-- @see treeViewColumnSetClickable
--
viewColumnSetClickable :: CellRendererClass cr => 
  Bool -> ViewColumn cr -> IO ()
viewColumnSetClickable c (ViewColumn tvc) = treeViewColumnSetClickable c tvc

-- @see treeViewColumnSetWidget
--
viewColumnSetWidget :: (CellRendererClass cr, WidgetClass w) => 
  w -> ViewColumn cr -> IO ()
viewColumnSetWidget w (ViewColumn tvc) = treeViewColumnSetWidget w tvc

-- @see treeViewColumnGetWidget
--
viewColumnGetWidget :: CellRendererClass cr => ViewColumn cr -> IO Widget
viewColumnGetWidget (ViewColumn tvc) = treeViewColumnGetWidget tvc

-- @see treeViewColumnSetAlignment
--
viewColumnSetAlignment :: CellRendererClass cr => 
  Float -> ViewColumn cr -> IO ()
viewColumnSetAlignment al (ViewColumn tvc) = treeViewColumnSetAlignment al tvc

-- @see treeViewColumnGetAlignment
--
viewColumnGetAlignment :: CellRendererClass cr =>  ViewColumn cr -> IO Float
viewColumnGetAlignment (ViewColumn tvc) = treeViewColumnGetAlignment tvc

-- @see treeViewColumnClicked
--
viewColumnClicked :: CellRendererClass cr => ViewColumn cr -> IO ()
viewColumnClicked (ViewColumn tvc) = treeViewColumnClicked tvc


-- TreePath: A casual way of addressing nodes in a hierarchical structure. 
-- (EXPORTED)
--
type TreePath = [Int]

-- Retreive non-abstract position of a node in a @TreeList/@TreeStore. 
-- (EXPORTED)
--
treeModelGetIter :: TreeModelClass tm => TreePath -> tm -> IO (Maybe TreeIter)
treeModelGetIter tp tm = do
  realPath <- Gtk.treePathNew
  mapM_ (\i -> Gtk.treePathAppendIndex i realPath) tp
  Gtk.treeModelGetIter realPath tm

treeModelGetPath :: TreeModelClass tm => TreeIter -> tm -> IO TreePath
treeModelGetPath ti tm = do
  realPath <- Gtk.treeModelGetPath ti tm
  Gtk.treePathGetIndices realPath
-}