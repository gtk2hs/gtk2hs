-- -*-haskell-*-
--  The Monad GUI Library (Mogul): Widget TreeView
--
--  Author : Axel Simon
--          
--  Created: 2 June 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/17 00:13:21 $
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
-- |
--
-- This module provides all object for a widget displaying data organized
-- in a table. 
--
-- * There are two flavors: A simple list organizes
--   data in rows and a tree which provides the possibility to impose a
--   hierarchical structure on the entries of one column.
--
-- * The widget is composed of two parts: A database holding rows of data and
--   the widget object itself which displays items based on the database.
--   Several widgets may use a single storage object. The data in the database
--   may be what is directly displayed like strings and images or it may be
--   some meta information like the padding or color of an item. Several
--   attributes in the storage object may be used to display one column
--   in the widget. In contrast each row in the store corresponds to a row
--   in the widget.
--
-- * The widget that displays the data and can be inserted like any other
--   into a container is called 'TreeView'. This widget is itself
--   a container for 'TreeViewColumn's which has a title at the top
--   of the column. Each 'TreeViewColumn' in turn can contain
--   several 'Renderer'. There are currently three
--   'Renderer', one for each of the following items: 
--   text, 'Pixbuf' and 'ToggleButton'.
--
-- * The database is called store, specifically for simple lists it is
--   'ListStore' and for hierachical data it is called 
--   'TreeStore'. A store is created from a skeleton. 
--   '' Attributes can be added to an empty 
--   'ListSkel' or 'TreeSkel' skeleton which yields
--   a functions to access the attribute and an 'Association'.
--   After the skeleton is turned into a store by calling either
--   'newListStore' or 'newTreeStore', 
--   'Association's can be inserted together with an appropriate
--   'Renderer' into a 'TreeViewColumn'.
--
-- TODO
--
-- * Figure out if properties in the store have priority over global
--   properties when both are set.
--
module Graphics.UI.Gtk.Mogul.TreeList (
  -- * ListStore
  ListSkel,
  emptyListSkel,
  listSkelAddAttribute,
  newListStore,
  -- * TreeStore
  TreeSkel,
  emptyTreeSkel,
  treeSkelAddAttribute,
  newTreeStore,
  -- * Widget
  Association,
  Renderer,
  treeViewColumnNewText,
  treeViewColumnNewPixbuf,
  treeViewColumnNewToggle,
  treeViewColumnAssociate,
  -- * CellRenderer
  cellRendererSetAttribute,
  cellRendererGetAttribute,
  -- * CellRendererText
  onEdited,
  afterEdited,  
  ) where

import Monad	(liftM, mapM, mapM_, foldM)
import System.Glib.GType	(typeInstanceIsA)
import Graphics.UI.Gtk	hiding (
  -- TreeModel
  treeModelGetValue,
  treeModelGetIter,
  treeModelGetPath,
  -- ListStore
  listStoreNew,
  listStoreSetValue,
  -- TreeStore
  treeStoreNew,
  treeStoreSetValue,
  -- TreeViewColumn
  treeViewColumnAddAttribute,
  -- CellRendererText
  onEdited,
  afterEdited,
  -- TreeView
  treeViewGetPathAtPos)

import Data.IORef		(IORef(..), newIORef, readIORef, writeIORef)
import Control.Exception	(throw, Exception(AssertionFailed))

import qualified Graphics.UI.Gtk as Gtk


-- | A skeleton of a 'ListStore' database.
--
-- * This datastructure describes what columns the database will have when
--   it is finally created by 'newListStore'.
--
newtype ListSkel = ListSkel (IORef ListSkelState)

data ListSkelState = LSSPrepare [TMType]
		   | LSSActive ListStore


-- | Returns an empty 'ListSkel'.
--
emptyListSkel :: IO ListSkel
emptyListSkel = liftM ListSkel (newIORef (LSSPrepare []))

-- | Reserve a new column in
-- 'ListSkel' to hold values for the given attribute.
--
-- * The type of the column is determined by the given 'Attribute'
--   of the 'ViewColumn' which should be stored here. It is possible
--   to associate this column with several 'ViewColumn's.
--
listSkelAddAttribute :: CellRendererClass cr => 
			ListSkel -> 
			Attribute cr argTy ->
			IO (Association cr,
			    TreeIter -> IO argTy,
			    TreeIter -> argTy -> IO ())
listSkelAddAttribute (ListSkel statusRef) 
		     (Attribute prop ty toGen fromGen) = do
  status <- readIORef statusRef
  case status of 
    LSSPrepare tTree -> do
      writeIORef statusRef (LSSPrepare (ty++tTree))
      let columnNo = length tTree
      let cols	   = length ty
      return (Association prop columnNo,
	\ti -> do
        status <- readIORef statusRef
	case status of 
	  LSSPrepare _ -> throw $ AssertionFailed 
	    "Modul.TreeStore<readValue>: \
	    \skeleton was not converted to a TreeStore before data access."
	  LSSActive ls -> mapM (Gtk.treeModelGetValue ls ti) 
			  [columnNo..columnNo+cols-1]
			  >>= fromGen,
	\ti arg -> do
        status <- readIORef statusRef
	case status of 
	  LSSPrepare _ -> throw $ AssertionFailed 
	    "Modul.TreeStore<writeValue>: \
	    \skeleton was not converted to a TreeStore before data access."
	  LSSActive ls -> liftM (zip [columnNo..]) (toGen arg) >>= 
			  mapM_ (uncurry (Gtk.listStoreSetValue ls ti))
	)


-- | Create a new 'ListStore' database.
--
-- * This method throws an exception if the skeleton has been used before.
--
newListStore :: ListSkel -> IO ListStore
newListStore (ListSkel statusRef) = do
  status <- readIORef statusRef
  case status of
    LSSPrepare tList -> do
      ls <- Gtk.listStoreNew (reverse tList)
      writeIORef statusRef (LSSActive ls)
      return ls
    LSSActive _ -> throw $ AssertionFailed 
      "Mogul.newListStore: tried to reuse a ListStore skeleton."


-- | A skeleton of a 'TreeStore' database.
--
-- * This datastructure describes what columns the database will have when
--   it is finally created by 'newTreeStore'
--
newtype TreeSkel = TreeSkel (IORef TreeSkelState)

data TreeSkelState = TSSPrepare [TMType]
		   | TSSActive TreeStore


-- | Returns an empty 'TreeSkel'.
--
emptyTreeSkel :: IO TreeSkel
emptyTreeSkel = liftM TreeSkel (newIORef (TSSPrepare []))

-- | Reserve a new column in
-- 'TreeSkel' to hold values for the given attribute.
--
-- * The type of the column is determined by the given 'Attribute'
--   of the 'ViewColumn' which should be stored here. It is possible
--   to associate this column with several 'ViewColumn's.
--
treeSkelAddAttribute :: CellRendererClass r => TreeSkel -> 
			Attribute r argTy ->
			IO (Association r,
			    TreeIter -> IO argTy,
			    TreeIter -> argTy -> IO ())
treeSkelAddAttribute (TreeSkel statusRef) 
		     (Attribute prop ty toGen fromGen) = do
  status <- readIORef statusRef
  case status of 
    TSSPrepare tTree -> do
      writeIORef statusRef (TSSPrepare (ty++tTree))
      let columnNo = length tTree
      let cols	   = length ty
      return (Association prop columnNo,
	\ti -> do
        status <- readIORef statusRef
	case status of 
	  TSSPrepare _ -> throw $ AssertionFailed 
	    "Modul.TreeStore<readValue>: \
	    \skeleton was not converted to a TreeStore before data access."
	  TSSActive ls -> mapM (Gtk.treeModelGetValue ls ti)
			  [columnNo..columnNo+cols-1]
			  >>= fromGen,
	\ti arg -> do
        status <- readIORef statusRef
	case status of 
	  TSSPrepare _ -> throw $ AssertionFailed 
	    "Modul.TreeStore<writeValue>: \
	    \skeleton was not converted to a TreeStore before data access."
	  TSSActive ls -> liftM (zip [columnNo..]) (toGen arg) >>= 
			  mapM_ (uncurry $Gtk.treeStoreSetValue ls ti)
	)

-- | Create a new 'TreeStore' database.
--
-- * This method throws an exception if the skeleton has been used before.
--
newTreeStore :: TreeSkel -> IO TreeStore
newTreeStore (TreeSkel statusRef) = do
  status <- readIORef statusRef
  case status of
    TSSPrepare tTree -> do
      ls <- Gtk.treeStoreNew (reverse tTree)
      writeIORef statusRef (TSSActive ls)
      return ls
    TSSActive _ -> throw $ AssertionFailed 
      "Mogul.newTreeStore: tried to reuse a TreeStore skeleton."

-- | An abstract link between a store and a view.
--
data CellRendererClass cr => Association cr = Association [String] Int

-- | A renderer for text in a 'TreeView'.
--
data CellRendererClass cr => Renderer cr = Renderer cr TreeViewColumn

-- | Create a new rederer showing text.
--
-- * There can be several 'Renderer' in each 
--   'TreeViewColumn'. Each 'Renderer' can reflect
--   several 'Attributes' from a 'ListStore' or
--   'TreeStore'.
--
treeViewColumnNewText :: TreeViewColumn -> Bool -> Bool -> 
			 IO (Renderer CellRendererText)
treeViewColumnNewText tvc atStart expand = do
  ren <- cellRendererTextNew
  (if atStart then Gtk.treeViewColumnPackStart else Gtk.treeViewColumnPackEnd)
    tvc ren expand
  return $ Renderer ren tvc

-- | Create a new renderer showing a 'Pixbuf'.
--
--
-- * There can be several 'Renderer' in each 
--   'TreeViewColumn'. Each 'Renderer' can reflect
--   several 'Attributes' from a 'ListStore' or
--   'TreeStore'.
--
treeViewColumnNewPixbuf :: TreeViewColumn -> Bool -> Bool -> 
			   IO (Renderer CellRendererPixbuf)
treeViewColumnNewPixbuf tvc atStart expand = do
  ren <- cellRendererPixbufNew
  (if atStart then Gtk.treeViewColumnPackStart else Gtk.treeViewColumnPackEnd)
    tvc ren expand
  return $ Renderer ren tvc

-- | Create a new renderer showing a 'ToggleButton'.
--
-- * There can be several 'Renderer' in each 
--   'TreeViewColumn'. Each 'Renderer' can reflect
--   several 'Attributes' from a 'ListStore' or
--   'TreeStore'.
--
treeViewColumnNewToggle :: TreeViewColumn -> Bool -> Bool ->
			   IO (Renderer CellRendererToggle)
treeViewColumnNewToggle tvc atStart expand = do
  ren <- cellRendererToggleNew
  (if atStart then Gtk.treeViewColumnPackStart else Gtk.treeViewColumnPackEnd)
    tvc ren expand
  return $ Renderer ren tvc

-- | Create a link between the store and this model.
--
-- * The results are undefined, if this 'TreeViewColumn' was not
--   created with the same 'TreeModel' as the 'Association's.
--
treeViewColumnAssociate :: CellRendererClass r => Renderer r -> 
						  [Association r] -> IO ()
treeViewColumnAssociate (Renderer ren  tvc) assocs = do
  let assocs' = concatMap (\(Association strs col) -> zip strs [col..]) assocs
  mapM_ (\(attr,col) ->
    Gtk.treeViewColumnAddAttribute tvc ren attr col) assocs'

-- | Set an 'Attribute' globally.
--
-- * An 'Attribute' of a 'Renderer' can either be set
--   on a row-by-row basis using 'listSkelAddAttribute' and
--   'treeSkelAddAttribute' or globally through this function.
--   
cellRendererSetAttribute :: CellRendererClass cr => Renderer cr ->
						    Attribute cr val ->
						    val -> IO ()
cellRendererSetAttribute (Renderer ren _) = Gtk.cellRendererSet ren

-- | Get an global 'Attribute'.
--
cellRendererGetAttribute :: CellRendererClass cr => Renderer cr ->
						    Attribute cr val ->
						    IO val
cellRendererGetAttribute (Renderer ren _) = Gtk.cellRendererGet ren


-- | Emitted when the user finished editing a cell.
--
-- * This signal is not emitted when editing is disabled (see 
--   'cellEditable') or when the user aborts editing.
--
onEdited, afterEdited :: TreeModelClass tm =>
			 Renderer CellRendererText -> tm ->
			 (TreeIter -> String -> IO ()) ->
			 IO (ConnectId CellRendererText)
onEdited (Renderer ren _) = Gtk.onEdited ren
afterEdited (Renderer ren _) = Gtk.afterEdited ren
