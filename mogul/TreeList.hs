-- -*-haskell-*-
--  The Monad GUI Library (Mogul): The TreeList collection.
--
--  Author : Axel Simon
--          
--  Created: 2 June 2001
--
--  Version $Revision: 1.4 $ from $Date: 2002/07/08 16:50:00 $
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
--
--- TODO ----------------------------------------------------------------------

module TreeList(
  ListSkel,
  emptyListSkel,
  listSkelAddAttribute,
  newListStore,
  TreeSkel,
  emptyTreeSkel,
  treeSkelAddAttribute,
  newTreeStore,
  Association,
  Renderer,
  treeViewColumnNewText,
  treeViewColumnNewPixbuf,
  treeViewColumnNewToggle,
  treeViewColumnAssociate,
  TreePath,
  treeModelGetIter,
  treeModelGetPath
  ) where

import Monad	(liftM)
import GType	(typeInstanceIsA)
import Gtk	hiding (
  -- TreeModel
  treeModelGetValue,
  TreePath,
  treePathNew,
  treePathNewFromString,
  treePathToString,
  treePathNewFirst,
  treePathAppendIndex,
  treePathPrependIndex,
  treePathGetDepth,
  treePathGetIndices,
  treePathCopy,
  treePathCompare,
  treePathNext,
  treePathPrev,
  treePathUp,
  treePathDown,
  treeModelGetIter,
  treeModelGetPath,
  -- ListStore
  listStoreNew,
  listStoreSetValue,
  -- TreeStore
  treeStoreNew,
  treeStoreSetValue,
  -- TreeViewColumn
  treeViewColumnAddAttribute)
import qualified Gtk
import IOExts	(IORef(..), newIORef, readIORef, writeIORef)
import Exception(throw, Exception(AssertionFailed))

-- @data ListSkel@ A skeleton of a @ref type ListStore@ database.
--
-- * This datastructure describes what columns the database will have when
--   it is finally created by @ref method newListStore@
--
newtype ListSkel = ListSkel (IORef ListSkelState)

data ListSkelState = LSSPrepare [TMType]
		   | LSSActive ListStore


-- @method emptyListSkel@ Returns an empty @ref type ListSkel@.
--
emptyListSkel :: IO ListSkel
emptyListSkel = liftM ListSkel (newIORef (LSSPrepare []))

-- @method listSkelAddAttribute@ Reserve a new column in
-- @ref type ListSkel@ to hold values for the given attribute.
--
-- * The type of the column is determined by the given @ref type Attribute@
--   of the 
--   @ViewColumn which should be stored here. It is possible to associate
--   this column with several @ViewColumn@s.
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
    LSSPrepare tList -> do
      writeIORef statusRef (LSSPrepare (ty:tList))
      let columnNo = length tList
      return (Association prop columnNo,
	\ti -> do
        status <- readIORef statusRef
	case status of 
	  LSSPrepare _ -> throw $ AssertionFailed 
	    "Modul.ListStore<readValue>: \
	    \skeleton was not converted to a ListStore before data access."
	  LSSActive ls -> Gtk.treeModelGetValue ls ti columnNo >>= fromGen,
	\ti arg -> do
        status <- readIORef statusRef
	case status of 
	  LSSPrepare _ -> throw $ AssertionFailed 
	    "Modul.ListStore<writeValue>: \
	    \skeleton was not converted to a ListStore before data access."
	  LSSActive ls -> toGen arg >>= Gtk.listStoreSetValue ls ti columnNo
	)


-- @method newListStore@ Create a new @ref type ListStore@ database.
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

-- @data TreeSkel@ A skeleton of a @ref type TreeStore@ database.
--
-- * This datastructure describes what columns the database will have when
--   it is finally created by @ref method newTreeStore@
--
newtype TreeSkel = TreeSkel (IORef TreeSkelState)

data TreeSkelState = TSSPrepare [TMType]
		   | TSSActive TreeStore


-- @method emptyTreeSkel@ Returns an empty @ref type TreeSkel@.
--
emptyTreeSkel :: IO TreeSkel
emptyTreeSkel = liftM TreeSkel (newIORef (TSSPrepare []))

-- @method treeSkelAddAttribute@ Reserve a new column in
-- @ref type TreeSkel@ to hold values for the given attribute.
--
-- * The type of the column is determined by the given @ref type Attribute@
--   of the 
--   @ViewColumn which should be stored here. It is possible to associate
--   this column with several @ViewColumn@s.
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
      writeIORef statusRef (TSSPrepare (ty:tTree))
      let columnNo = 1+length tTree
      return (Association prop columnNo,
	\ti -> do
        status <- readIORef statusRef
	case status of 
	  TSSPrepare _ -> throw $ AssertionFailed 
	    "Modul.TreeStore<readValue>: \
	    \skeleton was not converted to a TreeStore before data access."
	  TSSActive ls -> Gtk.treeModelGetValue ls ti columnNo >>= fromGen,
	\ti arg -> do
        status <- readIORef statusRef
	case status of 
	  TSSPrepare _ -> throw $ AssertionFailed 
	    "Modul.TreeStore<writeValue>: \
	    \skeleton was not converted to a TreeStore before data access."
	  TSSActive ls -> toGen arg >>= Gtk.treeStoreSetValue ls ti columnNo
	)


-- @method newTreeStore@ Create a new @ref type TreeStore@ database.
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

-- @data Association@ An abstract link between a store and a view.
--
data CellRendererClass cr => Association cr = Association String Int

-- @data TextRenderer@ A renderer for text in a @ref type TreeView@.
--
data CellRendererClass cr => Renderer cr = Renderer cr TreeViewColumn

treeViewColumnNewText :: TreeViewColumn -> Bool -> Bool -> 
			 IO (Renderer CellRendererText)
treeViewColumnNewText tvc atStart expand = do
  ren <- cellRendererTextNew
  (if atStart then Gtk.treeViewColumnPackStart else Gtk.treeViewColumnPackEnd)
    tvc ren expand
  return $ Renderer ren tvc

treeViewColumnNewPixbuf :: TreeViewColumn -> Bool -> Bool -> 
			   IO (Renderer CellRendererPixbuf)
treeViewColumnNewPixbuf tvc atStart expand = do
  ren <- cellRendererPixbufNew
  (if atStart then Gtk.treeViewColumnPackStart else Gtk.treeViewColumnPackEnd)
    tvc ren expand
  return $ Renderer ren tvc

treeViewColumnNewToggle :: TreeViewColumn -> Bool -> Bool ->
			   IO (Renderer CellRendererToggle)
treeViewColumnNewToggle tvc atStart expand = do
  ren <- cellRendererToggleNew
  (if atStart then Gtk.treeViewColumnPackStart else Gtk.treeViewColumnPackEnd)
    tvc ren expand
  return $ Renderer ren tvc

-- @method treeViewColumnAssociate@ Create a link between the store and this
-- model.
--
-- * The results are undefined, if this @type TreeViewColumn@ was not created
--   with the same @type TreeModel@ as the @type Association@s.
--
treeViewColumnAssociate :: CellRendererClass r => Renderer r -> 
						  [Association r] -> IO ()
treeViewColumnAssociate (Renderer ren  tvc) assocs = 
  mapM_ (\(Association attr col) ->
    Gtk.treeViewColumnAddAttribute tvc ren attr col) assocs

-- TreePath: A casual way of addressing nodes in a hierarchical structure. 
-- (EXPORTED)
--
type TreePath = [Int]

-- Retreive non-abstract position of a node in a @TreeList/@TreeStore. 
-- (EXPORTED)
--
treeModelGetIter :: TreeModelClass tm => tm -> TreePath -> IO (Maybe TreeIter)
treeModelGetIter _  [] = throw $ AssertionFailed "Mogul.treeModelGetIter: \
			 \a path must contain at least one element."
treeModelGetIter tm tp = do
  realPath <- Gtk.treePathNew
  mapM_ (Gtk.treePathAppendIndex realPath) tp
  Gtk.treeModelGetIter tm realPath

treeModelGetPath :: TreeModelClass tm => tm -> TreeIter -> IO TreePath
treeModelGetPath tm ti = do
  realPath <- Gtk.treeModelGetPath tm ti
  Gtk.treePathGetIndices realPath
