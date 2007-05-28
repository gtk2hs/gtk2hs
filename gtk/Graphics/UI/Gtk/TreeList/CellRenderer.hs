-- -*-haskell-*-
--  GIMP Toolkit (GTK) CellRenderer TreeView
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.7 $ from $Date: 2005/10/19 12:57:37 $
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
-- A 'CellRenderer' is an object that determines how the cell of a
-- 'TreeView' widget is displayed. 
--
-- * Each 'TreeViewColumn' has exactly one accociated 'CellRenderer'.
--   The data supply for a cell is contained in a 'TreeStore' or a
--   'ListStore' (both subclasses of 'TreeModel'). Each 'CellRenderer'
--   may have several attributes. Each 'Attribute' is associated with 
--   one column of the 'TreeModel' database. Thus several columns of a 
--   'TreeModel' may be the supply for one 'TreeViewColumn'.
--

module Graphics.UI.Gtk.TreeList.CellRenderer (
-- * Detail
-- 
-- | The 'CellRenderer' is a base class of a set of objects used for rendering
-- a cell to a 'Drawable'. These objects are used primarily by the 'TreeView'
-- widget, though they aren't tied to them in any specific way. It is worth
-- noting that 'CellRenderer' is not a 'Widget' and cannot be treated as such.
--
-- The primary use of a 'CellRenderer' is for drawing a certain graphical
-- elements on a 'Drawable'. Typically, one cell renderer is used to draw many
-- cells on the screen. To this extent, it isn't expected that a CellRenderer
-- keep any permanent state around. Instead, any state is set just prior to use
-- using 'GObject's property system. Then, the cell is measured using
-- 'cellRendererGetSize'. Finally, the cell is rendered in the correct location
-- using 'cellRendererRender'.
--
-- Beyond merely rendering a cell, cell renderers can optionally provide
-- active user interface elements. A cell renderer can be activatable like
-- 'CellRendererToggle', which toggles when it gets activated by a mouse click,
-- or it can be editable like 'CellRendererText', which allows the user to edit
-- the text using a 'Entry'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----CellRenderer
-- |               +----'CellRendererText'
-- |               +----'CellRendererPixbuf'
-- |               +----'CellRendererProgress'
-- |               +----'CellRendererToggle'
-- @

-- * Types
  CellRenderer,
  CellRendererClass,
  castToCellRenderer,
  toCellRenderer,
  Attribute(..),

-- * Methods
  cellRendererSet,
  cellRendererGet
  ) where

import Control.Monad (zipWithM)

import Graphics.UI.Gtk.Types
import System.Glib.StoreValue	(GenericValue, TMType,
                                 valueSetGenericValue, valueGetGenericValue)
import System.Glib.Properties	(objectSetPropertyInternal,
                                 objectGetPropertyInternal)

-- | Definition of the 'Attribute' data type.
--
-- * Each 'CellRenderer' defines a set of attributes. They are used
--   by the Mogul layer to generate columns in a 'TreeStore' or
--   'ListStore'.
--
data CellRendererClass cr => Attribute cr a = Attribute [String] [TMType]
					      (a -> IO [GenericValue]) 
					      ([GenericValue] -> IO a)

-- | Set a property statically.
--
-- * Instead of using a 'TreeStore' or 'ListStore' to set
--   properties of a 'CellRenderer' this method allows to set such
--   a property for the whole column.
--
cellRendererSet :: CellRendererClass cr => 
		   cr -> Attribute cr val -> val -> IO ()
cellRendererSet cr (Attribute names types write _) val = do
  values <- write val
  sequence_ $ zipWith3 (\name tmtype value ->
                            objectSetPropertyInternal
                              (fromIntegral $ fromEnum tmtype)
                              valueSetGenericValue name cr value)
                names types values

-- | Get a static property.
--
-- * See 'cellRendererSet'. Note that calling this function on a
--   property of a 'CellRenderer' object which retrieves its values
--   from a 'ListStore' or 'TreeStore' will result in an
--   abitrary value.
--
cellRendererGet :: CellRendererClass cr =>
		   cr -> Attribute cr val -> IO val
cellRendererGet cr (Attribute names types _ read) = do
  values <- zipWithM (\name tmtype ->
                         objectGetPropertyInternal
                           (fromIntegral $ fromEnum tmtype)
                           valueGetGenericValue name cr)
              names types
  read values
