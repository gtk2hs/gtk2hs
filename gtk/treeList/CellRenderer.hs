-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry CellRenderer TreeView@
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.5 $ from $Date: 2002/08/05 16:41:34 $
--
--  Copyright (c) 1999..2002 Axel Simon
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
-- @description@ --------------------------------------------------------------
--
-- * A @ref data CellRenderer@ is an object that determines how the cell of a
--   @ref data TreeView@
--   widget is displayed. Each @ref data TreeViewColumn@
--    has exactly one accociated
--   @ref data CellRenderer@. The data supply for a cell is contained in a 
--   @ref data TreeStore@
--   or a @ref data ListStore@ (both subclasses of @ref data TreeModel@).
--   Each @ref data CellRenderer@ may
--   have several attributes. Each @ref data Attribute@ is associated with 
--   one column
--   of the @ref data TreeModel@ database. Thus several columns of a 
--   @ref data TreeModel@ may be
--   the supply for one @ref data TreeViewColumn@.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

module CellRenderer(
  CellRenderer,
  CellRendererClass,
  Attribute(..),
  ) where

import Hierarchy
import StoreValue	(GenericValue, TMType)

-- @data CellRendererClass@ Definition of the @ref arg Attribute@ data type.
--
-- * Each @ref type CellRenderer@ defines a set of attributes. They are used
--   by the Mogul layer to generate columns in a @ref data TreeStore@ or
--   @ref data ListStore@.
--
data CellRendererClass cr => Attribute cr a = Attribute String TMType 
					      (a -> IO GenericValue) 
					      (GenericValue -> IO a)

