-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: CellRenderer
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
--
--  Copyright (c) [1999.2001] Manuel Chakravarty, Axel Simon
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
-- * A @CellRenderer is an object that determines how the cell of a @TreeView
--   widget is displayed. Each @TreeViewColumn has exactly one accociated
--   @CellRenderer. The data supply for a cell is contained in a @TreeStore
--   or a @ListStore (both subclasses of @TreeModel). Each @CellRenderer may
--   have several attributes. Each @Attribute is associated with one column
--   of the @TreeModel database. Thus several columns of a @TreeModel may be
--   the supply for one @TreeViewColumn.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module CellRenderer(
  CellRenderer,
  CellRendererClass,
  Attribute(..),
  ) where

import Hierarchy
import StoreValue	(GenericValue, TMType)

-- Definition of the @Attribute data type. (EXPORTED)
--
-- * Each @CellRenderer defines a set of attributes. They are used by the
--   Mogul layer to generate columns in a @TreeStore or @ListStore.
--
data CellRendererClass cr => Attribute cr a 
  = AttrSingle String TMType 
    (a -> IO GenericValue)
    (GenericValue -> IO a)
  | AttrDouble String TMType String TMType 
    (a -> IO (GenericValue,GenericValue))
    (GenericValue -> GenericValue -> IO a)
