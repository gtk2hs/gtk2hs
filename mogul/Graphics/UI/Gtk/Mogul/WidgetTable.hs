-- -*-haskell-*-
--  The Monad GUI Library (Mogul): The global widget table.
--
--  Author : Axel Simon
--          
--  Created: 2 June 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/01/16 21:32:32 $
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
-- This module provides the possibility to lookup a widget by name.
--
-- * This module uses a global variable (unsafePerformIO is lurking).
--

module Graphics.UI.Gtk.Mogul.WidgetTable (
  WidgetName,
  widgetLookup,
  newNamedWidget,
  isValidName
  ) where

import Monad	(liftM)
import Control.Concurrent.MVar	(MVar, newMVar, takeMVar, putMVar, readMVar)
import System.IO.Unsafe		(unsafePerformIO)
import Data.FiniteMap		(FiniteMap, emptyFM, addToFM, delFromFM,
				lookupFM, elemFM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
import Graphics.UI.Gtk.Types
import Graphics.UI.Gtk.Abstract.Widget	(widgetSetName, onUnrealize)

-- We define a synonym for the name of a widget. (EXPORTED)
--
type WidgetName = String

-- Eventually we might use hash values in addition to strings. Provide this
-- functionality today.
--
newtype WidgetId = WidgetId String
		   deriving (Eq,Ord)

mkWidgetId :: WidgetName -> WidgetId
mkWidgetId = WidgetId

{-# NOINLINE widgetTable #-}
widgetTable :: MVar (FiniteMap WidgetId (Ptr Widget))
widgetTable = unsafePerformIO $ newMVar emptyFM

-- Retrieve a widget from the global store. The second argument should
-- contain the type name of the object to generate a more meaningful error
-- message.
--
widgetLookup :: WidgetClass w => 
  WidgetName -> String -> (ForeignPtr w -> w) -> IO w
widgetLookup name oType mkObj = do
  table <- readMVar widgetTable
  case table `lookupFM` (mkWidgetId name) of
    Nothing -> error ("fetch"++oType++": "++name++" could not be found.")
    Just w -> makeNewObject mkObj $ return (castPtr w)

-- Create a widget and store its name in the table.
--
newNamedWidget :: WidgetClass w => WidgetName -> IO w -> IO w
newNamedWidget name new = do
  w <- new
  widgetSetName w name
  let wId = (mkWidgetId name)
  table <- takeMVar widgetTable
  putMVar widgetTable (addToFM table wId
    ((foreignPtrToPtr.unWidget.toWidget) w))
  w `onUnrealize` (do
    table <- takeMVar widgetTable
    putMVar widgetTable (table `delFromFM` wId))
  return w

-- Check if a given name is contained in the table. (EXPORTED)
--
isValidName :: WidgetName -> IO Bool
isValidName name = liftM (elemFM (mkWidgetId name)) $ readMVar widgetTable


