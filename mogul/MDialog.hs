-- -*-haskell-*-
--  The Monad GUI Library (Mogul): Dialog helper functions.
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
-- * Simplify the construction of a non-modal dialog.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module MDialog(
  assureDialog
  ) where

import GetWidget	(getDialog)
import NewWidget	(newNamedDialog)
import WidgetTable	(isValidName)
import Gtk		(widgetShowAll, widgetGrabFocus)
import Hierarchy

-- Move a dialog to the toplevel if it is already onscreen. Otherwise construct
-- a new dialog with the given construction function. (EXPORTED)
--
assureDialog :: String -> (Dialog -> IO ()) -> (Dialog -> IO ()) -> IO ()
assureDialog name construct fill = do
  vn <- isValidName name
  if vn then do
    dia <- getDialog name
    fill dia
    -- FIXME: focus the window
    return ()
   else do
    dia <- newNamedDialog name
    construct dia
    fill dia
    widgetShowAll dia





