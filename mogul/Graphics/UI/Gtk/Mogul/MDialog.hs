-- -*-haskell-*-
--  The Monad GUI Library (Mogul): Dialog helper functions.
--
--  Author : Axel Simon
--
--  Created: 2 June 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/05/20 23:54:00 $
--
--  Copyright (C) 2001 Axel Simon
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
--
-- Simplify the construction of a non-modal dialog.
--

module Graphics.UI.Gtk.Mogul.MDialog(
  assureDialog
  ) where

import Graphics.UI.Gtk.Mogul.GetWidget		(getDialog)
import Graphics.UI.Gtk.Mogul.NewWidget		(newNamedDialog)
import Graphics.UI.Gtk.Mogul.WidgetTable	(isValidName)
import Graphics.UI.Gtk			(widgetShowAll, widgetGrabFocus)
import Graphics.UI.Gtk.Types

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

