-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget ColorSelectionDialog
--
--  Author : Duncan Coutts
--  Created: 2 August 2004
--
--  Copyright (c) 2004 Duncan Coutts
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
-- |
--

module Graphics.UI.Gtk.Selectors.ColorSelectionDialog  (
  colorSelectionDialogNew,
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}


-- | Creates a new 'ColorSelectionDialog'.
--
colorSelectionDialogNew :: String -> IO ColorSelectionDialog
colorSelectionDialogNew title =
  makeNewObject mkColorSelectionDialog $ liftM castPtr $
  withUTFString title $ \strPtr ->
  {#call unsafe color_selection_dialog_new#} strPtr
