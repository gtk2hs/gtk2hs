-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget FontSelectionDialog
--
--  Author : Duncan Coutts
--
--  Created: 2 August 2004
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/12 17:19:25 $
--
--  Copyright (C) 2004-2005 Duncan Coutts
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
-- A dialog box for selecting fonts.
--
module Graphics.UI.Gtk.Selectors.FontSelectionDialog (
  fontSelectionDialogNew,
  fontSelectionDialogGetFontName,
  fontSelectionDialogSetFontName,
  fontSelectionDialogGetPreviewText,
  fontSelectionDialogSetPreviewText,
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}


-- | Creates a new 'FontSelectionDialog'.
--
fontSelectionDialogNew :: String -> IO FontSelectionDialog
fontSelectionDialogNew title =
  makeNewObject mkFontSelectionDialog $ liftM castPtr $
  withUTFString title $ \strPtr ->
  {#call unsafe font_selection_dialog_new#} strPtr

-- | Gets the currently-selected font name. Returns Nothing if no font is
-- selected.
--
fontSelectionDialogGetFontName :: FontSelectionDialogClass obj => obj
                               -> IO (Maybe String)
fontSelectionDialogGetFontName obj =
  {#call font_selection_dialog_get_font_name#} (toFontSelectionDialog obj)
    >>= maybePeek readUTFString

-- | Sets the currently-selected font. Returns False if the font was not found.
--
fontSelectionDialogSetFontName :: FontSelectionDialogClass obj => obj -> String -> IO Bool
fontSelectionDialogSetFontName obj fontname = liftM toBool $
  withUTFString fontname $ \strPtr ->
  {#call font_selection_dialog_set_font_name#} (toFontSelectionDialog obj)
    strPtr

-- | Gets the text displayed in the preview area.
--
fontSelectionDialogGetPreviewText :: FontSelectionDialogClass obj => obj
                                  -> IO String
fontSelectionDialogGetPreviewText obj =
  {#call unsafe font_selection_dialog_get_preview_text#} (toFontSelectionDialog obj)
    >>= peekUTFString

-- | Sets the text displayed in the preview area.
--
fontSelectionDialogSetPreviewText :: FontSelectionDialogClass obj => obj
                                  -> String -> IO ()
fontSelectionDialogSetPreviewText obj text =
  withUTFString text $ \strPtr ->
  {#call font_selection_dialog_set_preview_text#} (toFontSelectionDialog obj)
    strPtr
