-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget FontSelection
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
-- A widget for selecting fonts.
--
module Graphics.UI.Gtk.Selectors.FontSelection (
  fontSelectionNew,
  fontSelectionGetFontName,
  fontSelectionSetFontName,
  fontSelectionGetPreviewText,
  fontSelectionSetPreviewText,
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}


-- | Creates a new 'FontSelection'.
--
fontSelectionNew :: IO FontSelection
fontSelectionNew =
  makeNewObject mkFontSelection $ liftM castPtr $
  {#call unsafe font_selection_new#}

-- | Gets the currently-selected font name. Returns Nothing if no font is
-- selected.
--
fontSelectionGetFontName :: FontSelectionClass obj => obj -> IO (Maybe String)
fontSelectionGetFontName obj =
  {#call unsafe font_selection_get_font_name#} (toFontSelection obj)
    >>= maybePeek readUTFString

-- | Sets the currently-selected font. Returns False if the font was not found.
--
fontSelectionSetFontName :: FontSelectionClass obj => obj -> String -> IO Bool
fontSelectionSetFontName obj fontname = liftM toBool $
  withUTFString fontname $ \strPtr ->
  {#call font_selection_set_font_name#} (toFontSelection obj) strPtr

-- | Gets the text displayed in the preview area.
--
fontSelectionGetPreviewText :: FontSelectionClass obj => obj -> IO String
fontSelectionGetPreviewText obj =
  {#call unsafe font_selection_get_preview_text#} (toFontSelection obj)
    >>= peekUTFString

-- | Sets the text displayed in the preview area.
--
fontSelectionSetPreviewText :: FontSelectionClass obj => obj -> String -> IO ()
fontSelectionSetPreviewText obj text =
  withUTFString text $ \strPtr ->
  {#call font_selection_set_preview_text#} (toFontSelection obj) strPtr
