{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget FontSelectionDialog
--
--  Author : Duncan Coutts
--
--  Created: 2 August 2004
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
-- A dialog box for selecting fonts
--
module Graphics.UI.Gtk.Selectors.FontSelectionDialog (
-- * Detail
--
-- | The 'FontSelectionDialog' widget is a dialog box for selecting a font.
--
-- To set the font which is initially selected, use
-- 'fontSelectionDialogSetFontName'.
--
-- To get the selected font use 'fontSelectionDialogGetFontName'.
--
-- To change the text which is shown in the preview area, use
-- 'fontSelectionDialogSetPreviewText'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'Window'
-- |                                 +----'Dialog'
-- |                                       +----FontSelectionDialog
-- @

-- * Types
  FontSelectionDialog,
  FontSelectionDialogClass,
  castToFontSelectionDialog, gTypeFontSelectionDialog,
  toFontSelectionDialog,

-- * Constructors
  fontSelectionDialogNew,

-- * Methods
  fontSelectionDialogGetFontName,
  fontSelectionDialogSetFontName,
  fontSelectionDialogGetPreviewText,
  fontSelectionDialogSetPreviewText,
#if GTK_CHECK_VERSION(2,14,0)
  fontSelectionDialogGetCancelButton,
  fontSelectionDialogGetOkButton,
#endif
#if GTK_CHECK_VERSION(2,22,0)
  fontSelectionDialogGetFontSelection,
#endif

-- * Attributes
  fontSelectionDialogPreviewText,
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'FontSelectionDialog'.
--
fontSelectionDialogNew :: GlibString string
 => string                 -- ^ @title@ - the title of the dialog box.
 -> IO FontSelectionDialog
fontSelectionDialogNew title =
  makeNewObject mkFontSelectionDialog $
  liftM (castPtr :: Ptr Widget -> Ptr FontSelectionDialog) $
  withUTFString title $ \titlePtr ->
  {# call unsafe font_selection_dialog_new #}
    titlePtr

--------------------
-- Methods

-- | Gets the currently-selected font name.
--
fontSelectionDialogGetFontName :: (FontSelectionDialogClass self, GlibString string) => self
 -> IO (Maybe string) -- ^ returns the currently-selected font name, or
                      -- @Nothing@ if no font is selected.
fontSelectionDialogGetFontName self =
  {# call font_selection_dialog_get_font_name #}
    (toFontSelectionDialog self)
  >>= maybePeek readUTFString

-- | Sets the currently-selected font.
--
fontSelectionDialogSetFontName :: (FontSelectionDialogClass self, GlibString string) => self
 -> string  -- ^ @fontname@ - a fontname.
 -> IO Bool -- ^ returns @True@ if the font was found.
fontSelectionDialogSetFontName self fontname =
  liftM toBool $
  withUTFString fontname $ \fontnamePtr ->
  {# call font_selection_dialog_set_font_name #}
    (toFontSelectionDialog self)
    fontnamePtr

-- | Gets the text displayed in the preview area.
--
fontSelectionDialogGetPreviewText :: (FontSelectionDialogClass self, GlibString string) => self -> IO string
fontSelectionDialogGetPreviewText self =
  {# call unsafe font_selection_dialog_get_preview_text #}
    (toFontSelectionDialog self)
  >>= peekUTFString

-- | Sets the text displayed in the preview area.
--
fontSelectionDialogSetPreviewText :: (FontSelectionDialogClass self, GlibString string) => self -> string -> IO ()
fontSelectionDialogSetPreviewText self text =
  withUTFString text $ \textPtr ->
  {# call font_selection_dialog_set_preview_text #}
    (toFontSelectionDialog self)
    textPtr

#if GTK_CHECK_VERSION(2,14,0)
-- | Gets the 'Cancel' button.
--
-- * Available since Gtk+ version 2.14
--
fontSelectionDialogGetCancelButton :: FontSelectionDialogClass self => self
                                    -> IO Widget -- ^ returns the 'Widget' used in the dialog for the 'Cancel' button.
fontSelectionDialogGetCancelButton self =
  makeNewObject mkWidget $
  {#call gtk_font_selection_dialog_get_cancel_button #}
     (toFontSelectionDialog self)

-- | Gets the 'OK' button.
--
-- * Available since Gtk+ version 2.14
--
fontSelectionDialogGetOkButton :: FontSelectionDialogClass self => self
                               -> IO Widget -- ^ returns the 'Widget' used in the dialog for the 'OK' button.
fontSelectionDialogGetOkButton self =
  makeNewObject mkWidget $
  {#call gtk_font_selection_dialog_get_ok_button #}
     (toFontSelectionDialog self)

#endif

#if GTK_CHECK_VERSION(2,22,0)
-- | Retrieves the 'FontSelection' widget embedded in the dialog.
--
-- * Available since Gtk+ version 2.22
--
fontSelectionDialogGetFontSelection :: FontSelectionDialogClass self => self
                                    -> IO FontSelection -- ^ returns the embedded 'FontSelection'
fontSelectionDialogGetFontSelection self =
  makeNewObject mkFontSelection $
  liftM (castPtr :: Ptr Widget -> Ptr FontSelection) $
  {#call gtk_font_selection_dialog_get_font_selection #}
     (toFontSelectionDialog self)
#endif

--------------------
-- Attributes

-- | \'previewText\' property. See 'fontSelectionDialogGetPreviewText' and
-- 'fontSelectionDialogSetPreviewText'
--
fontSelectionDialogPreviewText :: (FontSelectionDialogClass self, GlibString string) => Attr self string
fontSelectionDialogPreviewText = newAttr
  fontSelectionDialogGetPreviewText
  fontSelectionDialogSetPreviewText
