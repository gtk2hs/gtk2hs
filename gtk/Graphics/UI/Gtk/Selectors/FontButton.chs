{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget FontButton
--
--  Author : Duncan Coutts
--
--  Created: 5 April 2005
--
--  Copyright (C) 2005 Duncan Coutts
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
-- A button to launch a font selection dialog
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.Selectors.FontButton (
-- * Detail
--
-- | The 'FontButton' is a button which displays the currently selected font
-- an allows to open a font selection dialog to change the font. It is suitable
-- widget for selecting a font in a preference dialog.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'Button'
-- |                                 +----FontButton
-- @

#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  FontButton,
  FontButtonClass,
  castToFontButton, gTypeFontButton,
  toFontButton,

-- * Constructors
  fontButtonNew,
  fontButtonNewWithFont,

-- * Methods
  fontButtonSetFontName,
  fontButtonGetFontName,
  fontButtonSetShowStyle,
  fontButtonGetShowStyle,
  fontButtonSetShowSize,
  fontButtonGetShowSize,
  fontButtonSetUseFont,
  fontButtonGetUseFont,
  fontButtonSetUseSize,
  fontButtonGetUseSize,
  fontButtonSetTitle,
  fontButtonGetTitle,

-- * Attributes
  fontButtonTitle,
  fontButtonFontName,
  fontButtonUseFont,
  fontButtonUseSize,
  fontButtonShowStyle,
  fontButtonShowSize,

-- * Signals
  onFontSet,
  afterFontSet,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,4,0)
--------------------
-- Constructors

-- | Creates a new font picker widget.
--
fontButtonNew :: IO FontButton
fontButtonNew =
  makeNewObject mkFontButton $
  liftM (castPtr :: Ptr Widget -> Ptr FontButton) $
  {# call gtk_font_button_new #}

-- | Creates a new font picker widget.
--
fontButtonNewWithFont :: GlibString string
 => string        -- ^ @fontname@ - Name of font to display in font selection
                  -- dialog
 -> IO FontButton
fontButtonNewWithFont fontname =
  makeNewObject mkFontButton $
  liftM (castPtr :: Ptr Widget -> Ptr FontButton) $
  withUTFString fontname $ \fontnamePtr ->
  {# call gtk_font_button_new_with_font #}
    fontnamePtr

--------------------
-- Methods

-- | Sets or updates the currently-displayed font in font picker dialog.
--
fontButtonSetFontName :: (FontButtonClass self, GlibString string) => self
 -> string  -- ^ @fontname@ - Name of font to display in font selection dialog
 -> IO Bool -- ^ returns Return value of 'Graphics.UI.Gtk.Selectors.FontSelectionDialog.fontSelectionDialogSetFontName' if
            -- the font selection dialog exists, otherwise @False@.
fontButtonSetFontName self fontname =
  liftM toBool $
  withUTFString fontname $ \fontnamePtr ->
  {# call gtk_font_button_set_font_name #}
    (toFontButton self)
    fontnamePtr

-- | Retrieves the name of the currently selected font.
--
fontButtonGetFontName :: (FontButtonClass self, GlibString string) => self
 -> IO string -- ^ returns an internal copy of the font name which must not be
              -- freed.
fontButtonGetFontName self =
  {# call gtk_font_button_get_font_name #}
    (toFontButton self)
  >>= peekUTFString

-- | If @showStyle@ is @True@, the font style will be displayed along with
-- name of the selected font.
--
fontButtonSetShowStyle :: FontButtonClass self => self
 -> Bool  -- ^ @showStyle@ - @True@ if font style should be displayed in
          -- label.
 -> IO ()
fontButtonSetShowStyle self showStyle =
  {# call gtk_font_button_set_show_style #}
    (toFontButton self)
    (fromBool showStyle)

-- | Returns whether the name of the font style will be shown in the label.
--
fontButtonGetShowStyle :: FontButtonClass self => self
 -> IO Bool -- ^ returns whether the font style will be shown in the label.
fontButtonGetShowStyle self =
  liftM toBool $
  {# call gtk_font_button_get_show_style #}
    (toFontButton self)

-- | If @showSize@ is @True@, the font size will be displayed along with the
-- name of the selected font.
--
fontButtonSetShowSize :: FontButtonClass self => self
 -> Bool  -- ^ @showSize@ - @True@ if font size should be displayed in dialog.
 -> IO ()
fontButtonSetShowSize self showSize =
  {# call gtk_font_button_set_show_size #}
    (toFontButton self)
    (fromBool showSize)

-- | Returns whether the font size will be shown in the label.
--
fontButtonGetShowSize :: FontButtonClass self => self
 -> IO Bool -- ^ returns whether the font size will be shown in the label.
fontButtonGetShowSize self =
  liftM toBool $
  {# call gtk_font_button_get_show_size #}
    (toFontButton self)

-- | If @useFont@ is @True@, the font name will be written using the selected
-- font.
--
fontButtonSetUseFont :: FontButtonClass self => self
 -> Bool  -- ^ @useFont@ - If @True@, font name will be written using font
          -- chosen.
 -> IO ()
fontButtonSetUseFont self useFont =
  {# call gtk_font_button_set_use_font #}
    (toFontButton self)
    (fromBool useFont)

-- | Returns whether the selected font is used in the label.
--
fontButtonGetUseFont :: FontButtonClass self => self
 -> IO Bool -- ^ returns whether the selected font is used in the label.
fontButtonGetUseFont self =
  liftM toBool $
  {# call gtk_font_button_get_use_font #}
    (toFontButton self)

-- | If @useSize@ is @True@, the font name will be written using the selected
-- size.
--
fontButtonSetUseSize :: FontButtonClass self => self
 -> Bool  -- ^ @useSize@ - If @True@, font name will be written using the
          -- selected size.
 -> IO ()
fontButtonSetUseSize self useSize =
  {# call gtk_font_button_set_use_size #}
    (toFontButton self)
    (fromBool useSize)

-- | Returns whether the selected size is used in the label.
--
fontButtonGetUseSize :: FontButtonClass self => self
 -> IO Bool -- ^ returns whether the selected size is used in the label.
fontButtonGetUseSize self =
  liftM toBool $
  {# call gtk_font_button_get_use_size #}
    (toFontButton self)

-- | Sets the title for the font selection dialog.
--
fontButtonSetTitle :: (FontButtonClass self, GlibString string) => self
 -> string -- ^ @title@ - a string containing the font selection dialog title
 -> IO ()
fontButtonSetTitle self title =
  withUTFString title $ \titlePtr ->
  {# call gtk_font_button_set_title #}
    (toFontButton self)
    titlePtr

-- | Retrieves the title of the font selection dialog.
--
fontButtonGetTitle :: (FontButtonClass self, GlibString string) => self
 -> IO string -- ^ returns an internal copy of the title string which must not
              -- be freed.
fontButtonGetTitle self =
  {# call gtk_font_button_get_title #}
    (toFontButton self)
  >>= peekUTFString

--------------------
-- Attributes

-- | The title of the font selection dialog.
--
-- Default value: \"Pick a Font\"
--
fontButtonTitle :: (FontButtonClass self, GlibString string) => Attr self string
fontButtonTitle = newAttr
  fontButtonGetTitle
  fontButtonSetTitle

-- | The name of the currently selected font.
--
-- Default value: \"Sans 12\"
--
fontButtonFontName :: (FontButtonClass self, GlibString string) => Attr self string
fontButtonFontName = newAttrFromStringProperty "font-name"

-- | If this property is set to @True@, the label will be drawn in the
-- selected font.
--
-- Default value: @False@
--
fontButtonUseFont :: FontButtonClass self => Attr self Bool
fontButtonUseFont = newAttr
  fontButtonGetUseFont
  fontButtonSetUseFont

-- | If this property is set to @True@, the label will be drawn with the
-- selected font size.
--
-- Default value: @False@
--
fontButtonUseSize :: FontButtonClass self => Attr self Bool
fontButtonUseSize = newAttr
  fontButtonGetUseSize
  fontButtonSetUseSize

-- | If this property is set to @True@, the name of the selected font style
-- will be shown in the label. For a more WYSIWIG way to show the selected
-- style, see the ::use-font property.
--
-- Default value: @True@
--
fontButtonShowStyle :: FontButtonClass self => Attr self Bool
fontButtonShowStyle = newAttr
  fontButtonGetShowStyle
  fontButtonSetShowStyle

-- | If this property is set to @True@, the selected font size will be shown
-- in the label. For a more WYSIWIG way to show the selected size, see the
-- ::use-size property.
--
-- Default value: @True@
--
fontButtonShowSize :: FontButtonClass self => Attr self Bool
fontButtonShowSize = newAttr
  fontButtonGetShowSize
  fontButtonSetShowSize

--------------------
-- Signals

-- | The 'fontSet' signal is emitted when the user selects a font. When
-- handling this signal, use 'fontButtonGetFontName' to find out which font was
-- just selected.
--
onFontSet, afterFontSet :: FontButtonClass self => self
 -> IO ()
 -> IO (ConnectId self)
onFontSet = connect_NONE__NONE "font-set" False
afterFontSet = connect_NONE__NONE "font-set" True
#endif
