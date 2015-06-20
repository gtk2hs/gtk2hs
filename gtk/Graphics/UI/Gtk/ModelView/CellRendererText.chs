{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) CellRendererText TreeView
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Copyright (C) 1999-2006 Axel Simon
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
-- A 'CellRenderer' which displays a single-line text.
--
module Graphics.UI.Gtk.ModelView.CellRendererText (
-- * Detail
--
-- | A 'CellRendererText' renders a given text in its cell, using the font,
-- color and style information provided by its attributes. The text will be
-- ellipsized if it is too long and the ellipsize property allows it.
--
-- If the 'cellMode' is 'CellRendererModeEditable', the 'CellRendererText'
-- allows the user to edit its text using an 'Entry' widget.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'CellRenderer'
-- |               +----CellRendererText
-- |                     +----'CellRendererCombo'
-- @

-- * Types
  CellRendererText,
  CellRendererTextClass,
  castToCellRendererText, gTypeCellRendererText,
  toCellRendererText,

-- * Constructors
  cellRendererTextNew,

-- * Methods
  cellRendererTextSetFixedHeightFromFont,

-- * Attributes
  cellText,
  cellTextMarkup,
  --cellTextAttributes,
  cellTextSingleParagraphMode,
  cellTextBackground,
  cellTextBackgroundColor,
  cellTextBackgroundSet,
  cellTextForeground,
  cellTextForegroundColor,
  cellTextForegroundSet,
  cellTextEditable,
  cellTextEditableSet,
  cellTextFont,
  cellTextFontDesc,
  cellTextFamily,
  cellTextFamilySet,
  cellTextStyle,
  cellTextStyleSet,
  cellTextVariant,
  cellTextVariantSet,
  cellTextWeight,
  cellTextWeightSet,
  cellTextStretch,
  cellTextStretchSet,
  cellTextSize,
  cellTextSizePoints,
  cellTextSizeSet,
  cellTextScale,
  cellTextScaleSet,
  cellTextRise,
  cellTextRiseSet,
  cellTextStrikethrough,
  cellTextStrikethroughSet,
  cellTextUnderline,
  cellTextUnderlineSet,
  cellTextLanguage,
  cellTextLanguageSet,
#if GTK_CHECK_VERSION(2,6,0)
  cellTextEllipsize,
  cellTextEllipsizeSet,
  cellTextWidthChars,
#endif
#if GTK_CHECK_VERSION(2,8,0)
  cellTextWrapMode,
  cellTextWrapWidth,
#endif
#if GTK_CHECK_VERSION(2,10,0)
  cellTextAlignment,
#endif

-- * Signals
  edited,

-- * Deprecated
#ifndef DISABLE_DEPRECATED
  onEdited,
  afterEdited
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Properties
import System.Glib.Attributes (Attr, WriteAttr)
import Graphics.UI.Gtk.Abstract.Object          (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.ModelView.Types#}
import Graphics.UI.Gtk.General.Structs          ()
import Graphics.Rendering.Pango.Enums
{#import Graphics.Rendering.Pango.BasicTypes#} ( FontDescription(..),
                                         makeNewFontDescription )
{#import Graphics.Rendering.Pango.Layout#}      ( LayoutAlignment, LayoutWrapMode )

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new CellRendererText object.
--
cellRendererTextNew :: IO CellRendererText
cellRendererTextNew =
  makeNewObject mkCellRendererText $
  liftM (castPtr :: Ptr CellRenderer -> Ptr CellRendererText) $
  {# call unsafe cell_renderer_text_new #}

--------------------
-- Methods

-- | Sets the height of a renderer to explicitly be determined by the
-- 'cellTextFont' and 'Graphics.UI.Gtk.ModelView.CellRenderer.cellYPad'
-- attribute set on it. Further changes in these properties do not affect the
-- height, so they must be accompanied by a subsequent call to this function.
-- Using this function is unflexible, and should really only be used if
-- calculating the size of a cell is too slow (ie, a massive number of cells
-- displayed). If @numberOfRows@ is -1, then the fixed height is unset, and
-- the height is determined by the properties again.
--
cellRendererTextSetFixedHeightFromFont :: CellRendererTextClass self => self
 -> Int   -- ^ @numberOfRows@ - Number of rows of text each cell renderer is
          -- allocated, or -1
 -> IO ()
cellRendererTextSetFixedHeightFromFont self numberOfRows =
  {# call gtk_cell_renderer_text_set_fixed_height_from_font #}
    (toCellRendererText self)
    (fromIntegral numberOfRows)


--------------------
-- Properties

-- | Text background color as a string.
--
-- Default value: @\"\"@
--
cellTextBackground :: (CellRendererClass self, GlibString string) => WriteAttr self string
cellTextBackground = writeAttrFromStringProperty "background"

-- | Text background color as a 'Color'.
--
cellTextBackgroundColor :: CellRendererClass self => Attr self Color
cellTextBackgroundColor = newAttrFromBoxedStorableProperty "background-gdk"
  {# call pure unsafe gdk_color_get_type #}

-- | Whether the 'cellTextBackground'\/'cellTextBackgroundColor' attribute is set.
--
-- Default value: @False@
--
cellTextBackgroundSet :: CellRendererClass self => Attr self Bool
cellTextBackgroundSet = newAttrFromBoolProperty "background-set"

-- | Whether the text can be modified by the user.
--
cellTextEditable :: CellRendererTextClass self => Attr self Bool
cellTextEditable = newAttrFromBoolProperty "editable"

-- | Whether the 'cellTextEditable' flag affects text editability.
--
cellTextEditableSet :: CellRendererTextClass self => Attr self Bool
cellTextEditableSet = newAttrFromBoolProperty "editable-set"

#if GTK_CHECK_VERSION(2,6,0)
-- | Specifies the preferred place to ellipsize the string, if the cell
--   renderer does not have enough room to display the entire string.
--   Setting it to 'Graphics.Rendering.Pango.Enums.EllipsizeNone' turns off
--   ellipsizing. See the 'cellTextWrapWidth' property for another way of
--   making the text fit into a given width.
--
-- * Available in Gtk 2.6 or higher.
--
cellTextEllipsize :: CellRendererTextClass self => Attr self EllipsizeMode
cellTextEllipsize = newAttrFromEnumProperty "ellipsize"
                {# call pure pango_ellipsize_mode_get_type #}

-- | Whether the 'cellTextEllipsize' tag affects the ellipsize mode.
--
-- * Available in Gtk 2.6 or higher.
--
cellTextEllipsizeSet :: CellRendererTextClass self => Attr self Bool
cellTextEllipsizeSet = newAttrFromBoolProperty "ellipsize-set"
#endif

-- | Name of the font family, e.g. Sans, Helvetica, Times, Monospace.
--
cellTextFamily :: (CellRendererTextClass self, GlibString string) => Attr self string
cellTextFamily = newAttrFromStringProperty "family"

-- | Determines if 'cellTextFamily' has an effect.
--
cellTextFamilySet :: CellRendererTextClass self => Attr self Bool
cellTextFamilySet = newAttrFromBoolProperty "family-set"

-- | Font description as a string.
--
cellTextFont :: (CellRendererTextClass self, GlibString string) => Attr self string
cellTextFont = newAttrFromStringProperty "font"

-- | Font description as a 'Graphics.Rendering.Pango.FontDescription'.
--
cellTextFontDesc :: CellRendererTextClass self => Attr self FontDescription
cellTextFontDesc = newAttrFromBoxedOpaqueProperty makeNewFontDescription
  (\(FontDescription fd) act -> withForeignPtr fd act) "font-desc"
  {# call pure unsafe pango_font_description_get_type #}

-- | Text foreground color as a string.
--
-- Default value: @\"\"@
--
cellTextForeground :: (CellRendererClass self, GlibString string) => WriteAttr self string
cellTextForeground = writeAttrFromStringProperty "foreground"

-- | Text foreground color as a 'Color'.
--
cellTextForegroundColor :: CellRendererClass self => Attr self Color
cellTextForegroundColor = newAttrFromBoxedStorableProperty "foreground-gdk"
  {# call pure unsafe gdk_color_get_type #}

-- | Whether the 'cellTextForeground'\/'cellTextForegroundColor' attribute is set.
--
-- Default value: @False@
--
cellTextForegroundSet :: CellRendererClass self => Attr self Bool
cellTextForegroundSet = newAttrFromBoolProperty "foreground-set"

-- | The language this text is in, as an ISO code. Pango can use this as
--   a hint when rendering the text. If you don't understand this parameter,
--   you probably don't need it.
--
cellTextLanguage :: (CellRendererTextClass self, GlibString string) => Attr self (Maybe string)
cellTextLanguage = newAttrFromMaybeStringProperty "language"

-- | Whether the 'cellTextLanguage' tag is used, default is @False@.
--
cellTextLanguageSet :: CellRendererTextClass self => Attr self Bool
cellTextLanguageSet = newAttrFromBoolProperty "language-set"

-- | Define a markup string instead of a text. See 'cellText'.
--
cellTextMarkup :: (CellRendererTextClass cr, GlibString string) => WriteAttr cr (Maybe string)
cellTextMarkup  = writeAttrFromMaybeStringProperty "markup"

-- %hash c:4e25 d:f7c6
-- | Offset of text above the baseline (below the baseline if rise is
--   negative).
--
-- Allowed values: >= -2147483647
--
-- Default value: 0
--
cellTextRise :: CellRendererTextClass self => Attr self Int
cellTextRise = newAttrFromIntProperty "rise"

-- | Whether the 'cellTextRise' tag is used, default is @False@.
--
cellTextRiseSet :: CellRendererTextClass self => Attr self Bool
cellTextRiseSet = newAttrFromBoolProperty "rise-set"

-- | Font scaling factor. Default is 1.
--
cellTextScale :: CellRendererTextClass self => Attr self Double
cellTextScale = newAttrFromDoubleProperty "scale"

-- | Whether the 'cellTextScale' tag is used, default is @False@.
--
cellTextScaleSet :: CellRendererTextClass self => Attr self Bool
cellTextScaleSet = newAttrFromBoolProperty "scale-set"

-- %hash c:d85f d:9cfb
-- | Whether or not to keep all text in a single paragraph.
--
-- Default value: @False@
--
cellTextSingleParagraphMode :: CellRendererTextClass self => Attr self Bool
cellTextSingleParagraphMode = newAttrFromBoolProperty "single-paragraph-mode"

-- | Font size in points.
--
cellTextSize :: CellRendererTextClass self => Attr self Double
cellTextSize = newAttrFromDoubleProperty "size-points"

-- %hash c:d281 d:3b0c
-- | Font size in points.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
cellTextSizePoints :: CellRendererTextClass self => Attr self Double
cellTextSizePoints = newAttrFromDoubleProperty "size-points"

-- | Whether the 'cellTextSize' tag is used, default is @False@.
--
cellTextSizeSet :: CellRendererTextClass self => Attr self Bool
cellTextSizeSet = newAttrFromBoolProperty "size-set"

-- | Font stretch.
--
cellTextStretch :: CellRendererTextClass self => Attr self Stretch
cellTextStretch = newAttrFromEnumProperty "stretch"
              {# call pure pango_stretch_get_type #}

-- | Whether the 'cellTextStretch' tag is used, default is @False@.
--
cellTextStretchSet :: CellRendererTextClass self => Attr self Bool
cellTextStretchSet = newAttrFromBoolProperty "stretch-set"

-- | Whether to strike through the text.
--
cellTextStrikethrough :: CellRendererTextClass self => Attr self Bool
cellTextStrikethrough = newAttrFromBoolProperty "strikethrough"

-- | Whether the 'cellTextStrikethrough' tag is used, default is @False@.
--
cellTextStrikethroughSet :: CellRendererTextClass self => Attr self Bool
cellTextStrikethroughSet = newAttrFromBoolProperty "strikethrough-set"

-- | Font style (e.g. normal or italics).
--
cellTextStyle :: CellRendererTextClass self => Attr self FontStyle
cellTextStyle = newAttrFromEnumProperty "style"
            {# call pure pango_style_get_type #}

-- | Whether the 'cellTextStyle' tag is used, default is @False@.
--
cellTextStyleSet :: CellRendererTextClass self => Attr self Bool
cellTextStyleSet = newAttrFromBoolProperty "style-set"

-- | Define the attribute that specifies the text to be rendered. See
--   also 'cellTextMarkup'.
--
cellText :: (CellRendererTextClass cr, GlibString string) => Attr cr string
cellText  = newAttrFromStringProperty "text"

-- | Style of underline for this text.
--
cellTextUnderline :: CellRendererTextClass self => Attr self Underline
cellTextUnderline = newAttrFromEnumProperty "underline"
                {# call pure pango_underline_get_type #}

-- | Whether the 'cellTextUnderline' tag is used, default is @False@.
--
cellTextUnderlineSet :: CellRendererTextClass self => Attr self Bool
cellTextUnderlineSet = newAttrFromBoolProperty "underline-set"

-- | Font variant (e.g. small caps).
--
cellTextVariant :: CellRendererTextClass self => Attr self Variant
cellTextVariant = newAttrFromEnumProperty "variant"
              {# call pure pango_variant_get_type #}

-- | Whether the 'cellTextVariant' tag is used, default is @False@.
--
cellTextVariantSet :: CellRendererTextClass self => Attr self Bool
cellTextVariantSet = newAttrFromBoolProperty "variant-set"

-- | Font weight, default: 400.
--
cellTextWeight :: CellRendererTextClass self => Attr self Int
cellTextWeight = newAttrFromIntProperty "weight"

-- | Whether the 'cellTextWeight' tag is used, default is @False@.
--
cellTextWeightSet :: CellRendererTextClass self => Attr self Bool
cellTextWeightSet = newAttrFromBoolProperty "weight-set"

#if GTK_CHECK_VERSION(2,6,0)

-- | The desired width of the cell, in characters. If this property is set
--   to @-1@, the width will be calculated automatically, otherwise the cell
--   will request either 3 characters or the property value, whichever is
--   greater.
--
-- * Available in Gtk 2.6 or higher.
--
cellTextWidthChars :: CellRendererTextClass self => Attr self Int
cellTextWidthChars = newAttrFromIntProperty "width-chars"

#endif

#if GTK_CHECK_VERSION(2,8,0)

-- | Specifies how to break the string into multiple lines, if the cell
--   renderer does not have enough room to display the entire string.
--   This property has no effect unless the 'cellTextWrapWidth' property is set.
--
-- * Available in Gtk 2.8 or higher.
--
cellTextWrapMode :: CellRendererTextClass self => Attr self LayoutWrapMode
cellTextWrapMode = newAttrFromEnumProperty "wrap-mode"
               {# call pure pango_wrap_mode_get_type #}

-- | Specifies the width at which the text is wrapped. The wrap-mode
--   property can be used to influence at what character positions the
--   line breaks can be placed. Setting wrap-width to @-1@ turns wrapping off.
--
-- * Available in Gtk 2.8 or higher.
--
cellTextWrapWidth :: CellRendererTextClass self => Attr self Int
cellTextWrapWidth = newAttrFromIntProperty "wrap-width"

#endif


#if GTK_CHECK_VERSION(2,10,0)
-- %hash c:a59c d:a84a
-- | Specifies how to align the lines of text with respect to each other.
--
-- Note that this property describes how to align the lines of text in case
-- there are several of them. The
-- 'Graphics.UI.Gtk.ModelView.CellRenderer.cellXAlign' property of
-- 'CellRenderer', on the other hand, sets the horizontal alignment of the
-- whole text.
--
-- Default value: 'Graphics.Rendering.Pango.Layout.AlignLeft'
--
-- * Available since Gtk+ version 2.10
--
cellTextAlignment :: CellRendererTextClass self => Attr self LayoutAlignment
cellTextAlignment = newAttrFromEnumProperty "alignment"
                              {# call pure unsafe pango_alignment_get_type #}
#endif

--------------------
-- Signals

-- %hash c:a541 d:18f9
-- | Emitted when the user finished editing a cell.
--
-- Whenever editing is finished successfully, this signal is emitted which
-- indicates that the model should be updated with the supplied value.
-- The value is always a string which matches the 'cellText' attribute of
-- 'CellRendererText' (and its derivates like 'CellRendererCombo').
--
-- * This signal is not emitted when editing is disabled (see
--   'cellTextEditable') or when the user aborts editing.
--
edited :: (CellRendererTextClass self, GlibString string) =>
          Signal self (TreePath -> string -> IO ())
edited = Signal internalEdited

--------------------
-- Deprecated Signals

#ifndef DISABLE_DEPRECATED
-- %hash c:76ed
onEdited :: (CellRendererTextClass self, GlibString string) => self
 -> (TreePath -> string -> IO ())
 -> IO (ConnectId self)
onEdited = internalEdited False
{-# DEPRECATED onEdited "instead of 'onEdited obj' use 'on obj edited'" #-}

-- %hash c:f70c
afterEdited :: (CellRendererTextClass self, GlibString string) => self
 -> (TreePath -> string -> IO ())
 -> IO (ConnectId self)
afterEdited = internalEdited True
{-# DEPRECATED afterEdited "instead of 'afterEdited obj' use 'after obj edited'" #-}
#endif

internalEdited :: (CellRendererTextClass cr, GlibString string) =>
                  Bool -> cr ->
                  (TreePath -> string -> IO ()) ->
                  IO (ConnectId cr)
internalEdited after cr user =
  connect_GLIBSTRING_GLIBSTRING__NONE "edited" after cr $ \path string -> do
    user (stringToTreePath path) string
