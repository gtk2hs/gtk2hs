-- -*-haskell-*-
--  GIMP Toolkit (GTK) CellRendererText TreeView
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.8 $ from $Date: 2005/11/18 15:54:57 $
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
module Graphics.UI.Gtk.TreeList.CellRendererText (
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
  castToCellRendererText,
  toCellRendererText,

-- * Constructors
  cellRendererTextNew,

-- * Methods
  cellRendererTextSetFixedHeightFromFont,

-- * Attributes
  cellTextBackground,
  cellTextBackgroundColor,
  cellTextBackgroundSet,
  cellTextForeground,
  cellTextForegroundColor,
  cellTextForegroundSet,
  cellTextEditable,
  cellTextEditableSet,
  cellTextFont,
--  cellTextFontDescription,
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
  cellTextSizeSet,
  cellTextSizePoints,
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

-- * Signals
  onEdited,
  afterEdited
  ) where

import Maybe	(fromMaybe)
import Monad	(liftM)

import System.Glib.FFI
import System.Glib.Properties
import System.Glib.Attributes			(Attr, WriteAttr)
import Graphics.UI.Gtk.Abstract.Object		(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.TreeList.TreeIter#}
import Graphics.UI.Gtk.General.Structs		(treeIterSize)
import Graphics.UI.Gtk.Pango.Enums
import Graphics.UI.Gtk.General.Enums		(WrapMode)
import Graphics.UI.Gtk.Gdk.GC			(Color)

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
-- 'cellTextFont' and 'cellYPad' attribute set on it. Further changes in these
-- properties do not
-- affect the height, so they must be accompanied by a subsequent call to this
-- function. Using this function is unflexible, and should really only be used
-- if calculating the size of a cell is too slow (ie, a massive number of cells
-- displayed). If @numberOfRows@ is -1, then the fixed height is unset, and the
-- height is determined by the properties again.
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
-- Attributes

-- | Text to render.
--
-- Default value: @\"\"@
--
cellText :: CellRendererTextClass self => Attr self String
cellText  = newAttrFromStringProperty "text"

-- | Define a markup string instead of a text.
--
-- Default value: @\"\"@
--
cellMarkup :: CellRendererTextClass self => WriteAttr self String
cellMarkup = writeAttrFromStringProperty "markup"

-- | Whether or not to keep all text in a single paragraph.
--
-- Default value: @False@
--
cellTextSingleParagraphMode :: CellRendererTextClass self => Attr self Bool
cellTextSingleParagraphMode = newAttrFromBoolProperty "single-paragraph-mode"

-- | Text background color as a string.
--
-- Default value: @\"\"@
--
cellTextBackground :: CellRendererTextClass self => WriteAttr self String
cellTextBackground = writeAttrFromStringProperty "background"

-- | Text background color as a 'Color'.
--
cellTextBackgroundColor :: CellRendererClass self => Attr self Color
cellTextBackgroundColor = newAttrFromBoxedStorableProperty "background-gdk"
  {# call pure unsafe gdk_color_get_type #}

-- | Whether the 'cellTextBackground'\/'cellTextBackgroundColor' attribute is
-- set.
--
-- You can use this to reset the attribute to its default.
--
-- Default value: @False@
--
cellTextBackgroundSet :: CellRendererClass self => Attr self Bool
cellTextBackgroundSet = newAttrFromBoolProperty "background-set"

-- | Text foreground color as a string.
--
-- Default value: @\"\"@
--
cellTextForeground :: CellRendererTextClass self => WriteAttr self String
cellTextForeground = writeAttrFromStringProperty "foreground"

-- | Text foreground color as a 'Color'.
--
cellTextForegroundColor :: CellRendererClass self => Attr self Color
cellTextForegroundColor = newAttrFromBoxedStorableProperty "foreground-gdk"
  {# call pure unsafe gdk_color_get_type #}

-- | Whether the 'cellTextForeground'\/'cellTextForegroundColor' attribute is
--
-- You can use this to reset the attribute to its default.
--
-- Default value: @False@
--
cellTextForegroundSet :: CellRendererClass self => Attr self Bool
cellTextForegroundSet = newAttrFromBoolProperty "foreground-set"

-- | Whether the text can be modified by the user.
--
-- Default value: @False@
--
cellTextEditable :: CellRendererTextClass self => Attr self Bool
cellTextEditable = newAttrFromBoolProperty "editable"

-- | Whether the 'cellTextEditable' attribute is set.
--
-- You can use this to reset the attribute to its default.
--
-- Default value: @False@
--
cellTextEditableSet :: CellRendererClass self => Attr self Bool
cellTextEditableSet = newAttrFromBoolProperty "editable-set"

-- | Font description as a string.
--
-- Default value: @\"\"@
--
cellTextFont :: CellRendererTextClass self => Attr self String
cellTextFont = newAttrFromStringProperty "font"

-- | Name of the font family, e.g. Sans, Helvetica, Times, Monospace.
--
-- Default value: @\"\"@
--
cellTextFamily :: CellRendererTextClass self => Attr self String
cellTextFamily = newAttrFromStringProperty "family"

-- | Whether the 'cellTextFamily' attribute is set.
--
-- You can use this to reset the attribute to its default.
--
-- Default value: @False@
--
cellTextFamilySet :: CellRendererClass self => Attr self Bool
cellTextFamilySet = newAttrFromBoolProperty "family-set"

-- | Font style.
--
-- Default value: 'StyleNormal'
--
cellTextStyle :: CellRendererTextClass self => Attr self FontStyle
cellTextStyle = newAttrFromEnumProperty "style"
  {# call pure unsafe pango_style_get_type #}

-- | Whether the 'cellTextStyle' attribute is set.
--
-- You can use this to reset the attribute to its default.
--
-- Default value: @False@
--
cellTextStyleSet :: CellRendererClass self => Attr self Bool
cellTextStyleSet = newAttrFromBoolProperty "style-set"

-- | Font variant.
--
-- Default value: 'VariantNormal'
--
cellTextVariant :: CellRendererTextClass self => Attr self Variant
cellTextVariant = newAttrFromEnumProperty "variant"
  {# call pure unsafe pango_variant_get_type #}

-- | Whether the 'cellTextVariant' attribute is set.
--
-- You can use this to reset the attribute to its default.
--
-- Default value: @False@
--
cellTextVariantSet :: CellRendererClass self => Attr self Bool
cellTextVariantSet = newAttrFromBoolProperty "variant-set"

-- | Font weight.
--
-- Allowed values: >= 0
--
-- Default value: 400
--
cellTextWeight :: CellRendererTextClass self => Attr self Int
cellTextWeight = newAttrFromIntProperty "weight"

-- | Whether the 'cellTextWeight' attribute is set.
--
-- You can use this to reset the attribute to its default.
--
-- Default value: @False@
--
cellTextWeightSet :: CellRendererClass self => Attr self Bool
cellTextWeightSet = newAttrFromBoolProperty "weight-set"

-- | Font stretch.
--
-- Default value: 'StretchNormal'
--
cellTextStretch :: CellRendererTextClass self => Attr self Stretch
cellTextStretch = newAttrFromEnumProperty "stretch"
  {# call pure unsafe pango_stretch_get_type #}

-- | Whether the 'cellTextStretch' attribute is set.
--
-- You can use this to reset the attribute to its default.
--
-- Default value: @False@
--
cellTextStretchSet :: CellRendererClass self => Attr self Bool
cellTextStretchSet = newAttrFromBoolProperty "stretch-set"

-- | Font size.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
cellTextSize :: CellRendererTextClass self => Attr self Int
cellTextSize = newAttrFromIntProperty "size"

-- | Whether the 'cellTextSize' attribute is set.
--
-- You can use this to reset the attribute to its default.
--
-- Default value: @False@
--
cellTextSizeSet :: CellRendererClass self => Attr self Bool
cellTextSizeSet = newAttrFromBoolProperty "size-set"

-- | Font size in points.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
cellTextSizePoints :: CellRendererTextClass self => Attr self Double
cellTextSizePoints = newAttrFromDoubleProperty "size-points"

-- | Font scaling factor.
--
-- Allowed values: >= 0
--
-- Default value: 1
--
cellTextScale :: CellRendererTextClass self => Attr self Double
cellTextScale = newAttrFromDoubleProperty "scale"

-- | Whether the 'cellTextScale' attribute is set.
--
-- You can use this to reset the attribute to its default.
--
-- Default value: @False@
--
cellTextScaleSet :: CellRendererClass self => Attr self Bool
cellTextScaleSet = newAttrFromBoolProperty "scale-set"

-- | Offset of text above the baseline (below the baseline if rise is
-- negative).
--
-- Allowed values: @>= -2147483647@
--
-- Default value: 0
--
cellTextRise :: CellRendererTextClass self => Attr self Int
cellTextRise = newAttrFromIntProperty "rise"

-- | Whether the 'cellTextRise' attribute is set.
--
-- You can use this to reset the attribute to its default.
--
-- Default value: @False@
--
cellTextRiseSet :: CellRendererClass self => Attr self Bool
cellTextRiseSet = newAttrFromBoolProperty "rise-set"

-- | Whether to strike through the text.
--
-- Default value: @False@
--
cellTextStrikethrough :: CellRendererTextClass self => Attr self Bool
cellTextStrikethrough = newAttrFromBoolProperty "strikethrough"

-- | Whether the 'cellTextStrikethrough' attribute is set.
--
-- You can use this to reset the attribute to its default.
--
-- Default value: @False@
--
cellTextStrikethroughSet :: CellRendererClass self => Attr self Bool
cellTextStrikethroughSet = newAttrFromBoolProperty "strikethrough-set"

-- | Style of underline for this text.
--
-- Default value: 'UnderlineNone'
--
cellTextUnderline :: CellRendererTextClass self => Attr self Underline
cellTextUnderline = newAttrFromEnumProperty "underline"
  {# call pure unsafe pango_underline_get_type #}

-- | Whether the 'cellTextUnderline' attribute is set.
--
-- You can use this to reset the attribute to its default.
--
-- Default value: @False@
--
cellTextUnderlineSet :: CellRendererClass self => Attr self Bool
cellTextUnderlineSet = newAttrFromBoolProperty "underline-set"

-- | The language this text is in, as an ISO code. Pango can use this as a
-- hint when rendering the text. If you don't understand this parameter, you
-- probably don't need it.
--
-- Default value: @\"\"@
--
cellTextLanguage :: CellRendererTextClass self => Attr self String
cellTextLanguage = newAttrFromStringProperty "language"

-- | Whether the 'cellTextLanguage' attribute is set.
--
-- You can use this to reset the attribute to its default.
--
-- Default value: @False@
--
cellTextLanguageSet :: CellRendererClass self => Attr self Bool
cellTextLanguageSet = newAttrFromBoolProperty "language-set"

#if GTK_CHECK_VERSION(2,6,0)
-- | Specifies the preferred place to ellipsize the string, if the cell
-- renderer does not have enough room to display the entire string. Setting it
-- to 'EllipsizeNone' turns off ellipsizing. See the wrap-width property for
-- another way of making the text fit into a given width.
--
-- Default value: 'EllipsizeNone'
--
cellTextEllipsize :: CellRendererTextClass self => Attr self EllipsizeMode
cellTextEllipsize = newAttrFromEnumProperty "ellipsize"
  {# call pure unsafe pango_ellipsize_mode_get_type #}

-- | Whether the 'cellTextEllipsize' attribute is set.
--
-- You can use this to reset the attribute to its default.
--
-- Default value: @False@
--
cellTextEllipsizeSet :: CellRendererClass self => Attr self Bool
cellTextEllipsizeSet = newAttrFromBoolProperty "ellipsize-set"

-- | The desired width of the cell, in characters. If this property is set to
-- -1, the width will be calculated automatically, otherwise the cell will
-- request either 3 characters or the property value, whichever is greater.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
cellTextWidthChars :: CellRendererTextClass self => Attr self Int
cellTextWidthChars = newAttrFromIntProperty "width-chars"
#endif

#if GTK_CHECK_VERSION(2,8,0)
-- | Specifies how to break the string into multiple lines, if the cell
-- renderer does not have enough room to display the entire string. This
-- property has no effect unless the wrap-width property is set.
--
-- Default value: 'WrapChar'
--
cellTextWrapMode :: CellRendererTextClass self => Attr self WrapMode
cellTextWrapMode = newAttrFromEnumProperty "wrap-mode"
  {# call pure unsafe gtk_wrap_mode_get_type #}

-- | Specifies the width at which the text is wrapped. The wrap-mode property
-- can be used to influence at what character positions the line breaks can be
-- placed. Setting wrap-width to -1 turns wrapping off.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
cellTextWrapWidth :: CellRendererTextClass self => Attr self Int
cellTextWrapWidth = newAttrFromIntProperty "wrap-width"
#endif


--------------------
-- Signals

-- | Emitted when the user finished editing a cell.
--
-- * This signal is not emitted when editing is disabled (see 
--   'cellEditable') or when the user aborts editing.
--
onEdited, afterEdited :: TreeModelClass tm => CellRendererText -> tm ->
			 (TreeIter -> String -> IO ()) ->
			 IO (ConnectId CellRendererText)
onEdited = internalEdited False
afterEdited = internalEdited True

internalEdited :: TreeModelClass tm => Bool -> 
                  CellRendererText -> tm ->
                  (TreeIter -> String -> IO ()) ->
                  IO (ConnectId CellRendererText)
internalEdited after cr tm user =
  connect_PTR_STRING__NONE "edited" after cr $ \strPtr string ->
  (receiveTreeIter $ \iterPtr ->
  {# call gtk_tree_model_get_iter_from_string #}
    (toTreeModel tm)
    iterPtr
    strPtr)
  >>= \res ->
  case res of
    Nothing -> fail "edited signal: invalid tree path"
    Just iter -> user iter string

