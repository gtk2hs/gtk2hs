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
--
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
  cellEditable,
  cellEditableSet,
#if GTK_CHECK_VERSION(2,6,0)
  cellEllipsize,
  cellEllipsizeSet,
#endif
  cellFamily,
  cellFamilySet,
  cellFont,
  cellFontDesc,
  cellTextForeground,
  cellTextForegroundColor,
  cellTextForegroundSet,
  cellLanguage,
  cellLanguageSet,
  cellMarkup,
  cellRise,
  cellRiseSet,
  cellScale,
  cellScaleSet,
  cellSingleParagraphMode,
  cellSize,
  cellStretch,
  cellStretchSet,
  cellStrikethrough,
  cellStrikethroughSet,
  cellStyle,
  cellStyleSet,
  cellText,
  cellUnderline,
  cellUnderlineSet,
  cellVariant,
  cellVariantSet,
  cellWeight,
  cellWeightSet,
#if GTK_CHECK_VERSION(2,6,0)
  cellWidthChars,
#endif
#if GTK_CHECK_VERSION(2,8,0)
  cellWrapMode,
  cellWrapWidth,
#endif

-- * Signals
  onEdited,
  afterEdited
  ) where

import Maybe	(fromMaybe)
import Monad	(liftM)

import System.Glib.FFI
import System.Glib.Properties
import System.Glib.Attributes ( Attr, WriteAttr, ReadAttr )
import Graphics.UI.Gtk.Abstract.Object		(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.TreeList.TreePath#}
import Graphics.UI.Gtk.General.Structs		(Color(..))
import Graphics.UI.Gtk.Pango.Enums
{#import Graphics.UI.Gtk.Pango.Types#} ( FontDescription(..),
					 makeNewFontDescription )
import Graphics.UI.Gtk.Pango.Layout    ( LayoutWrapMode )

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
-- Properties

-- | Text background color as a string.
--
-- Default value: @\"\"@
--
cellTextBackground :: CellRendererClass self => WriteAttr self String
cellTextBackground = writeAttrFromStringProperty "cell-background"

-- | Text background color as a 'Color'.
--
cellTextBackgroundColor :: CellRendererClass self => Attr self Color
cellTextBackgroundColor = newAttrFromBoxedStorableProperty "cell-background-gdk"
  {# call pure unsafe gdk_color_get_type #}

-- | Whether the 'cellTextBackground'\/'cellTextBackgroundColor' attribute is set.
--
-- Default value: @False@
--
cellTextBackgroundSet :: CellRendererClass self => Attr self Bool
cellTextBackgroundSet = newAttrFromBoolProperty "cell-background-set"

-- | Whether the text can be modified by the user.
--
cellEditable :: CellRendererTextClass self => Attr self Bool
cellEditable = newAttrFromBoolProperty "editable"

-- | Whether the text can be modified by the user.
--
cellEditableSet :: CellRendererTextClass self => Attr self Bool
cellEditableSet = newAttrFromBoolProperty "editable-set"

#if GTK_CHECK_VERSION(2,6,0)
-- | Specifies the preferred place to ellipsize the string, if the cell
--   renderer does not have enough room to display the entire string.
--   Setting it to 'Graphics.UI.Gtk.Pango.Enums.EllipsizeNone' turns off
--   ellipsizing. See the 'cellWrapWidth' property for another way of
--   making the text fit into a given width.
--
-- * Available in Gtk 2.6 or higher.
--
cellEllipsize :: CellRendererTextClass self => Attr self EllipsizeMode
cellEllipsize = newAttrFromEnumProperty "ellipsize"
		{# call pure pango_ellipsize_mode_get_type #}

-- | Whether the 'cellEllipsize' tag affects the ellipsize mode.
--
-- * Available in Gtk 2.6 or higher.
--
cellEllipsizeSet :: CellRendererTextClass self => Attr self Bool
cellEllipsizeSet = newAttrFromBoolProperty "ellipsize-set"
#endif

-- | Name of the font family, e.g. Sans, Helvetica, Times, Monospace.
--
cellFamily :: CellRendererTextClass self => Attr self String
cellFamily = newAttrFromStringProperty "family"

-- | Determines if 'cellFamily' has an effect.
--
cellFamilySet :: CellRendererTextClass self => Attr self Bool
cellFamilySet = newAttrFromBoolProperty "family-set"

-- | Font description as a string.
--
cellFont :: CellRendererTextClass self => Attr self String
cellFont = newAttrFromStringProperty "font"

-- | Font description as a 'Graphics.UI.Gtk.Pango.FontDescription'.
--
cellFontDesc :: CellRendererTextClass self => Attr self FontDescription
cellFontDesc = newAttrFromBoxedOpaqueProperty makeNewFontDescription
  (\(FontDescription fd) act -> withForeignPtr fd act) "font-desc"
  {# call pure unsafe pango_font_description_get_type #}

-- | Text foreground color as a string.
--
-- Default value: @\"\"@
--
cellTextForeground :: CellRendererClass self => WriteAttr self String
cellTextForeground = writeAttrFromStringProperty "cell-foreground"

-- | Text foreground color as a 'Color'.
--
cellTextForegroundColor :: CellRendererClass self => Attr self Color
cellTextForegroundColor = newAttrFromBoxedStorableProperty "cell-foreground-gdk"
  {# call pure unsafe gdk_color_get_type #}

-- | Whether the 'cellTextForeground'\/'cellTextForegroundColor' attribute is set.
--
-- Default value: @False@
--
cellTextForegroundSet :: CellRendererClass self => Attr self Bool
cellTextForegroundSet = newAttrFromBoolProperty "cell-foreground-set"

-- | The language this text is in, as an ISO code. Pango can use this as
--   a hint when rendering the text. If you don't understand this parameter,
--   you probably don't need it.
--
cellLanguage :: CellRendererTextClass self => Attr self (Maybe String)
cellLanguage = newAttrFromMaybeStringProperty "language"

-- | Whether the 'cellLanguage' tag is used, default is @False@.
--
cellLanguageSet :: CellRendererTextClass self => Attr self Bool
cellLanguageSet = newAttrFromBoolProperty "language-set"

-- | Define a markup string instead of a text. See 'cellText'.
--
cellMarkup :: CellRendererTextClass cr => WriteAttr cr (Maybe String)
cellMarkup  = writeAttrFromMaybeStringProperty "markup"

-- | Offset of text above the baseline (below the baseline if rise is
--   negative).
--
cellRise :: CellRendererTextClass self => Attr self Int
cellRise = newAttrFromIntProperty "rise"

-- | Whether the 'cellRise' tag is used, default is @False@.
--
cellRiseSet :: CellRendererTextClass self => Attr self Bool
cellRiseSet = newAttrFromBoolProperty "rise-set"

-- | Font scaling factor. Default is 1.
--
cellScale :: CellRendererTextClass self => Attr self Double
cellScale = newAttrFromDoubleProperty "scale"

-- | Whether the 'cellScale' tag is used, default is @False@.
--
cellScaleSet :: CellRendererTextClass self => Attr self Bool
cellScaleSet = newAttrFromBoolProperty "scale-set"

-- | Whether or not to keep all text in a single paragraph.
--
cellSingleParagraphMode :: CellRendererTextClass self => Attr self Bool
cellSingleParagraphMode = newAttrFromBoolProperty "single-paragraph-mode"

-- | Font size in points.
--
cellSize :: CellRendererTextClass self => Attr self Double
cellSize = newAttrFromDoubleProperty "size-points"

-- | Whether the 'cellSize' tag is used, default is @False@.
--
cellSizeSet :: CellRendererTextClass self => Attr self Bool
cellSizeSet = newAttrFromBoolProperty "size-set"

-- | Font stretch.
--
cellStretch :: CellRendererTextClass self => Attr self Stretch
cellStretch = newAttrFromEnumProperty "stretch"
	      {# call pure pango_stretch_get_type #}

-- | Whether the 'cellStretch' tag is used, default is @False@.
--
cellStretchSet :: CellRendererTextClass self => Attr self Bool
cellStretchSet = newAttrFromBoolProperty "stretch-set"

-- | Whether to strike through the text.
--
cellStrikethrough :: CellRendererTextClass self => Attr self Bool
cellStrikethrough = newAttrFromBoolProperty "strikethrough"

-- | Whether the 'cellStrikethrough' tag is used, default is @False@.
--
cellStrikethroughSet :: CellRendererTextClass self => Attr self Bool
cellStrikethroughSet = newAttrFromBoolProperty "strikethrough-set"

-- | Font style (e.g. normal or italics).
--
cellStyle :: CellRendererTextClass self => Attr self FontStyle
cellStyle = newAttrFromEnumProperty "style"
	    {# call pure pango_style_get_type #}

-- | Whether the 'cellStyle' tag is used, default is @False@.
--
cellStyleSet :: CellRendererTextClass self => Attr self Bool
cellStyleSet = newAttrFromBoolProperty "style-set"

-- | Define the attribute that specifies the text to be rendered. See
--   also 'cellMarkup'.
--
cellText :: CellRendererTextClass cr => Attr cr (Maybe String)
cellText  = newAttrFromMaybeStringProperty "text"

-- | Style of underline for this text.
--
cellUnderline :: CellRendererTextClass self => Attr self Underline
cellUnderline = newAttrFromEnumProperty "underline"
		{# call pure pango_underline_get_type #}

-- | Whether the 'cellUnderline' tag is used, default is @False@.
--
cellUnderlineSet :: CellRendererTextClass self => Attr self Bool
cellUnderlineSet = newAttrFromBoolProperty "underline-set"

-- | Font variant (e.g. small caps).
--
cellVariant :: CellRendererTextClass self => Attr self Variant
cellVariant = newAttrFromEnumProperty "variant"
	      {# call pure pango_variant_get_type #}

-- | Whether the 'cellVariant' tag is used, default is @False@.
--
cellVariantSet :: CellRendererTextClass self => Attr self Bool
cellVariantSet = newAttrFromBoolProperty "variant-set"

-- | Font weight, default: 400.
--
cellWeight :: CellRendererTextClass self => Attr self Int
cellWeight = newAttrFromIntProperty "weight"

-- | Whether the 'cellWeight' tag is used, default is @False@.
--
cellWeightSet :: CellRendererTextClass self => Attr self Bool
cellWeightSet = newAttrFromBoolProperty "weight-set"

#if GTK_CHECK_VERSION(2,6,0)

-- | The desired width of the cell, in characters. If this property is set
--   to @-1@, the width will be calculated automatically, otherwise the cell
--   will request either 3 characters or the property value, whichever is
--   greater.
--
-- * Available in Gtk 2.6 or higher.
--
cellWidthChars :: CellRendererTextClass self => Attr self Int
cellWidthChars = newAttrFromIntProperty "width-chars"

#endif

#if GTK_CHECK_VERSION(2,8,0)

-- | Specifies how to break the string into multiple lines, if the cell
--   renderer does not have enough room to display the entire string.
--   This property has no effect unless the 'cellWrapWidth' property is set.
--
-- * Available in Gtk 2.8 or higher.
--
cellWrapMode :: CellRendererTextClass self => Attr self LayoutWrapMode
cellWrapMode = newAttrFromEnumProperty "wrap-mode"
	       {# call pure pango_wrap_mode_get_type #}

-- | Specifies the width at which the text is wrapped. The wrap-mode
--   property can be used to influence at what character positions the
--   line breaks can be placed. Setting wrap-width to @-1@ turns wrapping off.
--
-- * Available in Gtk 2.8 or higher.
--
cellWrapWidth :: CellRendererTextClass self => Attr self Int
cellWrapWidth = newAttrFromIntProperty "wrap-width"

#endif

-- | Emitted when the user finished editing a cell.
--
-- * Whenever editing is finished successfully, this signal is emitted which
--   indicates that the model should be updated with the supplied value.
--   The value is always a string which matches 'CellRendererText' renderers
--   and 'CellRendererCombo' when the combo box accepts additional entries.
--   If the combo box has a predefined set of possible selections, the
--   string that this handler receives is always empty. In this case the
--   handler
--   of this signal needs to query the currently selected index of the combo
--   box and store that index in the model of this cell renderer. The only
--   time this combo box is passed to the user program is in the
--   'onEditingStarted' signal of the 'CellRenderer' base class. Hence,
--   when this handler is run, the handler to store the resulting value
--   needs to be installed using this function. See the
--   user manual for an example of this.
--
-- * This signal is not emitted when editing is disabled (see 
--   'cellEditable') or when the user aborts editing.
--
onEdited, afterEdited :: CellRendererTextClass cr =>
			 cr -> (TreePath -> String -> IO ()) ->
			 IO (ConnectId cr)
onEdited = internalEdited False
afterEdited = internalEdited True

internalEdited :: CellRendererTextClass cr =>
		  Bool -> cr ->
                  (TreePath -> String -> IO ()) ->
                  IO (ConnectId cr)
internalEdited after cr user =
  connect_STRING_STRING__NONE "edited" after cr $ \path string -> do
    user (stringToTreePath path) string
