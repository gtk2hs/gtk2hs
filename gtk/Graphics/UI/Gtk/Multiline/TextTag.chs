{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget TextTag
--
--  Author : Duncan Coutts
--
--  Created: 4 August 2004
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
-- TODO
--
--     Didn't bind `textTagTabs` properties, we need to bind PangoTab first (in `pango-tabs.c`)
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A tag that can be applied to text in a 'TextBuffer'
--
module Graphics.UI.Gtk.Multiline.TextTag (
-- * Detail
--
-- | You may wish to begin by reading the text widget conceptual overview
-- which gives an overview of all the objects and data types related to the
-- text widget and how they work together.
--
-- Tags should be in the 'TextTagTable' for a given
-- 'Graphics.UI.Gtk.Multiline.TextBuffer.TextBuffer' before
-- using them with that buffer.
--
-- 'Graphics.UI.Gtk.Multiline.TextBuffer.textBufferCreateTag' is the best way
-- to create tags.
--
-- The 'textTagInvisible' property was not implemented for Gtk+ 2.0; it's planned
-- to be implemented in future releases.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----TextTag
-- @

-- * Types
  TextTag,
  TextTagClass,
  castToTextTag, gTypeTextTag,
  toTextTag,
  TagName,

-- * Constructors
  textTagNew,

-- * Methods
  textTagSetPriority,
  textTagGetPriority,
  TextAttributes(..),
  textAttributesNew,
  textAttributesCopy,
  textAttributesCopyValues,
  makeNewTextAttributes, -- internal

-- * Attributes
  textTagName,
  textTagBackground,
  textTagBackgroundSet,
  textTagBackgroundFullHeight,
  textTagBackgroundFullHeightSet,
  textTagBackgroundGdk,
#if GTK_MAJOR_VERSION < 3
  textTagBackgroundStipple,
  textTagBackgroundStippleSet,
#endif
  textTagForeground,
  textTagForegroundSet,
  textTagForegroundGdk,
#if GTK_MAJOR_VERSION < 3
  textTagForegroundStipple,
  textTagForegroundStippleSet,
#endif
  textTagDirection,
  textTagEditable,
  textTagEditableSet,
  textTagFont,
  textTagFontDesc,
  textTagFamily,
  textTagFamilySet,
  textTagStyle,
  textTagStyleSet,
  -- textTagTabs,
  textTagTabsSet,
  textTagVariant,
  textTagVariantSet,
  textTagWeight,
  textTagWeightSet,
  textTagStretch,
  textTagStretchSet,
  textTagSize,
  textTagSizeSet,
  textTagScale,
  textTagScaleSet,
  textTagSizePoints,
  textTagJustification,
  textTagJustificationSet,
  textTagLanguage,
  textTagLanguageSet,
  textTagLeftMargin,
  textTagLeftMarginSet,
  textTagRightMargin,
  textTagRightMarginSet,
  textTagIndent,
  textTagIndentSet,
  textTagRise,
  textTagRiseSet,
  textTagPixelsAboveLines,
  textTagPixelsAboveLinesSet,
  textTagPixelsBelowLines,
  textTagPixelsBelowLinesSet,
  textTagPixelsInsideWrap,
  textTagPixelsInsideWrapSet,
  textTagStrikethrough,
  textTagStrikethroughSet,
  textTagUnderline,
  textTagUnderlineSet,
  textTagWrapMode,
  textTagWrapModeSet,
#if GTK_CHECK_VERSION(2,8,0)
  textTagInvisible,
  textTagInvisibleSet,
  textTagParagraphBackground,
  textTagParagraphBackgroundSet,
  textTagParagraphBackgroundGdk,
#endif
  textTagPriority,

-- * Signals
  textTagEvent,

-- * Deprecated
#ifndef DISABLE_DEPRECATED
  onTextTagEvent
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.Rendering.Pango.Font
import Graphics.Rendering.Pango.BasicTypes      (FontDescription (..), makeNewFontDescription)
import Graphics.Rendering.Pango.Enums   (FontStyle(..), Variant(..),
                                         Stretch(..), Underline(..))
import Graphics.UI.Gtk.General.Enums    (TextDirection(..),
                                         Justification(..), WrapMode(..))
import Graphics.UI.Gtk.General.Structs  (Color(..))
import Graphics.UI.Gtk.Multiline.Types  ( TextIter, mkTextIterCopy )
#ifndef DISABLE_DEPRECATED
import Graphics.UI.Gtk.Gdk.Events       (Event, marshalEvent)
#endif
import Graphics.UI.Gtk.Gdk.EventM (EventM, EAny)
import Control.Monad.Reader ( runReaderT )

{# context lib="gtk" prefix="gtk" #}

type TagName = DefaultGlibString

--------------------
-- Constructors

-- | Creates a 'TextTag'.
--
-- * Supplying @Nothing@ as tag name results in an anonymous tag.
--
textTagNew :: Maybe TagName -> IO TextTag
textTagNew (Just name) =
  wrapNewGObject mkTextTag $
  withUTFString name $ \namePtr ->
  {# call unsafe text_tag_new #}
    namePtr
textTagNew Nothing =
  wrapNewGObject mkTextTag $ {# call unsafe text_tag_new #} nullPtr


--------------------
-- Methods

-- | Get the tag priority.
--
textTagGetPriority :: TextTagClass self => self -> IO Int
textTagGetPriority self =
  liftM fromIntegral $
  {# call unsafe text_tag_get_priority #}
    (toTextTag self)

-- | Sets the priority of a 'TextTag'. Valid priorities are start at 0 and go
-- to one less than
-- 'Graphics.UI.Gtk.Multiline.TextTagTable.textTagTableGetSize'.
-- Each tag in a table has a unique
-- priority; setting the priority of one tag shifts the priorities of all the
-- other tags in the table to maintain a unique priority for each tag. Higher
-- priority tags \"win\" if two tags both set the same text attribute. When
-- adding a tag to a tag table, it will be assigned the highest priority in the
-- table by default; so normally the precedence of a set of tags is the order
-- in which they were added to the table, or created with
-- 'Graphics.UI.Gtk.Multiline.TextBuffer.textBufferCreateTag', which adds the tag to the buffer's table
-- automatically.
--
textTagSetPriority :: TextTagClass self => self -> Int -> IO ()
textTagSetPriority self priority =
  {# call text_tag_set_priority #}
    (toTextTag self)
    (fromIntegral priority)

-- TextAttributes methods

{#pointer * TextAttributes foreign newtype#}

-- | Creates a 'TextAttributes', which describes a set of properties on some
-- text.
--
textAttributesNew :: IO TextAttributes
textAttributesNew =
  {#call unsafe text_attributes_new#} >>= makeNewTextAttributes

-- | Copies src and returns a new 'TextAttributes'.
--
textAttributesCopy ::
  TextAttributes  -- ^ @src@ - a 'TextAttributes' to be copied
 -> IO TextAttributes
textAttributesCopy src =
  {#call text_attributes_copy#} src >>= makeNewTextAttributes

-- | Copies the values from src to dest so that dest has the same values as src.
--
textAttributesCopyValues :: TextAttributes -> TextAttributes -> IO ()
textAttributesCopyValues src dest =
  {# call text_attributes_copy_values #} src dest

-- | This function is use internal for transform TextAttributes.
-- Don't expoert this function.
makeNewTextAttributes :: Ptr TextAttributes -> IO TextAttributes
makeNewTextAttributes ptr =
  liftM TextAttributes $ newForeignPtr ptr text_attributes_unref

foreign import ccall unsafe "&gtk_text_attributes_unref"
  text_attributes_unref :: FinalizerPtr TextAttributes

--------------------
-- Attributes

-- | Name used to refer to the text tag. @Nothing@ for anonymous tags.
--
-- Default value: @Nothing@
--
textTagName :: (TextTagClass self, GlibString string) => Attr self (Maybe string)
textTagName = newAttrFromMaybeStringProperty "name"

-- | Background color as a string.
--
-- Default value: \"\"
--
textTagBackground :: (TextTagClass self, GlibString string) => WriteAttr self string
textTagBackground = writeAttrFromStringProperty "background"

-- | Whether this tag affects the background color.
--
-- Default value: @False@
--
textTagBackgroundSet :: TextTagClass self => Attr self Bool
textTagBackgroundSet = newAttrFromBoolProperty "background-set"

-- | Whether the background color fills the entire line height or only the
-- height of the tagged characters.
--
-- Default value: @False@
--
textTagBackgroundFullHeight :: TextTagClass self => Attr self Bool
textTagBackgroundFullHeight = newAttrFromBoolProperty "background-full-height"

-- | Whether this tag affects background height.
--
-- Default value: @False@
--
textTagBackgroundFullHeightSet :: TextTagClass self => Attr self Bool
textTagBackgroundFullHeightSet = newAttrFromBoolProperty "background-full-height-set"

-- | Background color as a (possibly unallocated) GdkColor.
--
textTagBackgroundGdk :: TextTagClass self => Attr self Color
textTagBackgroundGdk =
  newAttrFromBoxedStorableProperty "background-gdk"
  {#call pure unsafe gdk_color_get_type#}

#if GTK_MAJOR_VERSION < 3
-- | Bitmap to use as a mask when drawing the text background.
--
-- Removed in Gtk3.
textTagBackgroundStipple :: (TextTagClass self, PixmapClass pixmap) => ReadWriteAttr self Pixmap pixmap
textTagBackgroundStipple = newAttrFromObjectProperty "background-stipple"
  {# call pure unsafe gdk_pixmap_get_type #}

-- | Whether this tag affects the background stipple.
--
-- Default value: @False@
--
-- Removed in Gtk3.
textTagBackgroundStippleSet :: TextTagClass self => Attr self Bool
textTagBackgroundStippleSet = newAttrFromBoolProperty "background-stipple-set"
#endif

-- | Foreground color as a string.
--
-- Default value: \"\"
--
textTagForeground :: (TextTagClass self, GlibString string) => WriteAttr self string
textTagForeground = writeAttrFromStringProperty "foreground"

-- | Whether this tag affects the foreground color.
--
-- Default value: @False@
--
textTagForegroundSet :: TextTagClass self => Attr self Bool
textTagForegroundSet = newAttrFromBoolProperty "foreground-set"

-- | Foreground color as a (possibly unallocated) GdkColor.
--
textTagForegroundGdk :: TextTagClass self => Attr self Color
textTagForegroundGdk =
  newAttrFromBoxedStorableProperty "foreground-gdk"
  {# call pure unsafe gdk_color_get_type #}

#if GTK_MAJOR_VERSION < 3
-- | Bitmap to use as a mask when drawing the text foreground.
--
-- Removed in Gtk3.
textTagForegroundStipple :: (TextTagClass self, PixmapClass pixmap) => ReadWriteAttr self Pixmap pixmap
textTagForegroundStipple = newAttrFromObjectProperty "foreground-stipple"
  {# call pure unsafe gdk_pixmap_get_type #}

-- | Whether this tag affects the foreground stipple.
--
-- Default value: @False@
--
-- Removed in Gtk3.
textTagForegroundStippleSet :: TextTagClass self => Attr self Bool
textTagForegroundStippleSet = newAttrFromBoolProperty "foreground-stipple-set"
#endif

-- | Text direction, e.g. right-to-left or left-to-right.
--
-- Default value: 'TextDirLtr'
--
textTagDirection :: TextTagClass self => Attr self TextDirection
textTagDirection = newAttrFromEnumProperty "direction"
  {# call pure unsafe gtk_text_direction_get_type #}

-- | Whether the text can be modified by the user.
--
-- Default value: @True@
--
textTagEditable :: TextTagClass self => Attr self Bool
textTagEditable = newAttrFromBoolProperty "editable"

-- | Whether this tag affects text editability.
--
-- Default value: @False@
--
textTagEditableSet :: TextTagClass self => Attr self Bool
textTagEditableSet = newAttrFromBoolProperty "editable-set"

-- | Font description as a string, e.g. \"Sans Italic 12\".
--
-- Default value: \"\"
--
textTagFont :: (TextTagClass self, GlibString string) => Attr self string
textTagFont = newAttrFromStringProperty "font"

-- | Font description as a 'FontDescription' struct.
--
textTagFontDesc :: TextTagClass self => Attr self FontDescription
textTagFontDesc = newAttrFromBoxedOpaqueProperty makeNewFontDescription
  (\(FontDescription fd) act -> withForeignPtr fd act) "font-desc"
  {# call pure unsafe pango_font_description_get_type #}

-- | Name of the font family, e.g. Sans, Helvetica, Times, Monospace.
--
-- Default value: \"\"
--
textTagFamily :: (TextTagClass self, GlibString string) => Attr self string
textTagFamily = newAttrFromStringProperty "family"

-- | Whether this tag affects the font family.
--
-- Default value: @False@
--
textTagFamilySet :: TextTagClass self => Attr self Bool
textTagFamilySet = newAttrFromBoolProperty "family-set"

-- | Font style as a 'Style', e.g. 'StyleItalic'.
--
-- Default value: 'StyleNormal'
--
textTagStyle :: TextTagClass self => Attr self FontStyle
textTagStyle = newAttrFromEnumProperty "style"
  {# call pure unsafe pango_style_get_type #}

-- | Whether this tag affects the font style.
--
-- Default value: @False@
--
textTagStyleSet :: TextTagClass self => Attr self Bool
textTagStyleSet = newAttrFromBoolProperty "style-set"

-- | Custom tabs for this text.
-- textTagTabs :: TextTagClass self => Attr self TabArray

-- | Whether this tag affects tabs.
--
-- Default value: @False@
--
textTagTabsSet :: TextTagClass self => Attr self Bool
textTagTabsSet = newAttrFromBoolProperty "tabs-set"

-- | Font variant as a 'Variant', e.g. 'VariantSmallCaps'.
--
-- Default value: 'VariantNormal'
--
textTagVariant :: TextTagClass self => Attr self Variant
textTagVariant = newAttrFromEnumProperty "variant"
  {# call pure unsafe pango_variant_get_type #}

-- | Whether this tag affects the font variant.
--
-- Default value: @False@
--
textTagVariantSet :: TextTagClass self => Attr self Bool
textTagVariantSet = newAttrFromBoolProperty "variant-set"

-- | Font weight as an integer, see predefined values in 'Graphics.Rendering.Pango.Enums.Weight'; for
-- example, 'Graphics.Rendering.Pango.Enums.WeightBold'.
--
-- Allowed values: >= 0
--
-- Default value: 400
--
textTagWeight :: TextTagClass self => Attr self Int
textTagWeight = newAttrFromIntProperty "weight"

-- | Whether this tag affects the font weight.
--
-- Default value: @False@
--
textTagWeightSet :: TextTagClass self => Attr self Bool
textTagWeightSet = newAttrFromBoolProperty "weight-set"

-- | Font stretch as a 'Stretch', e.g. 'StretchCondensed'.
--
-- Default value: 'StretchNormal'
--
textTagStretch :: TextTagClass self => Attr self Stretch
textTagStretch = newAttrFromEnumProperty "stretch"
  {# call pure unsafe pango_stretch_get_type #}

-- | Whether this tag affects the font stretch.
textTagStretchSet :: TextTagClass self => Attr self Bool
textTagStretchSet = newAttrFromBoolProperty "stretch-set"

-- | Font size in Pango units.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textTagSize :: TextTagClass self => Attr self Int
textTagSize = newAttrFromIntProperty "size"

-- | Whether this tag affects the font size.
--
-- Default value: @False@
--
textTagSizeSet :: TextTagClass self => Attr self Bool
textTagSizeSet = newAttrFromBoolProperty "size-set"

-- | Font size as a scale factor relative to the default font size. This
-- properly adapts to theme changes etc. so is recommended.
--
-- Allowed values: >= 0
--
-- Default value: 1
--
textTagScale :: TextTagClass self => Attr self Double
textTagScale = newAttrFromDoubleProperty "scale"

-- | Whether this tag scales the font size by a factor.
--
-- Default value: @False@
--
textTagScaleSet :: TextTagClass self => Attr self Bool
textTagScaleSet = newAttrFromBoolProperty "scale-set"

-- | Font size in points.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textTagSizePoints :: TextTagClass self => Attr self Double
textTagSizePoints = newAttrFromDoubleProperty "size-points"

-- | Left, right, or center justification.
--
-- Default value: 'JustifyLeft'
--
textTagJustification :: TextTagClass self => Attr self Justification
textTagJustification = newAttrFromEnumProperty "justification"
  {# call pure unsafe gtk_justification_get_type #}

-- | Whether this tag affects paragraph justification.
--
-- Default value: @False@
--
textTagJustificationSet :: TextTagClass self => Attr self Bool
textTagJustificationSet = newAttrFromBoolProperty "justification-set"

-- | The language this text is in, as an ISO code. Pango can use this as a
-- hint when rendering the text. If not set, an appropriate default will be
-- used.
--
-- Default value: \"\"
--
textTagLanguage :: (TextTagClass self, GlibString string) => Attr self string
textTagLanguage = newAttrFromStringProperty "language"

-- | Whether this tag affects the language the text is rendered as.
--
-- Default value: @False@
--
textTagLanguageSet :: TextTagClass self => Attr self Bool
textTagLanguageSet = newAttrFromBoolProperty "language-set"

-- | Width of the left margin in pixels.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textTagLeftMargin :: TextTagClass self => Attr self Int
textTagLeftMargin = newAttrFromIntProperty "left-margin"

-- | Whether this tag affects the left margin.
--
-- Default value: @False@
--
textTagLeftMarginSet :: TextTagClass self => Attr self Bool
textTagLeftMarginSet = newAttrFromBoolProperty "left-margin-set"

-- | Width of the right margin in pixels.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textTagRightMargin :: TextTagClass self => Attr self Int
textTagRightMargin = newAttrFromIntProperty "right-margin"

-- | Whether this tag affects the right margin.
--
-- Default value: @False@
--
textTagRightMarginSet :: TextTagClass self => Attr self Bool
textTagRightMarginSet = newAttrFromBoolProperty "right-margin-set"

-- | Amount to indent the paragraph, in pixels.
--
-- Default value: 0
--
textTagIndent :: TextTagClass self => Attr self Int
textTagIndent = newAttrFromIntProperty "indent"

-- | Whether this tag affects indentation.
--
-- Default value: @False@
--
textTagIndentSet :: TextTagClass self => Attr self Bool
textTagIndentSet = newAttrFromBoolProperty "indent-set"

-- | Offset of text above the baseline (below the baseline if rise is
-- negative) in pixels.
--
-- Default value: 0
--
textTagRise :: TextTagClass self => Attr self Int
textTagRise = newAttrFromIntProperty "rise"

-- | Whether this tag affects the rise.
textTagRiseSet :: TextTagClass self => Attr self Bool
textTagRiseSet = newAttrFromBoolProperty "rise-set"

-- | Pixels of blank space above paragraphs.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textTagPixelsAboveLines :: TextTagClass self => Attr self Int
textTagPixelsAboveLines = newAttrFromIntProperty "pixels-above-lines"

-- | Whether this tag affects the number of pixels above lines.
--
-- Default value: @False@
--
textTagPixelsAboveLinesSet :: TextTagClass self => Attr self Bool
textTagPixelsAboveLinesSet = newAttrFromBoolProperty "pixels-above-lines-set"

-- | Pixels of blank space below paragraphs.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textTagPixelsBelowLines :: TextTagClass self => Attr self Int
textTagPixelsBelowLines = newAttrFromIntProperty "pixels-below-lines"

-- | Whether this tag affects the number of pixels below lines.
--
-- Default value: @False@
--
textTagPixelsBelowLinesSet :: TextTagClass self => Attr self Bool
textTagPixelsBelowLinesSet = newAttrFromBoolProperty "pixels-below-lines-set"

-- | Pixels of blank space between wrapped lines in a paragraph.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textTagPixelsInsideWrap :: TextTagClass self => Attr self Int
textTagPixelsInsideWrap = newAttrFromIntProperty "pixels-inside-wrap"

-- | Whether this tag affects the number of pixels between wrapped lines.
--
-- Default value: @False@
--
textTagPixelsInsideWrapSet :: TextTagClass self => Attr self Bool
textTagPixelsInsideWrapSet = newAttrFromBoolProperty "pixels-inside-wrap-set"

-- | Whether to strike through the text.
--
-- Default value: @False@
--
textTagStrikethrough :: TextTagClass self => Attr self Bool
textTagStrikethrough = newAttrFromBoolProperty "strikethrough"

-- | Whether this tag affects strikethrough.
--
-- Default value: @False@
--
textTagStrikethroughSet :: TextTagClass self => Attr self Bool
textTagStrikethroughSet = newAttrFromBoolProperty "strikethrough-set"

-- | Style of underline for this text.
--
-- Default value: 'UnderlineNone'
--
textTagUnderline :: TextTagClass self => Attr self Underline
textTagUnderline = newAttrFromEnumProperty "underline"
  {# call pure unsafe pango_underline_get_type #}

-- | Whether this tag affects underlining.
--
-- Default value: @False@
--
textTagUnderlineSet :: TextTagClass self => Attr self Bool
textTagUnderlineSet = newAttrFromBoolProperty "underline-set"

-- | Whether to wrap lines never, at word boundaries, or at character
-- boundaries.
--
-- Default value: 'WrapNone'
--
textTagWrapMode :: TextTagClass self => Attr self WrapMode
textTagWrapMode = newAttrFromEnumProperty "wrap-mode"
  {# call pure unsafe gtk_wrap_mode_get_type #}

-- | Whether this tag affects line wrap mode.
--
-- Default value: @False@
--
textTagWrapModeSet :: TextTagClass self => Attr self Bool
textTagWrapModeSet = newAttrFromBoolProperty "wrap-mode-set"

#if GTK_CHECK_VERSION(2,8,0)
-- | Whether this text is hidden.
--
-- Note that there may still be problems with the support for invisible
-- text, in particular when navigating programmatically inside a buffer
-- containing invisible segments.
--
-- Default value: @False@
--
textTagInvisible :: TextTagClass self => Attr self Bool
textTagInvisible = newAttrFromBoolProperty "invisible"

-- | Whether this tag affects text visibility.
--
-- Default value: @False@
--
textTagInvisibleSet :: TextTagClass self => Attr self Bool
textTagInvisibleSet = newAttrFromBoolProperty "invisible-set"

-- | The paragraph background color as a string.
--
-- Default value: \"\"
--
textTagParagraphBackground :: (TextTagClass self, GlibString string) => WriteAttr self string
textTagParagraphBackground = writeAttrFromStringProperty "paragraph-background"

-- | Whether this tag affects the paragraph background color.
--
-- Default value: @False@
--
textTagParagraphBackgroundSet :: TextTagClass self => Attr self Bool
textTagParagraphBackgroundSet = newAttrFromBoolProperty "paragraph-background-set"

-- | The paragraph background color as a as a (possibly unallocated) 'Color'.
--
textTagParagraphBackgroundGdk :: TextTagClass self => Attr self Color
textTagParagraphBackgroundGdk =
  newAttrFromBoxedStorableProperty "paragraph-background-gdk"
  {# call pure unsafe gdk_color_get_type #}
#endif

-- | \'priority\' property. See 'textTagGetPriority' and 'textTagSetPriority'
--
textTagPriority :: TextTagClass self => Attr self Int
textTagPriority = newAttr
  textTagGetPriority
  textTagSetPriority

--------------------
-- Signals

-- the following signal only really makes sense if the EventM module provides dynamic upcast
-- functions since the user must test what kind of event has been delivered.

-- | An event has occurred that affects the given tag.
--
-- * Adding an event handler to the tag makes it possible to react on
--   e.g. mouse clicks to implement hyperlinking.
--
-- * The first argument is the object the event was fired from (typically a 'TextView').
--   The second argument is the iterator indicating where the event happened.
--
textTagEvent :: TextTagClass self => Signal self (GObject -> TextIter -> EventM EAny Bool)
textTagEvent = Signal (\after obj fun ->
                       connect_OBJECT_PTR_BOXED__BOOL "event" mkTextIterCopy after obj
                         (\tv eventPtr iter -> runReaderT (fun tv iter) eventPtr)
                      )

--------------------
-- Deprecated Signals and Events

#ifndef DISABLE_DEPRECATED

-- | An event has occurred that affects the given tag.
--
-- * Adding an event handler to the tag makes it possible to react on
--   e.g. mouse clicks to implement hyperlinking.
--
onTextTagEvent :: TextTagClass t => t -> (Event -> TextIter -> IO ()) ->
                  IO (ConnectId t)
onTextTagEvent tt act =
  connect_PTR_BOXED_BOXED__BOOL "event" marshalEvent mkTextIterCopy False tt
    (\_ event iter -> act event iter >> return False)

#endif
