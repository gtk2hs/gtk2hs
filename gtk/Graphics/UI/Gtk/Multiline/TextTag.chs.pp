-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget TextTag
--
--  Author : Duncan Coutts
--
--  Created: 4 August 2004
--
--  Version $Revision: 1.9 $ from $Date: 2005/08/25 01:16:15 $
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
-- accessor functions for TextAttributes
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
-- Tags should be in the 'TextTagTable' for a given 'TextBuffer' before
-- using them with that buffer.
--
-- 'textBufferCreateTag' is the best way to create tags.
--
-- The \"invisible\" property was not implemented for Gtk+ 2.0; it's planned
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
  castToTextTag,
  TagName,

-- * Constructors
  textTagNew,

-- * Methods
  textTagSetPriority,
  textTagGetPriority,
  TextAttributes(..),
  textAttributesNew,
  makeNewTextAttributes,  --internal

-- * Attributes
  textTagName,
  textTagBackground,
  textTagBackgroundFullHeight,
  textTagBackgroundStipple,
  textTagForeground,
  textTagForegroundStipple,
  textTagDirection,
  textTagEditable,
  textTagFont,
  textTagFamily,
  textTagStyle,
  textTagVariant,
  textTagWeight,
  textTagStretch,
  textTagSize,
  textTagScale,
  textTagSizePoints,
  textTagJustification,
  textTagLanguage,
  textTagLeftMargin,
  textTagRightMargin,
  textTagIndent,
  textTagRise,
  textTagPixelsAboveLines,
  textTagPixelsBelowLines,
  textTagPixelsInsideWrap,
  textTagStrikethrough,
  textTagUnderline,
  textTagWrapMode,
#if GTK_CHECK_VERSION(2,8,0)
  textTagInvisible,
  textTagParagraphBackground,
#endif
  textTagPriority,
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GObject		(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.Pango.Enums	(FontStyle, Variant, Stretch, Underline)
import Graphics.UI.Gtk.General.Enums	(TextDirection, Justification, WrapMode)

{# context lib="gtk" prefix="gtk" #}

type TagName = String

--------------------
-- Constructors

-- | Creates a 'TextTag'.
--
textTagNew :: TagName -> IO TextTag
textTagNew name =
  makeNewGObject mkTextTag $
  withCString name $ \namePtr ->
  {# call unsafe text_tag_new #}
    namePtr

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
-- to one less than 'textTagTableGetSize'. Each tag in a table has a unique
-- priority; setting the priority of one tag shifts the priorities of all the
-- other tags in the table to maintain a unique priority for each tag. Higher
-- priority tags \"win\" if two tags both set the same text attribute. When
-- adding a tag to a tag table, it will be assigned the highest priority in the
-- table by default; so normally the precedence of a set of tags is the order
-- in which they were added to the table, or created with
-- 'textBufferCreateTag', which adds the tag to the buffer's table
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

makeNewTextAttributes :: Ptr TextAttributes -> IO TextAttributes
makeNewTextAttributes ptr =
  liftM TextAttributes $ newForeignPtr ptr (text_attributes_unref ptr)

#if __GLASGOW_HASKELL__>=600
                                                                                
foreign import ccall unsafe "&gtk_text_attributes_unref"
  text_attributes_unref' :: FinalizerPtr TextAttributes
                                                                                
text_attributes_unref :: Ptr TextAttributes -> FinalizerPtr TextAttributes
text_attributes_unref _ = text_attributes_unref'
                                                                                
#else
                                                                                
foreign import ccall unsafe "gtk_text_attributes_unref"
  text_attributes_unref :: Ptr TextAttributes -> IO ()
                                                                                
#endif

--------------------
-- Attributes

-- | Name used to refer to the text tag. @Nothing@ for anonymous tags.
--
-- Default value: @Nothing@
--
textTagName :: TextTagClass self => Attr self (Maybe String)
textTagName = newAttrFromMaybeStringProperty "name"

-- | Background color as a string.
--
-- Default value: \"\"
--
textTagBackground :: TextTagClass self => WriteAttr self String
textTagBackground = writeAttrFromStringProperty "background"

-- | Whether the background color fills the entire line height or only the
-- height of the tagged characters.
--
-- Default value: @False@
--
textTagBackgroundFullHeight :: TextTagClass self => Attr self Bool
textTagBackgroundFullHeight = newAttrFromBoolProperty "background-full-height"

-- | Bitmap to use as a mask when drawing the text background.
--
textTagBackgroundStipple :: (TextTagClass self, PixmapClass pixmap) => ReadWriteAttr self Pixmap pixmap
textTagBackgroundStipple = newAttrFromObjectProperty "background-stipple"

-- | Foreground color as a string.
--
-- Default value: \"\"
--
textTagForeground :: TextTagClass self => WriteAttr self String
textTagForeground = writeAttrFromStringProperty "foreground"

-- | Bitmap to use as a mask when drawing the text foreground.
--
textTagForegroundStipple :: (TextTagClass self, PixmapClass pixmap) => ReadWriteAttr self Pixmap pixmap
textTagForegroundStipple = newAttrFromObjectProperty "foreground-stipple"

-- | Text direction, e.g. right-to-left or left-to-right.
--
-- Default value: 'TextDirLtr'
--
textTagDirection :: TextTagClass self => Attr self TextDirection
textTagDirection = newAttrFromEnumProperty "direction"

-- | Whether the text can be modified by the user.
--
-- Default value: @True@
--
textTagEditable :: TextTagClass self => Attr self Bool
textTagEditable = newAttrFromBoolProperty "editable"

-- | Font description as a string, e.g. \"Sans Italic 12\".
--
-- Default value: \"\"
--
textTagFont :: TextTagClass self => Attr self String
textTagFont = newAttrFromStringProperty "font"

-- | Name of the font family, e.g. Sans, Helvetica, Times, Monospace.
--
-- Default value: \"\"
--
textTagFamily :: TextTagClass self => Attr self String
textTagFamily = newAttrFromStringProperty "family"

-- | Font style as a 'Style', e.g. 'StyleItalic'.
--
-- Default value: 'StyleNormal'
--
textTagStyle :: TextTagClass self => Attr self FontStyle
textTagStyle = newAttrFromEnumProperty "style"

-- | Font variant as a 'Variant', e.g. 'VariantSmallCaps'.
--
-- Default value: 'VariantNormal'
--
textTagVariant :: TextTagClass self => Attr self Variant
textTagVariant = newAttrFromEnumProperty "variant"

-- | Font weight as an integer, see predefined values in 'Weight'; for
-- example, 'WeightBold'.
--
-- Allowed values: >= 0
--
-- Default value: 400
--
textTagWeight :: TextTagClass self => Attr self Int
textTagWeight = newAttrFromIntProperty "weight"

-- | Font stretch as a 'Stretch', e.g. 'StretchCondensed'.
--
-- Default value: 'StretchNormal'
--
textTagStretch :: TextTagClass self => Attr self Stretch
textTagStretch = newAttrFromEnumProperty "stretch"

-- | Font size in Pango units.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textTagSize :: TextTagClass self => Attr self Int
textTagSize = newAttrFromIntProperty "size"

-- | Font size as a scale factor relative to the default font size. This
-- properly adapts to theme changes etc. so is recommended.
--
-- Allowed values: >= 0
--
-- Default value: 1
--
textTagScale :: TextTagClass self => Attr self Double
textTagScale = newAttrFromDoubleProperty "scale"

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

-- | The language this text is in, as an ISO code. Pango can use this as a
-- hint when rendering the text. If not set, an appropriate default will be
-- used.
--
-- Default value: \"\"
--
textTagLanguage :: TextTagClass self => Attr self String
textTagLanguage = newAttrFromStringProperty "language"

-- | Width of the left margin in pixels.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textTagLeftMargin :: TextTagClass self => Attr self Int
textTagLeftMargin = newAttrFromIntProperty "left-margin"

-- | Width of the right margin in pixels.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textTagRightMargin :: TextTagClass self => Attr self Int
textTagRightMargin = newAttrFromIntProperty "right-margin"

-- | Amount to indent the paragraph, in pixels.
--
-- Default value: 0
--
textTagIndent :: TextTagClass self => Attr self Int
textTagIndent = newAttrFromIntProperty "indent"

-- | Offset of text above the baseline (below the baseline if rise is
-- negative) in pixels.
--
-- Default value: 0
--
textTagRise :: TextTagClass self => Attr self Int
textTagRise = newAttrFromIntProperty "rise"

-- | Pixels of blank space above paragraphs.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textTagPixelsAboveLines :: TextTagClass self => Attr self Int
textTagPixelsAboveLines = newAttrFromIntProperty "pixels-above-lines"

-- | Pixels of blank space below paragraphs.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textTagPixelsBelowLines :: TextTagClass self => Attr self Int
textTagPixelsBelowLines = newAttrFromIntProperty "pixels-below-lines"

-- | Pixels of blank space between wrapped lines in a paragraph.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textTagPixelsInsideWrap :: TextTagClass self => Attr self Int
textTagPixelsInsideWrap = newAttrFromIntProperty "pixels-inside-wrap"

-- | Whether to strike through the text.
--
-- Default value: @False@
--
textTagStrikethrough :: TextTagClass self => Attr self Bool
textTagStrikethrough = newAttrFromBoolProperty "strikethrough"

-- | Style of underline for this text.
--
-- Default value: 'UnderlineNone'
--
textTagUnderline :: TextTagClass self => Attr self Underline
textTagUnderline = newAttrFromEnumProperty "underline"

-- | Whether to wrap lines never, at word boundaries, or at character
-- boundaries.
--
-- Default value: 'WrapNone'
--
textTagWrapMode :: TextTagClass self => Attr self WrapMode
textTagWrapMode = newAttrFromEnumProperty "wrap-mode"

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

-- | The paragraph background color as a string.
--
-- Default value: ""
--
textTagParagraphBackground :: TextTagClass self => WriteAttr self String
textTagParagraphBackground = writeAttrFromStringProperty "paragraph-background"
#endif

-- | \'priority\' property. See 'textTagGetPriority' and 'textTagSetPriority'
--
textTagPriority :: TextTagClass self => Attr self Int
textTagPriority = newAttr
  textTagGetPriority
  textTagSetPriority
