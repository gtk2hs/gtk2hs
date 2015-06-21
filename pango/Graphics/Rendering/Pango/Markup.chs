{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Markup
--
--  Author : Axel Simon
--
--  Created: 5 June 2001
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- This module defines some helper functions for generating texts with
-- embedded attributes. Note that there is no need to use these functions.
-- In particular, if you set markup in labels that are subject to
-- internationalization, it can be of advantage to write out the markup
-- instead of using the functions in this module.
--
-- In order to display a string that may contain markup characters, use
-- 'Graphics.UI.Gtk.Pango.Layout.escapeMarkup'.
--
-- When you write markup directly, you can make use of the following
-- convenience tags:
--
-- [@b@] Bold
--
-- [@big@] Makes font relatively larger
--
-- [@i@] Italic
--
-- [@s@] Strikethrough
--
-- [@sub@] Subscript
--
-- [@sup@] Superscript
--
-- [@small@] Makes font relatively smaller
--
-- [@tt@] Monospace font
--
-- [@u@] Underline
--
module Graphics.Rendering.Pango.Markup (
  SpanAttribute(..),
  markSpan,
  parseMarkup
  ) where

import qualified Graphics.Rendering.Pango.Enums as Pango
import Graphics.Rendering.Pango.Attributes ( parseMarkup )

-- | These are all the attributes the 'markSpan' function can express.
--
data SpanAttribute
  -- | Choose a font by textual description.
  --
  -- * Takes a string to completely describe the font, example:
  -- @FontDescr@ \"Sans Italic 12\"
  = FontDescr   String

  -- | Specify the family of font to use.
  --
  -- * Example: @FontFamily@ \"Sans\"
  | FontFamily  String

  -- | Change the size of the current font.
  --
  -- * The constuctor takes the size in points (pt) or a predefined
  --   sizes. Setting the absolute size 12.5pt can be achieved by passing
  --   'FontSize' ('SizePoint' 12.5) to 'markSpan'. Next to predefined
  --   absolute sizes such as 'Pango.SizeSmall' the size can be changed by
  --   asking for the next larger or smaller front with
  --   'Pango.SizeLarger' and 'Pango.SizeSmaller', respectively.
  | FontSize Pango.Size

  -- | Change the slant of the current font.
  --
  | FontStyle Pango.FontStyle

  -- | Change the thickness of the current font.
  --
  -- * The constructor takes one of the six predefined weights. Most likely to
  --   be supported: 'Pango.WeightBold'.
  --
  | FontWeight Pango.Weight

  -- | Choosing an alternative rendering for lower case letters.
  --
  -- * The argument 'Pango.VariantSmallCaps' will display lower case letters
  --   as smaller upper case letters, if this option is available.
  | FontVariant Pango.Variant

  -- | Choose a different width.
  --
  -- * Takes one of nine font widths, e.g. 'Pango.WidthExpanded'.
  --
  | FontStretch Pango.Stretch

  -- | Foreground color.
  --
  -- * This constructor and 'FontBackground' take both a description
  --   of the color to be used for rendering. The name is either a
  --   hex code of the form \"#RRGGBB\" or an X11 color name like
  --   \"dark olive green\".
  --
  | FontForeground String       -- FIXME: should be ColorName from GDK or so

  -- | Background color.
  | FontBackground String

  -- | Specify underlining of text.
  --
  | FontUnderline Pango.Underline

  -- | Specify a vertical displacement.
  --
  -- * Takes the vertical displacement in em (the width of the \'m\' character
  --   in the current font).
  | FontRise    Double

  -- | Give a hint about the language to be displayed.
  --
  -- * This hint might help the system rendering a particular piece of text
  --   with different fonts that are more suitable for the given language.
  --
  | FontLang    Pango.Language

#if PANGO_VERSION_CHECK(1,16,0)
  -- | Gravity of text, use for ratation.
  | FontGravity Pango.PangoGravity

  -- | Intensity of gravity.
  | FontGravityHint Pango.PangoGravityHint
#endif

instance Show SpanAttribute where
  showsPrec _ (FontDescr str)    = showString " font_desc=".shows str
  showsPrec _ (FontFamily str)   = showString " font_family=".shows str
  showsPrec _ (FontSize size)    = showString " size=".shows size
  showsPrec _ (FontStyle style)  = showString " style=".shows style
  showsPrec _ (FontWeight w)     = showString " weight=".shows w
  showsPrec _ (FontVariant v)    = showString " variant=".shows v
  showsPrec _ (FontStretch s)    = showString " stretch=".shows s
  showsPrec _ (FontForeground c) = showString " foreground=".shows c
  showsPrec _ (FontBackground c) = showString " background=".shows c
  showsPrec _ (FontUnderline u)  = showString " underline=".shows u
  showsPrec _ (FontRise r)       = showString " rise=".shows
                                   (show (round (r*10000)))
  showsPrec _ (FontLang l)       = showString " lang=".shows l
#if PANGO_VERSION_CHECK(1,16,0)
  showsPrec _ (FontGravity g) = showString " gravity=".shows g
  showsPrec _ (FontGravityHint h) = showString " gravity_hint".shows h
#endif

-- | Create the most generic span attribute.
--
markSpan :: [SpanAttribute] -> String -> String
markSpan attrs text = showString "<span".
                      foldr (.) (showChar '>') (map shows attrs).
                      showString text.
                      showString "</span>" $ ""

