--  GIMP Toolkit (GTK) Markup
--
--  Author : Axel Simon
--          
--  Created: 5 June 2001
--
--  Version $Revision: 1.8 $ from $Date: 2004/12/12 11:18:41 $
--
--  Copyright (c) 1999..2002 Axel Simon
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- |
--
-- This module defines some helper functions for generating texts with
-- embedded attributes.
--
-- TODO
--
-- * Add a numeric value to 'FontWeightDef'.
--
module Markup(
  Markup,
  SpanAttribute(..),
  markSpan,
  Size(..)
  ) where

import PangoTypes ( Language )
import qualified PangoEnums as Pango

-- | Define a synonym for text with embedded markup commands.
--
-- * Markup strings are just simple strings. But it's easier to tell if a
--   method expects text with or without markup.
--
type Markup = String

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
  | FontFamily	String

  -- | Change the size of the current font.
  --
  -- * The constuctor takes the size in points (pt) or a predefined
  --   sizes. Setting the absolute size 12.5pt can be achieved by passing
  --   'FontSize' ('SizePoint' 12.5) to 'markSpan'. Next to predefined
  --   absolute sizes such as 'SizeSmall' the size can be changed by 
  --   asking for the next larger or smaller front with
  --   'SizeLarger' and 'SizeSmaller', respectively.
  | FontSize Size

  -- | Change the slant of the current font.
  --
  | FontStyle Pango.FontStyle

  -- | Change the thickness of the current font.
  --
  -- * The constructor takes one of the six predefined weights. Most likely to
  --   be supported: 'WeightBold'.
  --
  | FontWeight Pango.Weight

  -- | Choosing an alternative rendering for lower case letters.
  --
  -- * The argument 'VariangtSmallCaps' will display lower case letters
  --   as smaller upper case letters, if this option is available.
  | FontVariant Pango.Variant

  -- | Choose a different width.
  --
  -- * Takes one of nine font widths, e.g. 'WidthExpanded'.
  --
  | FontStretch Pango.Stretch

  -- | Foreground color.
  --
  -- * This constructor and 'FontBackground' take both a description
  --   of the color to be used for rendering.
  | FontForeground String	-- FIXME: should be ColorName from GDK or so

  -- | Background color.
  | FontBackground String

  -- | Specify underlining of text.
  --
  | FontUnderline Pango.Underline

  -- | Specify a vertical displacement.
  --
  -- * Takes the vertical displacement in em (the width of the \'m\' character
  --   in the current font).
  | FontRise	Double

  -- | Give a hint about the language to be displayed.
  --
  -- * This hint might help the system rendering a particular piece of text
  --   with different fonts that are more suitable for the given language.
  --
  | FontLang	Language

instance Show SpanAttribute where
  showsPrec _ (FontDescr str)    = showString " font_desc=".shows str
  showsPrec _ (FontFamily str)	 = showString " font_family=".shows str
  showsPrec _ (FontSize size)	 = showString " size=".shows size
  showsPrec _ (FontStyle style)  = showString " style=".shows style
  showsPrec _ (FontWeight w)	 = showString " weight=".shows w
  showsPrec _ (FontVariant v)	 = showString " variant=".shows v
  showsPrec _ (FontStretch s)	 = showString " stretch=".shows s
  showsPrec _ (FontForeground c) = showString " foreground=".shows c
  showsPrec _ (FontBackground c) = showString " background=".shows c
  showsPrec _ (FontUnderline u)	 = showString " underline=".shows u
  showsPrec _ (FontRise r)	 = showString " rise=".shows 
				   (show (round (r*10000)))
  showsPrec _ (FontLang l)	 = showString " lang=".shows l

-- | Create the most generic span attribute.
--
markSpan :: [SpanAttribute] -> String -> String
markSpan attrs text = showString "<span".
		      foldr (.) (showChar '>') (map shows attrs).
		      showString text.
		      showString "</span>" $ ""

-- | Define attributes for 'FontSize'.
--
data Size
  = SizePoint Double
  | SizeUnreadable
  | SizeTiny
  | SizeSmall
  | SizeMedium
  | SizeLarge
  | SizeHuge
  | SizeGiant
  | SizeSmaller
  | SizeLarger

instance Show Size where
  showsPrec _ (SizePoint v)        	= shows $ show (round (v*1000))
  showsPrec _ (SizeUnreadable)		= shows "xx-small"
  showsPrec _ (SizeTiny)		= shows "x-small"
  showsPrec _ (SizeSmall)		= shows "small"
  showsPrec _ (SizeMedium)		= shows "medium"
  showsPrec _ (SizeLarge)		= shows "large"
  showsPrec _ (SizeHuge)		= shows "x-large"
  showsPrec _ (SizeGiant)		= shows "xx-large"
  showsPrec _ (SizeSmaller)		= shows "smaller"
  showsPrec _ (SizeLarger)	  	= shows "larger"




