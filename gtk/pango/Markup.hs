--  GIMP Toolkit (GTK) Markup
--
--  Author : Axel Simon
--          
--  Created: 5 June 2001
--
--  Version $Revision: 1.6 $ from $Date: 2004/05/23 16:12:20 $
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
  FontSizeDef(..),
  FontStyleDef(..),
  FontWeightDef(..),
  FontVariantDef(..),
  FontStretchDef(..),
  FontUnderlineDef(..),
  markSpan
  ) where


-- | Define a synonym for text with embedded markup commands.
--
-- * Markup strings are just simple strings. But it's easier to tell if a
--   method expects text with or without markup.
--
type Markup = String

-- | These are all the attributes the
-- 'markSpan' function can express.
--
-- DOCFIXME(constructor): FontDescr Choose a font by textual description.
--
-- * Takes a
--   string to completely describe the font, example: 'FontDescr' 
--   @"Sans Italic 12"@
--
-- DOCFIXME(constructor): FontFamily Specify the family of font to use.
--
-- * Example: 'FontFamily' @"Sans"@
--
-- DOCFIXME(constructor): FontSize Change the size of the current font.
--
-- * The constuctor takes the size in points (pt) or as predefined
--   sizes. Setting the absolute size 12.5pt can be achieved by passing
--   'FontSize' @('FSPoint' 
--   @12.5)@ to 'markSpan'. Next to predefined absolute
--   sizes such as 'FSsmall' the size can be changed by asking for
--   the next larger or smaller front with 'FSlarger' and
--   'FSsmaller', respectively.
--
-- DOCFIXME(constructor): FontStyle Change the slant of the current font.
--
-- * The constructor takes one of three styles: 'FYnormal',
--   'FYoblique' or 'FYitalic'.
--
-- DOCFIXME(constructor): FontWeight Change the thickness of the current font.
--
-- * The constructor takes one of the six predefined weights. Most likely to
--   be supported: 'FWbold'.
--
-- DOCFIXME(constructor): FontVariant Choosing an alternative rendering for lower case
-- letters.
--
-- * The argument 'FVsmallcaps' will display lower case letters
--   as smaller upper case letters, if this option is available.
--
-- DOCFIXME(constructor): FontStretch Choose a different width.
--
-- * Takes one of nine font widths, e.g. 'FTcondensed' or
--   'FTexpanded'.
--
-- DOCFIXME(constructor): FontForeground Foreground color.
--
-- * This constructor and 'FontBackground' take both a description
--   of the color to be used for rendering.
--
-- DOCFIXME(constructor): FontBackground Background color.
--
-- DOCFIXME(constructor): FontUnderline Specify underlining of text.
--
-- * 'FUnone', 'FUsingle', 'FUdouble' or
--   'FUlow' are possible choices.
--
-- DOCFIXME(constructor): FontRise Specify a vertical displacement.
--
-- * Takes the vertical displacement in em (the width of the \'m\' character
--   in the current font).
--
-- DOCFIXME(constructor): FontLang Give a hint about the language to be displayed.
--
data SpanAttribute
  = FontDescr   String
  | FontFamily	String
  | FontSize	FontSizeDef
  | FontStyle   FontStyleDef
  | FontWeight  FontWeightDef
  | FontVariant FontVariantDef
  | FontStretch FontStretchDef
  | FontForeground String	-- FIXME: should be ColorName from GDK or so
  | FontBackground String
  | FontUnderline FontUnderlineDef
  | FontRise	Double
  | FontLang	String		-- FIXME: enumeration? what's the use anyway?

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

-- | Define attributes for 'FontSize'.
--
data FontSizeDef
  = FSPoint Double
  | FSunreadable
  | FStiny
  | FSsmall
  | FSmedium
  | FSlarge
  | FShuge
  | FSgiant
  | FSsmaller
  | FSlarger

instance Show FontSizeDef where
  showsPrec _ (FSPoint v)         = shows $ show (round (v*1000))
  showsPrec _ (FSunreadable)	  = shows "xx-small"
  showsPrec _ (FStiny)		  = shows "x-small"
  showsPrec _ (FSsmall)		  = shows "small"
  showsPrec _ (FSmedium)	  = shows "medium"
  showsPrec _ (FSlarge)		  = shows "large"
  showsPrec _ (FShuge)		  = shows "x-large"
  showsPrec _ (FSgiant)		  = shows "xx-large"
  showsPrec _ (FSsmaller)	  = shows "smaller"
  showsPrec _ (FSlarger)	  = shows "larger"

-- | Define attributes for 'FontStyle'.
--
data FontStyleDef
  = FYnormal
  | FYoblique
  | FYitalic

instance Show FontStyleDef where
  showsPrec _ FYnormal	   = shows "normal"
  showsPrec _ FYoblique	   = shows "oblique"
  showsPrec _ FYitalic	   = shows "italic"

-- | Define attributes for 'FontWeight'.
--
data FontWeightDef
  = FWultralight
  | FWlight
  | FWnormal
  | FWbold
  | FWultrabold
  | FWheavy

instance Show FontWeightDef where
  showsPrec _ FWultralight = shows "ultralight"
  showsPrec _ FWlight	   = shows "light"
  showsPrec _ FWnormal	   = shows "normal"
  showsPrec _ FWbold	   = shows "bold"
  showsPrec _ FWultrabold  = shows "ultrabold"
  showsPrec _ FWheavy	   = shows "heavy"

-- | Define attributes for 'FontVariant'.
--
data FontVariantDef
  = FVnormal
  | FVsmallcaps

instance Show FontVariantDef where
  showsPrec _ FVnormal	     = shows "normal"
  showsPrec _ FVsmallcaps    = shows "smallcaps"

-- | Define attributes for 'FontStretch'.
--
data FontStretchDef
  = FTultracondensed
  | FTextracondensed
  | FTcondensed
  | FTsemicondensed
  | FTnormal
  | FTsemiexpanded
  | FTexpanded
  | FTextraexpanded
  | FTultraexpanded

instance Show FontStretchDef where
  showsPrec _ FTultracondensed	= shows "ultracondensed"
  showsPrec _ FTextracondensed	= shows "extracondensed"
  showsPrec _ FTcondensed	= shows "condensed"
  showsPrec _ FTsemicondensed	= shows "semicondensed"
  showsPrec _ FTnormal		= shows "normal"
  showsPrec _ FTsemiexpanded	= shows "semiexpanded"
  showsPrec _ FTexpanded	= shows "expanded"
  showsPrec _ FTextraexpanded	= shows "extraexpanded"
  showsPrec _ FTultraexpanded	= shows "ultraexpanded"

-- | Define attributes for 'FontUnderline'.
--
data FontUnderlineDef
  = FUsingle
  | FUdoulbe
  | FUlow
  | FUnone

instance Show FontUnderlineDef where
  showsPrec _ FUsingle	= shows "single"
  showsPrec _ FUdoulbe	= shows "doulbe"
  showsPrec _ FUlow	= shows "low"
  showsPrec _ FUnone	= shows "none"

-- | Create the most generic span attribute.
--
markSpan :: [SpanAttribute] -> String -> String
markSpan attrs text = showString "<span".
		      foldr (.) (showChar '>') (map shows attrs).
		      showString text.
		      showString "</span>" $ ""



