--  GIMP Toolkit (GTK) @entry Markup@
--
--  Author : Axel Simon
--          
--  Created: 5 June 2001
--
--  Version $Revision: 1.4 $ from $Date: 2002/11/08 10:39:21 $
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
-- @description@ --------------------------------------------------------------
--
-- * This module defines some helper functions for generating texts with
--   embedded attributes.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------
--
-- * Add a numeric value to @ref data FontWeightDef@.
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


-- @type Markup@ Define a synonym for text with embedded markup commands.
--
-- * Markup strings are just simple strings. But its easier to tell if a
--   method expecs text with or without markup.
--
type Markup = String

-- @data SpanAttribute@ These are all the attributes the
-- @ref function markSpan@ function can express.
--
-- @variant FontDescr@ Choose a font by textual description.
--
-- * Takes a
--   string to completely describe the font, example: @ref variant FontDescr@ 
--   @literal "Sans Italic 12"@
--
-- @variant FontFamily@ Specify the family of font to use.
--
-- * Example: @ref variant FontFamily@ @literal "Sans"@
--
-- @variant FontSize@ Change the size of the current font.
--
-- * The constuctor takes the size in points (pt) or as predefined
--   sizes. Setting the absolute size 12.5pt can be achieved by passing
--   @ref variant FontSize@ @literal (@ @ref variant FSPoint@ 
--   @literal 12.5)@ to @ref function markSpan@. Next to predefined absolute
--   sizes such as @ref variant FSsmall@ the size can be changed by asking for
--   the next larger or smaller front with @ref variant FSlarger@ and
--   @ref variant FSsmaller@, respectively.
--
-- @variant FontStyle@ Change the slant of the current font.
--
-- * The constructor takes one of three styles: @ref variant FYnormal@,
--   @ref variant FYoblique@ or @ref variant FYitalic@.
--
-- @variant FontWeight@ Change the thickness of the current font.
--
-- * The constructor takes one of the six predefined weights. Most likely to
--   be supported: @ref variant FWbold@.
--
-- @variant FontVariant@ Choosing an alternative rendering for lower case
-- letters.
--
-- * The argument @ref variant FVsmallcaps@ will display lower case letters
--   as smaller upper case letters, if this option is available.
--
-- @variant FontStretch@ Choose a different width.
--
-- * Takes one of nine font widths, e.g. @ref variant FTcondensed@ or
--   @ref variant FTexpanded@.
--
-- @variant FontForeground@ Foreground color.
--
-- * This constructor and @ref variant FontBackground@ take both a description
--   of the color to be used for rendering.
--
-- @variant FontBackground@ Background color.
--
-- @variant FontUnderline@ Specify underlining of text.
--
-- * @ref variant FUnone@, @ref variant FUsingle@, @ref variant FUdouble@ or
--   @ref variant  FUlow@ are possible choices.
--
-- @variant FontRise@ Specify a vertical displacement.
--
-- * Takes the vertical displacement in em (the width of the 'm' character
--   in the current font).
--
-- @variant FontLang@ Give a hint about the language to be displayed.
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

-- @data FontSizeDef@ Define attributes for @ref type FontSize@.
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

-- @data FontStyleDef@ Define attributes for @ref type FontStyle@.
--
data FontStyleDef
  = FYnormal
  | FYoblique
  | FYitalic

instance Show FontStyleDef where
  showsPrec _ FYnormal	   = shows "normal"
  showsPrec _ FYoblique	   = shows "oblique"
  showsPrec _ FYitalic	   = shows "italic"

-- @data FontWeightDef@ Define attributes for @ref type FontWeight@.
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

-- @data FontVariantDef@ Define attributes for @ref type FontVariant@.
--
data FontVariantDef
  = FVnormal
  | FVsmallcaps

instance Show FontVariantDef where
  showsPrec _ FVnormal	     = shows "normal"
  showsPrec _ FVsmallcaps    = shows "smallcaps"

-- @data FontStretchDef@ Define attributes for @ref type FontStretch@.
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

-- @data FontUnderlineDef@ Define attributes for @ref type FontUnderline@.
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

-- @method markSpan@ Create the most generic span attribute.
--
markSpan :: [SpanAttribute] -> String -> String
markSpan attrs text = showString "<span".
		      foldr (.) (showChar '>') (map shows attrs).
		      showString text.
		      showString "</span>" $ ""



