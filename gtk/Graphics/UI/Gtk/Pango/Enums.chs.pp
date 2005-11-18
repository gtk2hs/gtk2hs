-- -*-haskell-*-
--  GIMP Toolkit (GTK) Enumerations for Pango.
--
--  Author : Axel Simon
--
--  Created: 12 September 2004
--
--  Version $Revision: 1.10 $ from $Date: 2005/11/18 15:41:07 $
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
-- Enumerations for describing font characteristics.
--
module Graphics.UI.Gtk.Pango.Enums (
  FontStyle(..),
  Weight(..),
  Variant(..),
  Stretch(..),
  Underline(..),
#if PANGO_CHECK_VERSION(1,6,0)
  EllipsizeMode(..)
#endif
  ) where

{# context lib="pango" prefix="pango" #}

-- | The style of a font.
--
-- * 'StyleOblique' is a slanted font like 'StyleItalic', 
--   but in a roman style.
--
{#enum Style as FontStyle {underscoreToCase} deriving (Eq)#}

instance Show FontStyle where
  showsPrec _ StyleNormal	   = shows "normal"
  showsPrec _ StyleOblique	   = shows "oblique"
  showsPrec _ StyleItalic	   = shows "italic"

-- | Define attributes for 'Weight'.
--
{#enum Weight {underscoreToCase} deriving (Eq)#}

instance Show Weight where
  showsPrec _ WeightUltralight	= shows "ultralight"
  showsPrec _ WeightLight	= shows "light"
  showsPrec _ WeightNormal	= shows "normal"
  showsPrec _ WeightBold	= shows "bold"
  showsPrec _ WeightUltrabold 	= shows "ultrabold"
  showsPrec _ WeightHeavy	= shows "heavy"

-- | The variant of a font.
--
-- * The 'VariantSmallCaps' is a version of a font where lower case
--   letters are shown as physically smaller upper case letters.
--
{#enum Variant {underscoreToCase} deriving (Eq)#}

instance Show Variant where
  showsPrec _ VariantNormal       = shows "normal"
  showsPrec _ VariantSmallCaps    = shows "smallcaps"

-- | Define how wide characters are.
--
{#enum Stretch {underscoreToCase} deriving (Eq)#}

instance Show Stretch where
  showsPrec _ StretchUltraCondensed	= shows "ultracondensed"
  showsPrec _ StretchExtraCondensed	= shows "extracondensed"
  showsPrec _ StretchCondensed		= shows "condensed"
  showsPrec _ StretchSemiCondensed	= shows "semicondensed"
  showsPrec _ StretchNormal		= shows "normal"
  showsPrec _ StretchSemiExpanded	= shows "semiexpanded"
  showsPrec _ StretchExpanded		= shows "expanded"
  showsPrec _ StretchExtraExpanded	= shows "extraexpanded"
  showsPrec _ StretchUltraExpanded	= shows "ultraexpanded"

-- | Define attributes for 'Underline'.
--
-- * The squiggly underline for errors is only available in Gtk 2.4 and higher.
--
{#enum Underline {underscoreToCase} deriving (Eq)#}

instance Show Underline where
  showsPrec _ UnderlineNone	= shows "none"
  showsPrec _ UnderlineSingle	= shows "single"
  showsPrec _ UnderlineDouble	= shows "double"
  showsPrec _ UnderlineLow	= shows "low"
#if GTK_CHECK_VERSION(2,4,0)
  showsPrec _ UnderlineError	= shows "error"
#endif

#if PANGO_CHECK_VERSION(1,6,0)
-- | The 'EllipsizeMode' type describes what sort of (if any) ellipsization
-- should be applied to a line of text. In the ellipsization process characters
-- are removed from the text in order to make it fit to a given width and
-- replaced with an ellipsis.
--
{# enum EllipsizeMode {underscoreToCase} deriving (Eq) #}
#endif

