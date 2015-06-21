{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Enumerations for Pango.
--
--  Author : Axel Simon
--
--  Created: 12 September 2004
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
module Graphics.Rendering.Pango.Enums (
  PangoUnit,
  pangoScale,
  Color(..),
  Rectangle(..),
  PangoRectangle(..),
  FontMetrics(..),
  Size(..),
  FontStyle(..),
  Weight(..),
  Variant(..),
  Stretch(..),
  Underline(..),
  PangoDirection(..),
  PangoAttribute(..),
#if PANGO_VERSION_CHECK(1,6,0)
  EllipsizeMode(..),
#endif
#if PANGO_VERSION_CHECK(1,16,0)
  PangoGravity(..),
  PangoGravityHint(..),
#endif

  -- these will not be exported from this module in the future
  Language,
  emptyLanguage,
  languageFromString,
  ) where

{#import Graphics.Rendering.Pango.Types#}
{#import Graphics.Rendering.Pango.BasicTypes#}
import Graphics.Rendering.Pango.Structs

{# context lib="pango" prefix="pango" #}

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
  showsPrec _ (SizePoint v)             = shows $ show (round (v*1000))
  showsPrec _ (SizeUnreadable)          = shows "xx-small"
  showsPrec _ (SizeTiny)                = shows "x-small"
  showsPrec _ (SizeSmall)               = shows "small"
  showsPrec _ (SizeMedium)              = shows "medium"
  showsPrec _ (SizeLarge)               = shows "large"
  showsPrec _ (SizeHuge)                = shows "x-large"
  showsPrec _ (SizeGiant)               = shows "xx-large"
  showsPrec _ (SizeSmaller)             = shows "smaller"
  showsPrec _ (SizeLarger)              = shows "larger"



#if PANGO_VERSION_CHECK(1,6,0)
-- | The 'EllipsizeMode' type describes what sort of (if any) ellipsization
-- should be applied to a line of text. In the ellipsization process characters
-- are removed from the text in order to make it fit to a given width and
-- replaced with an ellipsis.
--
{# enum EllipsizeMode {underscoreToCase} deriving (Eq) #}
#endif


-- | The characteristic measurements of a font.
--
-- * All values are measured in pixels.
--
-- * In Pango versions before 1.6 only 'ascent', 'descent',
--   'approximateCharWidth' and 'approximateDigitWidth' are available.
--
data FontMetrics = FontMetrics {
  -- | The ascent is the distance from the baseline to the logical top
  --   of a line of text. (The logical top may be above or below the
  --   top of the actual drawn ink. It is necessary to lay out the
  --   text to figure where the ink will be.)
  ascent :: Double,
  -- | The descent is the distance from the baseline to the logical
  --   bottom of a line of text. (The logical bottom may be above or
  --   below the bottom of the actual drawn ink. It is necessary to
  --   lay out the text to figure where the ink will be.)
  descent :: Double,
  -- | The approximate character width. This is merely a
  --   representative value useful, for example, for determining the
  --   initial size for a window. Actual characters in text will be
  --   wider and narrower than this.
  approximateCharWidth :: Double,
  -- | The approximate digit width. This is merely a representative
  --   value useful, for example, for determining the initial size for
  --   a window. Actual digits in text can be wider and narrower than
  --   this, though this value is generally somewhat more accurate
  --   than 'approximateCharWidth'.
  approximateDigitWidth :: Double
#if PANGO_VERSION_CHECK(1,6,0)
  ,
  -- | The suggested thickness to draw an underline.
  underlineThickness :: Double,
  -- | The suggested position to draw the underline. The value returned is
  --   the distance above the baseline of the top of the underline. Since
  --   most fonts have underline positions beneath the baseline, this value
  --   is typically negative.
  underlinePosition :: Double,
  -- | The suggested thickness to draw for the strikethrough.
  strikethroughThickness :: Double,
  -- | The suggested position to draw the strikethrough. The value
  --   returned is the distance above the baseline of the top of the
  --   strikethrough.
  strikethroughPosition :: Double
#endif
  } deriving Show

