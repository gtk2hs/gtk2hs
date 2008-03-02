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
module Graphics.UI.Gtk.Pango.Enums (
  FontStyle(..),
  Size(..),
  Weight(..),
  Variant(..),
  Stretch(..),
  Underline(..),
  PangoDirection(..),
#if PANGO_CHECK_VERSION(1,6,0)
  EllipsizeMode(..),
#endif
#if PANGO_CHECK_VERSION(1,16,0)
  PangoGravity(..),
  PangoGravityHint(..),
#endif
  ) where

{#import Graphics.UI.Gtk.Pango.Types#}
import Graphics.UI.Gtk.Pango.Structs -- for the Enum instance of PangoDirection
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



#if PANGO_CHECK_VERSION(1,6,0)
-- | The 'EllipsizeMode' type describes what sort of (if any) ellipsization
-- should be applied to a line of text. In the ellipsization process characters
-- are removed from the text in order to make it fit to a given width and
-- replaced with an ellipsis.
--
{# enum EllipsizeMode {underscoreToCase} deriving (Eq) #}
#endif

