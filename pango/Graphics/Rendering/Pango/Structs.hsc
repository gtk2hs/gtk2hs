{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Structures for Pango
--
--  Author : Axel Simon
--
--  Created: 2 March 2008
--
--  Copyright (C) 2008 Axel Simon
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
-- #hide

#include "hspango.h"
#include <glib.h>
#include "template-hsc-gtk2hs.h"

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
module Graphics.Rendering.Pango.Structs (
  PangoUnit,
  Color(..),
  Rectangle(..),
  PangoRectangle(..),
  peekIntPangoRectangle,

  PangoDirection(..),

  pangoScale,
  puToInt, puToUInt,
  intToPu, uIntToPu,
  pangodirToLevel,
  PangoAttribute(..),
  setAttrPos,
  pangoItemGetFont,
  pangoItemGetLanguage,
  pangoItemRawGetOffset,
  pangoItemRawGetLength,
  pangoItemRawAnalysis,
  pangoItemRawGetLevel,
  readAttr
  ) where

import Control.Monad            (liftM)
import Data.IORef
import Control.Exception

import System.Glib.FFI
import System.Glib.UTFString ( peekUTFString, UTFCorrection,
                               ofsToUTF, ofsFromUTF, DefaultGlibString )
import System.Glib.GObject              (makeNewGObject)
import Graphics.Rendering.Pango.Types
import Graphics.Rendering.Pango.BasicTypes

-- A pango unit is an internal euclidian metric, that is, a measure for
-- lengths and position.
--
-- * Deprecated. Replaced by Double.
type PangoUnit = Double

-- | Color
--
-- * Specifies a color with three integer values for red, green and blue.
--   All values range from 0 (least intense) to 65535 (highest intensity).
--
data Color = Color (#gtk2hs_type guint16) (#gtk2hs_type guint16) (#gtk2hs_type guint16)
             deriving (Eq,Show)

-- PangoColor is different from GdkColor, but for the Gtk2Hs user we pretend they
-- are the same. To do this, we need a different marshalling routine for PangoColors.

peekPangoColor :: Ptr Color -> IO Color
peekPangoColor ptr = do
    red    <- #{peek PangoColor, red} ptr
    green  <- #{peek PangoColor, green} ptr
    blue   <- #{peek PangoColor, blue} ptr
    return $ Color red green blue

-- | Rectangle
--
-- * Specifies x, y, width and height
--
data Rectangle = Rectangle Int Int Int Int deriving (Eq,Show)

-- | Rectangles describing an area in 'Double's.
--
-- * Specifies x, y, width and height
--
data PangoRectangle = PangoRectangle Double Double Double Double
                      deriving Show

instance Storable PangoRectangle where
  sizeOf _ = #{const sizeof(PangoRectangle)}
  alignment _ = alignment (undefined:: #gtk2hs_type gint)
  peek ptr = do
    (Rectangle x_ y_ w_ h_) <- peekIntPangoRectangle ptr
    return $ PangoRectangle (fromIntegral x_/pangoScale) (fromIntegral y_/pangoScale)
                            (fromIntegral w_/pangoScale) (fromIntegral h_/pangoScale)
  poke ptr (PangoRectangle x y w h) = do
    #{poke PangoRectangle, x} ptr ((truncate (x*pangoScale))::#gtk2hs_type gint)
    #{poke PangoRectangle, y} ptr ((truncate (y*pangoScale))::#gtk2hs_type gint)
    #{poke PangoRectangle, width} ptr ((truncate (w*pangoScale))::#gtk2hs_type gint)
    #{poke PangoRectangle, height} ptr ((truncate (h*pangoScale))::#gtk2hs_type gint)

peekIntPangoRectangle :: Ptr PangoRectangle -> IO Rectangle
peekIntPangoRectangle ptr = do
    (x_ ::#gtk2hs_type gint)    <- #{peek PangoRectangle, x} ptr
    (y_ ::#gtk2hs_type gint)    <- #{peek PangoRectangle, y} ptr
    (w_ ::#gtk2hs_type gint)    <- #{peek PangoRectangle, width} ptr
    (h_ ::#gtk2hs_type gint)    <- #{peek PangoRectangle, height} ptr
    return (Rectangle (fromIntegral x_) (fromIntegral y_)
                      (fromIntegral w_) (fromIntegral h_))

-- | The 'PangoDirection' type represents a direction in the Unicode
-- bidirectional algorithm.
--
-- * The \"weak\" values denote a left-to-right or right-to-left direction
--   only if there is no character with a strong direction in a paragraph.
--   An example is a sequence of special, graphical characters which are
--   neutral with respect to their rendering direction. A fresh
--   'Graphics.Rendering.Pango.Rendering.PangoContext' is by default weakly
--   left-to-right.
--
-- * Not every value in this enumeration makes sense for every usage
--   of 'PangoDirection'; for example, the return value of
--   'unicharDirection' and 'findBaseDir' cannot be 'PangoDirectionWeakLtr'
--   or 'PangoDirectionWeakRtl', since every character is either neutral or
--   has a strong direction; on the other hand 'PangoDirectionNeutral'
--   doesn't make sense to pass to 'log2visGetEmbeddingLevels'.
--
data PangoDirection = PangoDirectionLtr
                    | PangoDirectionRtl
#if PANGO_VERSION_CHECK(1,4,0)
                    | PangoDirectionWeakLtr
                    | PangoDirectionWeakRtl
                    | PangoDirectionNeutral
#endif
                    deriving (Eq,Ord)



-- Internal unit of measuring sizes.
--
-- * This constant represents the scale between
--   dimensions used for distances in text rendering and Pango device units.
--   The
--   definition of device unit is dependent on the output device; it will
--   typically be pixels for a screen, and points for a printer.  When
--   setting font sizes, device units are always considered to be points
--   (as in \"12 point font\"), rather than pixels.
--
pangoScale :: Double
pangoScale = #const PANGO_SCALE

puToInt :: Double -> GInt
puToInt u = truncate (u*pangoScale)

puToUInt :: Double -> GInt
puToUInt u = let u' = u*pangoScale in if u'<0 then 0 else truncate u'

intToPu :: GInt -> Double
intToPu i = fromIntegral i/pangoScale

uIntToPu :: GInt -> Double
uIntToPu i = fromIntegral i/pangoScale

instance Enum PangoDirection where
  fromEnum PangoDirectionLtr        = #{const PANGO_DIRECTION_LTR }
  fromEnum PangoDirectionRtl        = #{const PANGO_DIRECTION_RTL }
#if PANGO_VERSION_CHECK(1,4,0)
  fromEnum PangoDirectionWeakLtr    = #{const PANGO_DIRECTION_WEAK_LTR }
  fromEnum PangoDirectionWeakRtl    = #{const PANGO_DIRECTION_WEAK_RTL }
  fromEnum PangoDirectionNeutral    = #{const PANGO_DIRECTION_NEUTRAL }
#endif
  toEnum #{const PANGO_DIRECTION_LTR } = PangoDirectionLtr
  toEnum #{const PANGO_DIRECTION_RTL } = PangoDirectionRtl
  toEnum #{const PANGO_DIRECTION_TTB_LTR } = PangoDirectionLtr
  toEnum #{const PANGO_DIRECTION_TTB_RTL } = PangoDirectionRtl
#if PANGO_VERSION_CHECK(1,4,0)
  toEnum #{const PANGO_DIRECTION_WEAK_LTR } = PangoDirectionWeakLtr
  toEnum #{const PANGO_DIRECTION_WEAK_RTL } = PangoDirectionWeakRtl
  toEnum #{const PANGO_DIRECTION_NEUTRAL } = PangoDirectionNeutral
#endif

-- This is a copy of the local function direction_simple in pango-layout.c
pangodirToLevel :: PangoDirection -> Int
pangodirToLevel PangoDirectionLtr = 1
pangodirToLevel PangoDirectionRtl = -1
#if PANGO_VERSION_CHECK(1,4,0)
pangodirToLevel PangoDirectionWeakLtr = 1
pangodirToLevel PangoDirectionWeakRtl = -1
pangodirToLevel PangoDirectionNeutral = 0
#endif

-- | Extract the font used for this 'PangoItem'.
--
pangoItemGetFont :: PangoItem -> IO Font
pangoItemGetFont (PangoItem _ (PangoItemRaw pir)) =
  withForeignPtr pir pangoItemRawGetFont

-- | Extract the 'Language' used for this 'PangoItem'.
--
pangoItemGetLanguage :: PangoItem -> IO Language
pangoItemGetLanguage (PangoItem _ (PangoItemRaw pir)) =
  liftM (Language . castPtr) $ withForeignPtr pir pangoItemRawGetLanguage

-- Get the font of a PangoAnalysis within a PangoItem.
pangoItemRawGetFont :: Ptr pangoItem -> IO Font
pangoItemRawGetFont ptr =
  makeNewGObject mkFont (#{peek PangoItem, analysis.font} ptr)

-- Get the font of a PangoAnalysis within a PangoItem.
pangoItemRawGetLanguage :: Ptr pangoItem -> IO (Ptr CChar)
pangoItemRawGetLanguage ptr =
  #{peek PangoItem, analysis.language} ptr

-- Get the offset at which a PangoItem starts
pangoItemRawGetOffset :: Ptr pangoItem -> IO #{type gint}
pangoItemRawGetOffset = #{peek PangoItem, offset}

-- Get the number of bytes that the PangoItem affects
pangoItemRawGetLength :: Ptr pangoItem -> IO #{type gint}
pangoItemRawGetLength = #{peek PangoItem, length}

-- Get the PangoAnalysis within a PangoItem
pangoItemRawAnalysis :: Ptr pangoItem -> Ptr pangoAnalysis
pangoItemRawAnalysis = #{ptr PangoItem, analysis}

-- Get the text direction of this PangoItem.
pangoItemRawGetLevel :: Ptr pangoItem -> IO Bool
pangoItemRawGetLevel ptr = do
  level <- #{peek PangoItem, analysis.level} ptr
  return (toBool (level :: #{gtk2hs_type guint8}))

-- Set the start and end position of an attribute
setAttrPos :: UTFCorrection -> Int -> Int -> IO (Ptr ()) -> IO (Ptr ())
setAttrPos correct start end act = do
  atPtr <- act
  #{poke PangoAttribute, start_index} atPtr
    (fromIntegral (ofsToUTF start correct) :: #{gtk2hs_type guint})
  #{poke PangoAttribute, end_index} atPtr
    (fromIntegral (ofsToUTF end correct) :: #{gtk2hs_type guint})
  return atPtr

-- | Attributes for 'PangoItem's.
--
-- * A given attribute is applied from its start position 'paStart' up,
--   but not including the end position, 'paEnd'.
--
data PangoAttribute
  -- | A hint as to what language this piece of text is written in.
  = AttrLanguage { paStart :: Int, paEnd :: Int, paLang :: Language }
  -- | The font family, e.g. @sans serif@.
  | AttrFamily { paStart :: Int, paEnd :: Int, paFamily :: DefaultGlibString }
  -- | The slant of the current font.
  | AttrStyle { paStart :: Int, paEnd :: Int, paStyle :: FontStyle }
  -- | Weight of font, e.g. 'WeightBold'.
  | AttrWeight { paStart :: Int, paEnd :: Int, paWeight :: Weight }
  -- | 'VariantSmallCaps' will display lower case letters as small
  -- upper case letters (if the font supports this).
  | AttrVariant { paStart :: Int, paEnd :: Int, paVariant :: Variant }
  -- | Stretch or condense the width of the letters.
  | AttrStretch { paStart :: Int, paEnd :: Int, paStretch :: Stretch }
  -- | Specify the size of the font in points.
  | AttrSize { paStart :: Int, paEnd :: Int, paSize :: Double }
#if PANGO_VERSION_CHECK(1,8,0)
  -- | Specify the size of the font in device units (pixels).
  --
  -- * Available in Pango 1.8.0 and higher.
  --
  | AttrAbsSize { paStart :: Int, paEnd :: Int, paSize :: Double }
#endif
  -- | Specify several attributes of a font at once. Note that no deep copy
  --   of the description is made when this attributes is passed to or received
  --   from functions.
    | AttrFontDescription { paStart :: Int, paEnd :: Int,
                          paFontDescription :: FontDescription }
  -- | Specify the foreground color.
  | AttrForeground { paStart :: Int, paEnd :: Int, paColor :: Color }
  -- | Specify the background color.
  | AttrBackground { paStart :: Int, paEnd :: Int, paColor :: Color }
  -- | Specify the kind of underline, e.g. 'UnderlineSingle'.
  | AttrUnderline { paStart :: Int, paEnd :: Int, paUnderline :: Underline }
#if  (defined (WIN32) && PANGO_VERSION_CHECK(1,10,0)) \
 || (!defined (WIN32) && PANGO_VERSION_CHECK(1,8,0))
  -- | Specify the color of an underline.
  --
  -- * Available in Pango 1.8.0 and higher.
  --
  | AttrUnderlineColor { paStart :: Int, paEnd :: Int, paColor :: Color }
#endif
  -- | Specify if this piece of text should have a line through it.
  | AttrStrikethrough { paStart :: Int, paEnd :: Int, paStrikethrough :: Bool }
#if  (defined (WIN32) && PANGO_VERSION_CHECK(1,10,0)) \
 || (!defined (WIN32) && PANGO_VERSION_CHECK(1,8,0))
  -- | Specify the color of the strike through line.
  --
  -- * Available in Pango 1.8.0 and higher.
  --
  | AttrStrikethroughColor { paStart :: Int, paEnd :: Int, paColor :: Color }
#endif
  -- | Displace the text vertically. Positive values move the text upwards.
  | AttrRise { paStart :: Int, paEnd :: Int, paRise :: Double }
#if PANGO_VERSION_CHECK(1,8,0)
  -- | Restrict the amount of what is drawn of the marked shapes.
  --
  -- * Available in Pango 1.8.0 and higher.
  --
  | AttrShape { paStart :: Int, paEnd :: Int, paInk :: PangoRectangle,
                paLogical :: PangoRectangle }
#endif
  -- | Scale the font up (values greater than one) or shrink the font.
  | AttrScale { paStart :: Int, paEnd :: Int, paScale :: Double }
#if PANGO_VERSION_CHECK(1,4,0)
  -- | Determine if a fall back font should be substituted if no matching
  -- font is available.
  | AttrFallback { paStart :: Int, paEnd :: Int, paFallback :: Bool }
#endif
#if PANGO_VERSION_CHECK(1,6,0)
  -- | Add extra space between graphemes of the text.
  --
  -- * Available in Pango 1.6.0 and higher.
  --
  | AttrLetterSpacing { paStart :: Int, paEnd :: Int,
                        paLetterSpacing :: Double }
#endif
#if PANGO_VERSION_CHECK(1,16,0)
  -- | Sets the gravity field of a font description. The gravity field specifies
  -- how the glyphs should be rotated. If gravity is 'GravityAuto', this
  -- actually unsets the gravity mask on the font description.
  --
  -- * This function is seldom useful to the user. Gravity should normally be
  --   set on a 'PangoContext'.
  --
  -- * Available in Pango 1.16.0 and higher.
  --
  | AttrGravity { paStart :: Int, paEnd :: Int,
                        paGravity :: PangoGravity }

        -- | Set the way horizontal scripts behave in a vertical context.
  --
  -- * Available in Pango 1.16.0 and higher.
  --
        | AttrGravityHint  { paStart :: Int, paEnd :: Int,
                        paGravityHint :: PangoGravityHint }
#endif
  deriving Show

-- | Convert a pointer to an attribute to an attribute.
readAttr :: UTFCorrection -> CPangoAttribute -> IO PangoAttribute
readAttr correct attrPtr = do
  klassPtr <- #{peek PangoAttribute, klass} attrPtr
  startByte <- #{peek PangoAttribute, start_index} attrPtr
  endByte <- #{peek PangoAttribute, end_index} attrPtr
  ty <- #{peek PangoAttrClass, type} klassPtr
  let b :: Int
      b = ofsFromUTF (fromIntegral (startByte :: #{gtk2hs_type guint})) correct
      e :: Int
      e = ofsFromUTF (fromIntegral (endByte :: #{gtk2hs_type guint})) correct
  case ty :: #{gtk2hs_type PangoAttrType} of
    #{const PANGO_ATTR_LANGUAGE} -> do
      lang <- #{peek PangoAttrLanguage, value} attrPtr
      return $ AttrLanguage b e (Language lang)
    #{const PANGO_ATTR_FAMILY} -> do
      strPtr <- #{peek PangoAttrString, value} attrPtr
      str <- peekUTFString strPtr
      return $ AttrFamily b e str
    #{const PANGO_ATTR_STYLE} -> do
      v <- #{peek PangoAttrInt, value} attrPtr
      return $ AttrStyle b e (toEnum (fromIntegral (v::#{gtk2hs_type int})))
    #{const PANGO_ATTR_WEIGHT} -> do
      v <- #{peek PangoAttrInt, value} attrPtr
      return $ AttrWeight b e (toEnum (fromIntegral (v::#{gtk2hs_type int})))
    #{const PANGO_ATTR_VARIANT} -> do
      v <- #{peek PangoAttrInt, value} attrPtr
      return $ AttrVariant b e (toEnum (fromIntegral (v::#{gtk2hs_type int})))
    #{const PANGO_ATTR_STRETCH} -> do
      v <- #{peek PangoAttrInt, value} attrPtr
      return $ AttrStretch b e (toEnum (fromIntegral (v::#{gtk2hs_type int})))
    #{const PANGO_ATTR_SIZE} -> do
      v <- #{peek PangoAttrFloat, value} attrPtr
      return $ AttrSize b e (realToFrac (v::#{gtk2hs_type double}))
#if PANGO_VERSION_CHECK(1,8,0)
    #{const PANGO_ATTR_ABSOLUTE_SIZE} -> do
      v <- #{peek PangoAttrFloat, value} attrPtr
      return $ AttrAbsSize b e (realToFrac (v::#{gtk2hs_type double}))
#endif
    #{const PANGO_ATTR_FONT_DESC} -> do
      fdPtr <- #{peek PangoAttrFontDesc, desc} attrPtr
      fd <- makeNewFontDescription fdPtr
      return $ AttrFontDescription b e fd
    #{const PANGO_ATTR_FOREGROUND} -> do
      col <- peekPangoColor (#{ptr PangoAttrColor, color} attrPtr)
      return $ AttrForeground b e col
    #{const PANGO_ATTR_BACKGROUND} -> do
      col <- peekPangoColor (#{ptr PangoAttrColor, color} attrPtr)
      return $ AttrBackground b e col
    #{const PANGO_ATTR_UNDERLINE} -> do
      v <- #{peek PangoAttrInt, value} attrPtr
      return $ AttrUnderline b e (toEnum (fromIntegral (v::#{gtk2hs_type int})))
#if  (defined (WIN32) && PANGO_VERSION_CHECK(1,10,0)) \
 || (!defined (WIN32) && PANGO_VERSION_CHECK(1,8,0))
    #{const PANGO_ATTR_UNDERLINE_COLOR} -> do
      col <- peekPangoColor (#{ptr PangoAttrColor, color} attrPtr)
      return $ AttrUnderlineColor b e col
#endif
    #{const PANGO_ATTR_STRIKETHROUGH} -> do
      v <- #{peek PangoAttrInt, value} attrPtr
      return $ AttrStrikethrough b e (toEnum (fromIntegral (v::#{gtk2hs_type int})))
#if  (defined (WIN32) && PANGO_VERSION_CHECK(1,10,0)) \
 || (!defined (WIN32) && PANGO_VERSION_CHECK(1,8,0))
    #{const PANGO_ATTR_STRIKETHROUGH_COLOR} -> do
      col <- peekPangoColor (#{ptr PangoAttrColor, color} attrPtr)
      return $ AttrStrikethroughColor b e col
#endif
    #{const PANGO_ATTR_RISE} -> do
      v <- #{peek PangoAttrFloat, value} attrPtr
      return $ AttrRise b e  (realToFrac (v::#{gtk2hs_type double}))
#if PANGO_VERSION_CHECK(1,8,0)
    #{const PANGO_ATTR_SHAPE} -> do
      rect1 <- #{peek PangoAttrShape, ink_rect} attrPtr
      rect2 <- #{peek PangoAttrShape, logical_rect} attrPtr
      return $ AttrShape b e rect1 rect2
#endif
    #{const PANGO_ATTR_SCALE} -> do
      v <- #{peek PangoAttrFloat, value} attrPtr
      return $ AttrScale b e (realToFrac (v::#{gtk2hs_type double}))
#if PANGO_VERSION_CHECK(1,4,0)
    #{const PANGO_ATTR_FALLBACK} -> do
      v <- #{peek PangoAttrInt, value} attrPtr
      return $ AttrFallback b e (toBool (v::#{gtk2hs_type int}))
#endif
#if PANGO_VERSION_CHECK(1,6,0)
    #{const PANGO_ATTR_LETTER_SPACING} -> do
      v <- #{peek PangoAttrFloat, value} attrPtr
      return $ AttrLetterSpacing b e (realToFrac (v::#{gtk2hs_type double}))
#endif
#if PANGO_VERSION_CHECK(1,16,0)
    #{const PANGO_ATTR_GRAVITY} -> do
      v <- #{peek PangoAttrInt, value} attrPtr
      return $ AttrGravity b e (toEnum (fromIntegral (v::#{gtk2hs_type int})))
    #{const PANGO_ATTR_GRAVITY_HINT} -> do
      v <- #{peek PangoAttrInt, value} attrPtr
      return $ AttrGravityHint b e (toEnum (fromIntegral (v::#{gtk2hs_type int})))
#endif
    _ -> error "extracting pango attributes: unknown attribute type"
