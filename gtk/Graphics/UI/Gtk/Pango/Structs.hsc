{-# OPTIONS -cpp #-}
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

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
module Graphics.UI.Gtk.Pango.Structs (
  pangoScale,
  puToInt, puToUInt,
  intToPu, uIntToPu,
  PangoDirection(..),
  pangodirToLevel,
  fromRect,
  toRect,
  setAttrPos,
  pangoItemGetFont,
  pangoItemGetLanguage,
  pangoItemRawAnalysis,
  pangoItemRawGetLevel,
  readAttr
  ) where

import Control.Monad		(liftM)
import Data.IORef
import Control.Exception

import System.Glib.FFI
import System.Glib.UTFString ( peekUTFString, UTFCorrection,
                               ofsToUTF, ofsFromUTF )
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
import System.Glib.GObject		(makeNewGObject)
import Graphics.UI.Gtk.Types
import Graphics.UI.Gtk.Pango.Types
import Graphics.UI.Gtk.General.Structs  (Color(..), Rectangle(..))

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
puToUInt u = let u' = u*pangoScale in if u<0 then 0 else truncate u

intToPu :: GInt -> Double
intToPu i = fromIntegral i/pangoScale

uIntToPu :: GInt -> Double
uIntToPu i = fromIntegral i/pangoScale

instance Enum PangoDirection where
  fromEnum PangoDirectionLtr        = #{const PANGO_DIRECTION_LTR }
  fromEnum PangoDirectionRtl        = #{const PANGO_DIRECTION_RTL }
#if PANGO_CHECK_VERSION(1,4,0)
  fromEnum PangoDirectionWeakLtr    = #{const PANGO_DIRECTION_WEAK_LTR }
  fromEnum PangoDirectionWeakRtl    = #{const PANGO_DIRECTION_WEAK_RTL }
  fromEnum PangoDirectionNeutral    = #{const PANGO_DIRECTION_NEUTRAL }
#endif
  toEnum #{const PANGO_DIRECTION_LTR } = PangoDirectionLtr
  toEnum #{const PANGO_DIRECTION_RTL } = PangoDirectionRtl
  toEnum #{const PANGO_DIRECTION_TTB_LTR } = PangoDirectionLtr
  toEnum #{const PANGO_DIRECTION_TTB_RTL } = PangoDirectionRtl
#if PANGO_CHECK_VERSION(1,4,0)
  toEnum #{const PANGO_DIRECTION_WEAK_LTR } = PangoDirectionWeakLtr
  toEnum #{const PANGO_DIRECTION_WEAK_RTL } = PangoDirectionWeakRtl
  toEnum #{const PANGO_DIRECTION_NEUTRAL } = PangoDirectionNeutral
#endif

-- This is a copy of the local function direction_simple in pango-layout.c
pangodirToLevel :: PangoDirection -> Int
pangodirToLevel PangoDirectionLtr = 1
pangodirToLevel PangoDirectionRtl = -1
#if PANGO_CHECK_VERSION(1,4,0)
pangodirToLevel PangoDirectionWeakLtr = 1
pangodirToLevel PangoDirectionWeakRtl = -1
pangodirToLevel PangoDirectionNeutral = 0
#endif


-- Cheating functions: We marshal PangoRectangles as Rectangles.
fromRect :: Rectangle -> PangoRectangle
fromRect (Rectangle x y w h) =
  PangoRectangle (fromIntegral x/pangoScale)
		 (fromIntegral y/pangoScale)
		 (fromIntegral w/pangoScale)
		 (fromIntegral h/pangoScale)

toRect :: PangoRectangle -> Rectangle
toRect (PangoRectangle x y w h) = Rectangle (truncate (x*pangoScale))
				            (truncate (y*pangoScale))
				            (truncate (w*pangoScale))
				            (truncate (h*pangoScale))

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

-- Get the PangoAnalysis within a PangoItem
pangoItemRawAnalysis :: Ptr pangoItem -> Ptr pangoAnalysis
pangoItemRawAnalysis = #{ptr PangoItem, analysis}

-- Get the text direction of this PangoItem.
pangoItemRawGetLevel :: Ptr pangoItem -> IO Bool
pangoItemRawGetLevel ptr = do
  level <- #{peek PangoItem, analysis.level} ptr
  return (toBool (level :: #{type guint8}))

-- Set the start and end position of an attribute
setAttrPos :: UTFCorrection -> Int -> Int -> IO (Ptr ()) -> IO (Ptr ())
setAttrPos correct start end act = do
  atPtr <- act
  #{poke PangoAttribute, start_index} atPtr
    (fromIntegral (ofsToUTF start correct) :: #{type guint})
  #{poke PangoAttribute, end_index} atPtr
    (fromIntegral (ofsToUTF end correct) :: #{type guint})
  return atPtr

-- | Convert a pointer to an attribute to an attribute.
readAttr :: UTFCorrection -> CPangoAttribute -> IO PangoAttribute
readAttr correct attrPtr = do
  klassPtr <- #{peek PangoAttribute, klass} attrPtr
  startByte <- #{peek PangoAttribute, start_index} attrPtr
  endByte <- #{peek PangoAttribute, end_index} attrPtr
  ty <- #{peek PangoAttrClass, type} klassPtr
  let b :: Int
      b = ofsFromUTF (fromIntegral (startByte :: #{type guint})) correct
      e :: Int
      e = ofsFromUTF (fromIntegral (endByte :: #{type guint})) correct
  case ty :: #{type PangoAttrType} of
    #{const PANGO_ATTR_LANGUAGE} -> do
      lang <- #{peek PangoAttrLanguage, value} attrPtr
      return $ AttrLanguage b e (Language lang)
    #{const PANGO_ATTR_FAMILY} -> do
      strPtr <- #{peek PangoAttrString, value} attrPtr
      str <- peekUTFString strPtr
      return $ AttrFamily b e str
    #{const PANGO_ATTR_STYLE} -> do
      v <- #{peek PangoAttrInt, value} attrPtr
      return $ AttrStyle b e (toEnum (fromIntegral (v::#{type int})))
    #{const PANGO_ATTR_WEIGHT} -> do
      v <- #{peek PangoAttrInt, value} attrPtr
      return $ AttrWeight b e (toEnum (fromIntegral (v::#{type int})))
    #{const PANGO_ATTR_VARIANT} -> do
      v <- #{peek PangoAttrInt, value} attrPtr
      return $ AttrVariant b e (toEnum (fromIntegral (v::#{type int})))
    #{const PANGO_ATTR_STRETCH} -> do
      v <- #{peek PangoAttrInt, value} attrPtr
      return $ AttrStretch b e (toEnum (fromIntegral (v::#{type int})))
    #{const PANGO_ATTR_SIZE} -> do
      v <- #{peek PangoAttrFloat, value} attrPtr
      return $ AttrSize b e (realToFrac (v::#{type double}))
#if PANGO_CHECK_VERSION(1,8,0)
    #{const PANGO_ATTR_ABSOLUTE_SIZE} -> do
      v <- #{peek PangoAttrFloat, value} attrPtr
      return $ AttrAbsSize b e (realToFrac (v::#{type double}))
#endif
    #{const PANGO_ATTR_FONT_DESC} -> do
      fdPtr <- #{peek PangoAttrFontDesc, desc} attrPtr
      fd <- makeNewFontDescription fdPtr
      return $ AttrFontDescription b e fd
    #{const PANGO_ATTR_FOREGROUND} -> do
      col <- #{peek PangoAttrColor, color} attrPtr
      return $ AttrForeground b e col
    #{const PANGO_ATTR_BACKGROUND} -> do
      col <- #{peek PangoAttrColor, color} attrPtr
      return $ AttrBackground b e col
    #{const PANGO_ATTR_UNDERLINE} -> do
      v <- #{peek PangoAttrInt, value} attrPtr
      return $ AttrUnderline b e (toEnum (fromIntegral (v::#{type int})))
#if  (defined (WIN32) && PANGO_CHECK_VERSION(1,10,0)) \
 || (!defined (WIN32) && PANGO_CHECK_VERSION(1,8,0))
    #{const PANGO_ATTR_UNDERLINE_COLOR} -> do
      col <- #{peek PangoAttrColor, color} attrPtr
      return $ AttrUnderlineColor b e col
#endif
    #{const PANGO_ATTR_STRIKETHROUGH} -> do
      v <- #{peek PangoAttrInt, value} attrPtr
      return $ AttrStrikethrough b e (toEnum (fromIntegral (v::#{type int})))
#if  (defined (WIN32) && PANGO_CHECK_VERSION(1,10,0)) \
 || (!defined (WIN32) && PANGO_CHECK_VERSION(1,8,0))
    #{const PANGO_ATTR_STRIKETHROUGH_COLOR} -> do
      col <- #{peek PangoAttrColor, color} attrPtr
      return $ AttrStrikethroughColor b e col
#endif
    #{const PANGO_ATTR_RISE} -> do
      v <- #{peek PangoAttrFloat, value} attrPtr
      return $ AttrRise b e  (realToFrac (v::#{type double}))
#if PANGO_CHECK_VERSION(1,8,0)
    #{const PANGO_ATTR_SHAPE} -> do
      rect1 <- #{peek PangoAttrShape, ink_rect} attrPtr
      rect2 <- #{peek PangoAttrShape, logical_rect} attrPtr
      return $ AttrShape b e (fromRect rect1) (fromRect rect2)
#endif
    #{const PANGO_ATTR_SCALE} -> do
      v <- #{peek PangoAttrFloat, value} attrPtr
      return $ AttrScale b e (realToFrac (v::#{type double}))
#if PANGO_CHECK_VERSION(1,4,0)
    #{const PANGO_ATTR_FALLBACK} -> do
      v <- #{peek PangoAttrInt, value} attrPtr
      return $ AttrFallback b e (toBool (v::#{type int}))
#endif
#if PANGO_CHECK_VERSION(1,6,0)
    #{const PANGO_ATTR_LETTER_SPACING} -> do
      v <- #{peek PangoAttrFloat, value} attrPtr
      return $ AttrLetterSpacing b e (realToFrac (v::#{type double}))
#endif
#if PANGO_CHECK_VERSION(1,16,0)
    #{const PANGO_ATTR_GRAVITY} -> do
      v <- #{peek PangoAttrInt, value} attrPtr
      return $ AttrGravity b e (toEnum (fromIntegral (v::#{type int})))
    #{const PANGO_ATTR_GRAVITY_HINT} -> do
      v <- #{peek PangoAttrInt, value} attrPtr
      return $ AttrGravityHint b e (toEnum (fromIntegral (v::#{type int})))
#endif
    _ -> error "extracting pango attributes: unknown attribute type"
