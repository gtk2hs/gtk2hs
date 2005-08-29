-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Types
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Haskell bindings to the cairo types.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Types (
    Matrix(Matrix), MatrixPtr
  , Render(..)
  , Cairo(Cairo), unCairo
  , Surface(Surface), unSurface
  , Pattern(Pattern), unPattern
  , Status(..)
  , Operator(..)
  , Antialias(..)
  , FillRule(..)
  , LineCap(..)
  , LineJoin(..)
  , ScaledFont(..), unScaledFont
  , FontFace(..), unFontFace
  , Glyph, unGlyph
  , TextExtentsPtr
  , TextExtents(..)
  , FontExtentsPtr
  , FontExtents(..)
  , FontSlant(..)
  , FontWeight(..)
  , SubpixelOrder(..)
  , HintStyle(..)
  , HintMetrics(..)
  , FontOptions(..), withFontOptions, mkFontOptions
  , Path(..), unPath
  , Content(..)
  , Format(..)
  , Extend(..)
  , Filter(..)

  , cIntConv
  , cFloatConv
  , cFromBool
  , cToBool
  , cToEnum
  , cFromEnum
  , peekFloatConv
  , withFloatConv

  ) where

{#import Graphics.Rendering.Cairo.Matrix#}

import Foreign hiding (rotate)
import CForeign

import Monad (liftM)
import Control.Monad.Reader

{#context lib="cairo" prefix="cairo"#}

-- newtype Render m = Render (ReaderT Cairo IO m)
--   deriving (Functor, Monad, MonadIO, MonadReader Cairo)
newtype Render m = Render { runRender :: ReaderT Cairo IO m }
  deriving (Functor, Monad, MonadIO, MonadReader Cairo)

{#pointer *cairo_t as Cairo newtype#}
unCairo (Cairo x) = x

{#pointer *surface_t as Surface newtype#}
unSurface (Surface x) = x

{#pointer *pattern_t as Pattern newtype#}
unPattern (Pattern x) = x

{#enum status_t as Status {underscoreToCase} deriving(Eq)#}

{#enum operator_t as Operator {underscoreToCase}#}

{#enum antialias_t as Antialias {underscoreToCase}#}

{#enum fill_rule_t as FillRule {underscoreToCase}#}

{#enum line_cap_t as LineCap {underscoreToCase}#}

{#enum line_join_t as LineJoin {underscoreToCase}#}

{#pointer *scaled_font_t as ScaledFont newtype#}
unScaledFont (ScaledFont x) = x

{#pointer *font_face_t as FontFace newtype#}
unFontFace (FontFace x) = x

{#pointer *glyph_t as Glyph newtype#}
unGlyph (Glyph x) = x

{#pointer *text_extents_t as TextExtentsPtr -> TextExtents#}

data TextExtents = TextExtents {
    textExtentsXbearing :: Double
  , textExtentsYbearing :: Double
  , textExtentsWidth    :: Double
  , textExtentsHeight   :: Double
  , textExtentsXadvance :: Double
  , textExtentsYadvance :: Double
  }

instance Storable TextExtents where
  sizeOf _ = {#sizeof text_extents_t#}
  alignment _ = alignment (undefined :: CDouble)
  peek p = do
    x_bearing <- {#get text_extents_t->x_bearing#} p
    y_bearing <- {#get text_extents_t->y_bearing#} p
    width     <- {#get text_extents_t->width#}     p
    height    <- {#get text_extents_t->height#}    p
    x_advance <- {#get text_extents_t->x_advance#} p
    y_advance <- {#get text_extents_t->y_advance#} p
    return $ TextExtents (cFloatConv x_bearing) (cFloatConv y_bearing)
                         (cFloatConv width)     (cFloatConv height)
                         (cFloatConv x_advance) (cFloatConv y_advance)
  poke p (TextExtents x_bearing y_bearing width height x_advance y_advance) = do
    {#set text_extents_t->x_bearing#} p (cFloatConv x_bearing)
    {#set text_extents_t->y_bearing#} p (cFloatConv y_bearing)
    {#set text_extents_t->width#}     p (cFloatConv width)
    {#set text_extents_t->height#}    p (cFloatConv height)
    {#set text_extents_t->x_advance#} p (cFloatConv x_advance)
    {#set text_extents_t->y_advance#} p (cFloatConv y_advance)
    return ()

{#pointer *font_extents_t as FontExtentsPtr -> FontExtents#}

data FontExtents = FontExtents {
    fontExtentsAscent      :: Double
  , fontExtentsDescent     :: Double
  , fontExtentsHeight      :: Double
  , fontExtentsMaxXadvance :: Double
  , fontExtentsMaxYadvance :: Double
  }

instance Storable FontExtents where
  sizeOf _ = {#sizeof font_extents_t#}
  alignment _ = alignment (undefined :: CDouble)
  peek p = do
    ascent        <- {#get font_extents_t->ascent#}        p
    descent       <- {#get font_extents_t->descent#}       p
    height        <- {#get font_extents_t->height#}        p
    max_x_advance <- {#get font_extents_t->max_x_advance#} p
    max_y_advance <- {#get font_extents_t->max_y_advance#} p
    return $ FontExtents (cFloatConv ascent) (cFloatConv descent) (cFloatConv height)
                         (cFloatConv max_x_advance) (cFloatConv max_y_advance)
  poke p (FontExtents ascent descent height max_x_advance max_y_advance) = do
    {#set font_extents_t->ascent#}        p (cFloatConv ascent)
    {#set font_extents_t->descent#}       p (cFloatConv descent)
    {#set font_extents_t->height#}        p (cFloatConv height)
    {#set font_extents_t->max_x_advance#} p (cFloatConv max_x_advance)
    {#set font_extents_t->max_y_advance#} p (cFloatConv max_y_advance)
    return ()

{#enum font_slant_t as FontSlant {underscoreToCase}#}

{#enum font_weight_t as FontWeight {underscoreToCase}#}

{#enum subpixel_order_t as SubpixelOrder {underscoreToCase}#}

{#enum hint_style_t as HintStyle {underscoreToCase}#}

{#enum hint_metrics_t as HintMetrics {underscoreToCase}#}

{#pointer *font_options_t as FontOptions foreign newtype#}

withFontOptions (FontOptions fptr) = withForeignPtr fptr

mkFontOptions :: Ptr FontOptions -> IO FontOptions
mkFontOptions fontOptionsPtr = do
  fontOptionsForeignPtr <- newForeignPtr fontOptionsDestroy fontOptionsPtr
  return (FontOptions fontOptionsForeignPtr)

foreign import ccall unsafe "&cairo_font_options_destroy"
  fontOptionsDestroy :: FinalizerPtr FontOptions

-- XXX: pathToList :: Path -> [PathData]
-- 
-- http://cairographics.org/manual/bindings-path.html
-- 
-- {#enum path_data_type_t as PathDataType {underscoreToCase}#}
-- 
-- type Point = (Double, Double)
-- data PathData = PathMoveTo Point
--               | PathLineTo Point
--               | PathCurveTo Point Point Point
--               | PathClose

{#pointer *path_t as Path newtype#}
unPath (Path x) = x

{#enum content_t as Content {underscoreToCase}#}

data Format = FormatARGB32
            | FormatRGB24
            | FormatA8
            | FormatA1
            deriving (Enum)

{#enum extend_t as Extend {underscoreToCase}#}

{#enum filter_t as Filter {underscoreToCase}#}

-- Marshalling functions

cIntConv :: (Integral a, Integral b) => a -> b
cIntConv  = fromIntegral

cFloatConv :: (RealFloat a, RealFloat b) => a -> b
cFloatConv  = realToFrac
-- As this conversion by default goes via `Rational', it can be very slow...
{-# RULES
  "cFloatConv/Float->Float"   forall (x::Float).  cFloatConv x = x;
  "cFloatConv/Double->Double" forall (x::Double). cFloatConv x = x
 #-}

cFromBool :: Num a => Bool -> a
cFromBool  = fromBool

cToBool :: Num a => a -> Bool
cToBool  = toBool

cToEnum :: (Integral i, Enum e) => i -> e
cToEnum  = toEnum . cIntConv

cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum  = cIntConv . fromEnum

peekFloatConv :: (Storable a, RealFloat a, RealFloat b) =>  Ptr a -> IO b
peekFloatConv  = liftM cFloatConv . peek

withFloatConv :: (Storable b, RealFloat a, RealFloat b) => a -> (Ptr b -> IO c) -> IO c
withFloatConv  = with . cFloatConv

withArrayFloatConv :: (Storable b, RealFloat a, RealFloat b) => [a] -> (Ptr b -> IO b1) -> IO b1
withArrayFloatConv = withArray . map (cFloatConv)
