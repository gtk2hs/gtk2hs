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

-- #hide
module Graphics.Rendering.Cairo.Types (
    Matrix(Matrix), MatrixPtr
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

{#context lib="cairo" prefix="cairo"#}

-- not visible
{#pointer *cairo_t as Cairo newtype#}
unCairo (Cairo x) = x

-- | The medium to draw on.
{#pointer *surface_t as Surface newtype#}
unSurface (Surface x) = x

-- | Attributes for drawing operations.
{#pointer *pattern_t as Pattern newtype#}
unPattern (Pattern x) = x

-- | Cairo status.
--
-- * 'Status' is used to indicate errors that can occur when using
--   Cairo. In some cases it is returned directly by functions. When using
--   'Graphics.Rendering.Cairo.Render', the last error, if any, is stored
--   in the monad and can be retrieved with 'Graphics.Rendering.Cairo.status'.
--
{#enum status_t as Status {underscoreToCase} deriving(Eq)#}

-- | Composition operator for all drawing operations.
--
{#enum operator_t as Operator {underscoreToCase}#}

-- | Specifies the type of antialiasing to do when rendering text or shapes
--
-- ['AntialiasDefault']  Use the default antialiasing for the subsystem
-- and target device.
--
-- ['AntialiasNone']  Use a bilevel alpha mask.
--
-- ['AntialiasGray']  Perform single-color antialiasing (using shades of
-- gray for black text on a white background, for example).
--
-- ['AntialiasSubpixel']  Perform antialiasing by taking advantage of
-- the order of subpixel elements on devices such as LCD panels.
--
{#enum antialias_t as Antialias {underscoreToCase}#}

-- | Specify how paths are filled.
--
-- * For both fill rules, whether or not a point is included in the fill is
--   determined by taking a ray from that point to infinity and looking at
--   intersections with the path. The ray can be in any direction, as long
--   as it doesn't pass through the end point of a segment or have a tricky
--   intersection such as intersecting tangent to the path. (Note that
--   filling is not actually implemented in this way. This is just a
--   description of the rule that is applied.)
--
-- ['FillRuleWinding']  If the path crosses the ray from left-to-right,
--   counts +1. If the path crosses the ray from right to left, counts -1.
--   (Left and right are determined from the perspective of looking along
--   the ray from the starting point.) If the total count is non-zero, the
--   point will be filled.
--
-- ['FillRuleEvenOdd']  Counts the total number of intersections,
--   without regard to the orientation of the contour. If the total number
--   of intersections is odd, the point will be filled.
--
{#enum fill_rule_t as FillRule {underscoreToCase}#}

-- | Specify line endings.
--
-- ['LineCapButt'] Start(stop) the line exactly at the start(end) point.
--
-- ['LineCapRound'] Use a round ending, the center of the circle is the
--   end point.
--
-- ['LineCapSquare'] Use squared ending, the center of the square is the
--   end point
--
{#enum line_cap_t as LineCap {underscoreToCase}#}

-- | Specify how lines join.
--
{#enum line_join_t as LineJoin {underscoreToCase}#}

{#pointer *scaled_font_t as ScaledFont newtype#}
unScaledFont (ScaledFont x) = x

{#pointer *font_face_t as FontFace newtype#}
unFontFace (FontFace x) = x

{#pointer *glyph_t as Glyph newtype#}
unGlyph (Glyph x) = x

{#pointer *text_extents_t as TextExtentsPtr -> TextExtents#}

-- | Specify the extents of a text.
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

-- | Result of querying the font extents.
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

-- | Specify font slant.
{#enum font_slant_t as FontSlant {underscoreToCase}#}

-- | Specify font weight.
{#enum font_weight_t as FontWeight {underscoreToCase}#}

-- | Specify subpixel order.
{#enum subpixel_order_t as SubpixelOrder {underscoreToCase}#}

-- | FIXME: Document.
{#enum hint_style_t as HintStyle {underscoreToCase}#}

-- | FIXME: Document.
{#enum hint_metrics_t as HintMetrics {underscoreToCase}#}

-- | Specifies how to render text.
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

-- | A Cairo path.
--
-- * A path is a sequence of drawing operations that are accumulated until
--   'Graphics.Rendering.Cairo.stroke' is called. Using a path is particularly
--   useful when drawing lines with special join styles and
--   'Graphics.Rendering.Cairo.closePath'.
--
{#pointer *path_t as Path newtype#}
unPath (Path x) = x

{#enum content_t as Content {underscoreToCase}#}

data Format = FormatARGB32
            | FormatRGB24
            | FormatA8
            | FormatA1
            deriving (Enum)

-- | FIX ME: We should find out about this.
{#enum extend_t as Extend {underscoreToCase}#}

-- | Specify how filtering is done.
{#enum filter_t as Filter {underscoreToCase}#}

-- Marshalling functions

cIntConv :: (Integral a, Integral b) => a -> b
cIntConv  = fromIntegral

cFloatConv :: (RealFloat a, RealFloat b) => a -> b
cFloatConv  = realToFrac

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
