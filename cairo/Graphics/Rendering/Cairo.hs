-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo
-- Copyright   :  (c) Paolo Martini 2005, (c) Abraham Egnor 2004, (c) Aetion Technologies LLC 2004
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Higher level interface to cairo
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo (

    liftIO

  , Render(runRender)
  , renderWith

  , save
  , restore
  , withTargetSurface
  , setSourceRGB
  , setSourceRGBA
  , setSource
  , setSourceSurface
  , getSource
  , setAntialias
  , setDash
  , setFillRule
  , getFillRule
  , setLineCap
  , getLineCap
  , setLineJoin
  , getLineJoin
  , setLineWidth
  , getLineWidth
  , setMiterLimit
  , getMiterLimit
  , setOperator
  , getOperator
  , setTolerance
  , getTolerance
  , clip
  , clipPreserve
  , resetClip
  , fill
  , fillPreserve
  , fillExtents
  , inFill
  , mask
  , maskSurface
  , paint
  , paintWithAlpha
  , stroke
  , strokePreserve
  , strokeExtents
  , inStroke
  , copyPage
  , showPage

  , getCurrentPoint
  , newPath
  , closePath
  , arc
  , arcNegative
  , curveTo
  , lineTo
  , moveTo
  , rectangle
  , textPath
  , relCurveTo
  , relLineTo
  , relMoveTo

  , withRGBPattern
  , withRGBAPattern
  , withPatternForSurface
  , withLinearPattern
  , withRadialPattern
  , patternAddColorStopRGB
  , patternAddColorStopRGBA
  , patternSetMatrix
  , patternGetMatrix
  , patternSetExtend
  , patternGetExtend
  , patternSetFilter
  , patternGetFilter

  , translate
  , scale
  , rotate
  , transform
  , setMatrix
  , getMatrix
  , identityMatrix
  , userToDevice
  , userToDeviceDistance
  , deviceToUser
  , deviceToUserDistance

  , selectFontFace
  , setFontSize
  , setFontMatrix
  , getFontMatrix
  , showText
  , fontExtents
  , textExtents

  , fontOptionsCreate
  , fontOptionsCopy
  , fontOptionsMerge
  , fontOptionsHash
  , fontOptionsEqual
  , fontOptionsSetAntialias
  , fontOptionsGetAntialias
  , fontOptionsSetSubpixelOrder
  , fontOptionsGetSubpixelOrder
  , fontOptionsSetHintStyle
  , fontOptionsGetHintStyle
  , fontOptionsSetHintMetrics
  , fontOptionsGetHintMetrics

  , surfaceCreateSimilar
  , surfaceGetFontOptions
  , surfaceMarkDirty
  , surfaceMarkDirtyRectangle
  , surfaceSetDeviceOffset

  , withImageSurface
  , imageSurfaceGetWidth
  , imageSurfaceGetHeight

  , withImageSurfaceFromPNG
  , surfaceWriteToPNG

  , version
  , versionString

  , Matrix
  , Surface
  , Pattern
  , Status(..)
  , Operator(..)
  , Antialias(..)
  , FillRule(..)
  , LineCap(..)
  , LineJoin(..)
  , ScaledFont
  , FontFace
  , Glyph
  , TextExtents(..)
  , FontExtents(..)
  , FontSlant(..)
  , FontWeight(..)
  , SubpixelOrder(..)
  , HintStyle(..)
  , HintMetrics(..)
  , FontOptions
  , Path
  , Content(..)
  , Format(..)
  , Extend(..)
  , Filter(..)

  ) where

import Control.Monad.Reader
import Control.Exception
import Graphics.Rendering.Cairo.Types
import qualified Graphics.Rendering.Cairo.Internal as Internal

liftRender0 :: (Cairo -> IO a) -> Render a
liftRender0 f = ask >>= \context -> liftIO (f context)
liftRender1 :: (Cairo -> a -> IO b) -> a -> Render b
liftRender1 f a = ask >>= \context -> liftIO (f context a)
liftRender2 :: (Cairo -> a -> b -> IO c) -> a -> b -> Render c
liftRender2 f a b = ask >>= \context -> liftIO (f context a b)
liftRender3 :: (Cairo -> a -> b -> c -> IO d) -> a -> b -> c -> Render d
liftRender3 f a b c = ask >>= \context -> liftIO (f context a b c)
liftRender4 :: (Cairo -> a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> Render e
liftRender4 f a b c d = ask >>= \context -> liftIO (f context a b c d)
liftRender5 :: (Cairo -> a -> b -> c -> d -> e -> IO f) -> a -> b -> c -> d -> e -> Render f
liftRender5 f a b c d e = ask >>= \context -> liftIO (f context a b c d e)
liftRender6 :: (Cairo -> a -> b -> c -> d -> e -> f -> IO g) -> a -> b -> c -> d -> e -> f -> Render g
liftRender6 f a b c d e g = ask >>= \context -> liftIO (f context a b c d e g)

bracketR :: IO a -> (a -> IO b) -> (a -> Render c) -> Render c
bracketR begin end action = Render $ ReaderT $ \r ->
  bracket (begin)
          (\s -> end s)
          (\s -> runReaderT (runRender $ action s) r)

renderWith :: Surface -> Render a -> IO a
renderWith surface (Render m) =
  bracket (Internal.create surface)
          (\context -> do status <- Internal.status context
                          Internal.destroy context
                          unless (status == StatusSuccess) $
                            fail =<< Internal.statusToString status)
          (\context -> runReaderT m context)

save :: Render ()
save = liftRender0 Internal.save
restore :: Render ()
restore = liftRender0 Internal.restore
withTargetSurface :: (Surface -> Render a) -> Render a
withTargetSurface f = do
  context <- ask
  surface <- liftIO $ Internal.getTarget context
  f surface
  
-- | Sets the source pattern within cr to an opaque color. This opaque color
-- will then be used for any subsequent drawing operation until a new source
-- pattern is set.
--
-- The color components are floating point numbers in the range 0 to 1. If the
-- values passed in are outside that range, they will be clamped.
--
setSourceRGB ::
     Double -- ^ red component of color
  -> Double -- ^ green component of color
  -> Double -- ^ blue compoment of color
  -> Render ()
setSourceRGB = liftRender3 Internal.setSourceRGB

-- | Sets the source pattern within cr to a translucent color. This color will
-- then be used for any subsequent drawing operation until a new source pattern
-- is set.
--
-- The color and alpha components are floating point numbers in the range 0 to
-- 1. If the values passed in are outside that range, they will be clamped.
--
setSourceRGBA ::
     Double -- ^ red component of color
  -> Double -- ^ green component of color
  -> Double -- ^ blue component of color
  -> Double -- ^ alpha component of color
  -> Render ()
setSourceRGBA = liftRender4 Internal.setSourceRGBA

-- | Sets the source pattern within the context to source. This pattern will
-- then be used for any subsequent drawing operation until a new source pattern
-- is set.
--
-- Note: The pattern's transformation matrix will be locked to the user space
-- in effect at the time of cairo_set_source(). This means that further
-- modifications of the current transformation matrix will not affect the source
-- pattern. See 'setMatrix'.
--
setSource ::
     Pattern -- ^ a @Pattern@ to be used as the source for subsequent drawing
             -- operations.
  -> Render ()
setSource = liftRender1 Internal.setSource

-- | This is a convenience function for creating a pattern from surface and
-- setting it as the source in the context with 'setSource'.
--
-- The x and y parameters give the user-space coordinate at which the surface
-- origin should appear. (The surface origin is its upper-left corner before any
-- transformation has been applied.) The x and y patterns are negated and then
-- set as translation values in the pattern matrix.
--
-- Other than the initial translation pattern matrix, as described above, all
-- other pattern attributes, (such as its extend mode), are set to the default
-- values as in 'patternCreateForSurface'. The resulting pattern can be queried
-- with 'getSource' so that these attributes can be modified if desired, (eg. to
-- create a repeating pattern with 'patternSetExtent'.
--
setSourceSurface ::
     Surface -- ^ a surface to be used to set the source pattern
  -> Double  -- ^ user-space X coordinate for surface origin
  -> Double  -- ^ user-space Y coordinate for surface origin
  -> Render ()
setSourceSurface = liftRender3 Internal.setSourceSurface

-- | Gets the current source pattern.
--
getSource :: Render Pattern
getSource = liftRender0 Internal.getSource

-- | Set the antialiasing mode of the rasterizer used for drawing shapes. This
-- value is a hint, and a particular backend may or may not support a particular
-- value. At the current time, no backend supports @AntialiasSubpixel@ when
-- drawing shapes.
--
-- Note that this option does not affect text rendering, instead see
-- 'fontOptionsSetAntilias'.
--
setAntialias ::
     Antialias -- ^ the new antialiasing mode
  -> Render ()
setAntialias = liftRender1 Internal.setAntialias

-- | Gets the current shape antialiasing mode, as set by 'setAntialias'.
--
getAntialias :: Render Antialias
getAntialias = liftRender0 Internal.getAntialias

-- | Sets the dash pattern to be used by 'stroke'. A dash pattern is specified
-- by dashes, a list of positive values. Each value provides the user-space
-- length of altenate "on" and "off" portions of the stroke. The offset
-- specifies an offset into the pattern at which the stroke begins.
--
-- If @dashes@ is [] dashing is disabled.

-- If @dashes@ is [a] a symmetric pattern is assumed with alternating on and
-- off portions of the size specified by the single value in dashes.

-- If any value in @dashes@ is negative, or if all values are 0, then context
-- will be put into an error state with a status of @StatusInvalidDash@.
--
setDash ::
     [Double] -- ^ @dashes@ a list specifying alternate lengths of on and off
              -- portions of the stroke
  -> Double   -- ^ an offset into the dash pattern at which the stroke should
              -- start
  -> Render ()
setDash = liftRender2 Internal.setDash

-- | Set the current fill rule within the cairo context. The fill rule is used
-- to determine which regions are inside or outside a complex (potentially
-- self-intersecting) path. The current fill rule affects both 'fill' and
-- 'clip'. See 'FillRule' for details on the semantics of each available fill
-- rule.
--
setFillRule ::
     FillRule -- ^ a fill rule
  -> Render ()
setFillRule = liftRender1 Internal.setFillRule

-- | Gets the current fill rule, as set by 'setFillrule'.
--
getFillRule :: Render FillRule
getFillRule = liftRender0 Internal.getFillRule

-- | Sets the current line cap style within the cairo context. See 'LineCap'
-- for details about how the available line cap styles are drawn.
--
-- As with the other stroke parameters, the current line cap style is examined
-- by 'stroke', 'strokeExtents', and 'strokeToPath', but does not have any
-- effect during path construction.
--
setLineCap ::
     LineCap -- ^ a line cap style
  -> Render ()
setLineCap = liftRender1 Internal.setLineCap

-- | Gets the current line cap style, as set by 'setLineCap'.
--
getLineCap :: Render LineCap
getLineCap = liftRender0 Internal.getLineCap

-- | Sets the current line join style within the cairo context. See 'LineJoin'
-- for details about how the available line join styles are drawn.
--
-- As with the other stroke parameters, the current line join style is examined
-- by 'stroke', 'strokeExtents', and 'strokeToPath', but does not have any
-- effect during path construction.
--
setLineJoin ::
     LineJoin -- ^ a line joint style
  -> Render ()
setLineJoin = liftRender1 Internal.setLineJoin

-- | Gets the current line join style, as set by 'setLineJoin'.
--
getLineJoin :: Render LineJoin
getLineJoin = liftRender0 Internal.getLineJoin

-- | Sets the current line width within the cairo context. The line width
-- specifies the diameter of a pen that is circular in user-space.
--
-- As with the other stroke parameters, the current line cap style is examined
-- by 'stroke', 'strokeExtents', and 'strokeToPath', but does not have any
-- effect during path construction.
--
setLineWidth ::
     Double -- ^ a line width
  -> Render ()
setLineWidth = liftRender1 Internal.setLineWidth

-- | Gets the current line width, as set by 'setLineWidth'.
--
getLineWidth :: Render Double
getLineWidth = liftRender0 Internal.getLineWidth

-- | -
--
setMiterLimit ::
     Double -- ^ -
  -> Render ()
setMiterLimit = liftRender1 Internal.setMiterLimit

-- | Gets the current miter limit, as set by 'setMiterLimit'.
--
getMiterLimit :: Render Double
getMiterLimit = liftRender0 Internal.getMiterLimit

-- | Sets the compositing operator to be used for all drawing operations.
-- See 'Operator' for details on the semantics of each available compositing
-- operator.
--
setOperator ::
     Operator -- ^ a compositing operator
  -> Render ()
setOperator = liftRender1 Internal.setOperator

-- | Gets the current compositing operator for a cairo context.
--
getOperator :: Render Operator
getOperator = liftRender0 Internal.getOperator

-- | Sets the tolerance used when converting paths into trapezoids. Curved
-- segments of the path will be subdivided until the maximum deviation between
-- the original path and the polygonal approximation is less than tolerance.
-- The default value is 0.1. A larger value will give better performance,
-- a smaller value, better appearance. (Reducing the value from the default
-- value of 0.1 is unlikely to improve appearance significantly.)
--
setTolerance ::
     Double -- ^ the tolerance, in device units (typically pixels)
  -> Render ()
setTolerance = liftRender1 Internal.setTolerance

-- | Gets the current tolerance value, as set by 'setTolerance'.
--
getTolerance :: Render Double
getTolerance = liftRender0 Internal.getTolerance

-- | Establishes a new clip region by intersecting the current clip region with
-- the current path as it would be filled by 'fill' and according to the current
-- fill rule (see 'setFillRule').
--
-- After cairo_clip, the current path will be cleared from the cairo context.
--
-- The current clip region affects all drawing operations by effectively masking
-- out any changes to the surface that are outside the current clip region.
--
-- Calling 'clip' can only make the clip region smaller, never larger. But the
-- current clip is part of the graphics state, so a temporary restriction of the
-- clip region can be achieved by calling 'clip' within a 'save'/'restore' pair.
-- The only other means of increasing the size of the clip region is 'resetClip'.
--
clip :: Render ()
clip = liftRender0 Internal.clip

-- | Establishes a new clip region by intersecting the current clip region with
-- the current path as it would be filled by 'fill' and according to the current
-- fill rule (see 'setFillRule').
--
-- Unlike 'clip', cairoClipPreserve preserves the path within the cairo context.
--
-- The current clip region affects all drawing operations by effectively masking
-- out any changes to the surface that are outside the current clip region.
--
-- Calling 'clip' can only make the clip region smaller, never larger. But the
-- current clip is part of the graphics state, so a temporary restriction of the
-- clip region can be achieved by calling 'clip' within a 'save'/'restore' pair.
-- The only other means of increasing the size of the clip region is 'resetClip'.
--
clipPreserve :: Render ()
clipPreserve = liftRender0 Internal.clipPreserve

-- | Reset the current clip region to its original, unrestricted state. That is,
-- set the clip region to an infinitely large shape containing the target
-- surface. Equivalently, if infinity is too hard to grasp, one can imagine the
-- clip region being reset to the exact bounds of the target surface.
--
-- Note that code meant to be reusable should not call 'resetClip' as it will
-- cause results unexpected by higher-level code which calls 'clip'. Consider
-- using 'save' and 'restore' around 'clip' as a more robust means of
-- temporarily restricting the clip region.
--
resetClip :: Render ()
resetClip = liftRender0 Internal.resetClip

-- | A drawing operator that fills the current path according to the current
-- fill rule, (each sub-path is implicitly closed before being filled).
-- After 'fill', the current path will be cleared from the cairo context.
--
-- See 'setFillRule' and 'fillPreserve'.
--
fill :: Render ()
fill = liftRender0 Internal.fill

-- | A drawing operator that fills the current path according to the current
-- fill rule, (each sub-path is implicitly closed before being filled).
-- Unlike 'fill', 'fillPreserve' preserves the path within the cairo context.
--
-- See 'setFillRule' and 'fill'.
--
fillPreserve :: Render ()
fillPreserve = liftRender0 Internal.fillPreserve

-- | -
--
fillExtents :: Render (Double,Double,Double,Double)
fillExtents = liftRender0 Internal.fillExtents

-- | -
--
inFill :: Double -> Double -> Render Bool
inFill = liftRender2 Internal.inFill

-- | A drawing operator that paints the current source using the alpha channel
-- of pattern as a mask. (Opaque areas of mask are painted with the source,
-- transparent areas are not painted.)
--
mask ::
     Pattern -- ^ a @Pattern@
  -> Render ()
mask = liftRender1 Internal.mask

-- | A drawing operator that paints the current source using the alpha channel
-- of surface as a mask. (Opaque areas of surface are painted with the source,
-- transparent areas are not painted.)
--
maskSurface ::
     Surface -- ^ a @Surface@
  -> Double  -- ^ X coordinate at which to place the origin of surface
  -> Double  -- ^ Y coordinate at which to place the origin of surface
  -> Render ()
maskSurface = liftRender3 Internal.maskSurface

-- | A drawing operator that paints the current source everywhere within the
-- current clip region.
--
paint :: Render ()
paint = liftRender0 Internal.paint

-- | A drawing operator that paints the current source everywhere within the
-- current clip region using a mask of constant alpha value alpha. The effect
-- is similar to 'paint', but the drawing is faded out using the alpha value.
--
paintWithAlpha ::
     Double -- ^ alpha value, between 0 (transparent) and 1 (opaque)
  -> Render ()
paintWithAlpha = liftRender1 Internal.paintWithAlpha

-- | A drawing operator that strokes the current path according to the current
-- line width, line join, line cap, and dash settings. After cairo_stroke, the
-- current path will be cleared from the cairo context.
--
-- See 'setLineWidth', 'setLineJoin', 'setLineCap', 'setDash', and 'strokePreserve'.
--
stroke :: Render ()
stroke = liftRender0 Internal.stroke

-- | A drawing operator that strokes the current path according to the current
-- line width, line join, line cap, and dash settings. Unlike 'stroke',
-- 'strokePreserve' preserves the path within the cairo context.
--
-- See 'setLineWidth', 'setLineJoin', 'setLineCap', 'setDash', and 'strokePreserve'.
--
strokePreserve :: Render ()
strokePreserve = liftRender0 Internal.strokePreserve

-- | -
--
strokeExtents :: Render (Double,Double,Double,Double)
strokeExtents = liftRender0 Internal.strokeExtents

-- | -
--
inStroke :: Double -> Double -> Render Bool
inStroke = liftRender2 Internal.inStroke

-- | -
--
copyPage :: Render ()
copyPage = liftRender0 Internal.copyPage

-- | -
--
showPage :: Render ()
showPage = liftRender0 Internal.showPage


-- | Gets the current point of the current path, which is conceptually the final
-- point reached by the path so far.
--
-- The current point is returned in the user-space coordinate system. If there
-- is no defined current point then x and y will both be set to 0.0.
--
-- Most path construction functions alter the current point. See the following
-- for details on how they affect the current point: 'newPath', 'moveTo',
-- 'lineTo', 'curveTo', 'arc', 'relMoveTo', 'relLineTo', 'relCurveTo',
-- 'arcNegative', 'textPath', 'strokeToPath'.
--
getCurrentPoint :: Render (Double,Double)
getCurrentPoint = liftRender0 Internal.getCurrentPoint

-- | Clears the current path. After this call there will be no current point.
--
newPath :: Render ()
newPath = liftRender0 Internal.newPath

-- | Adds a line segment to the path from the current point to the beginning of
-- the current subpath, (the most recent point passed to 'moveTo'), and closes
-- this subpath.
--
-- The behavior of 'closePath' is distinct from simply calling 'lineTo' with the
-- equivalent coordinate in the case of stroking. When a closed subpath is
-- stroked, there are no caps on the ends of the subpath. Instead, their is a
-- line join connecting the final and initial segments of the subpath.
--
closePath :: Render ()
closePath = liftRender0 Internal.closePath

-- | Adds a circular arc of the given radius to the current path. The arc is
-- centered at (@xc@, @yc@), begins at @angle1@ and proceeds in the direction of
-- increasing angles to end at @angle2@. If @angle2@ is less than @angle1@ it
-- will be progressively increased by @2*pi@ until it is greater than @angle1@.
--
-- If there is a current point, an initial line segment will be added to the
-- path to connect the current point to the beginning of the arc.
--
-- Angles are measured in radians. An angle of 0 is in the direction of the
-- positive X axis (in user-space). An angle of @pi@ radians (90 degrees) is in
-- the direction of the positive Y axis (in user-space). Angles increase in the
-- direction from the positive X axis toward the positive Y axis. So with the
-- default transformation matrix, angles increase in a clockwise direction.
--
-- (To convert from degrees to radians, use @degrees * (pi \/ 180)@.)
--
-- This function gives the arc in the direction of increasing angles; see
-- 'arcNegative' to get the arc in the direction of decreasing angles.
--
-- The arc is circular in user-space. To achieve an elliptical arc, you can
-- scale the current transformation matrix by different amounts in the X and Y
-- directions. For example, to draw an ellipse in the box given by x, y, width,
-- height:
--
-- > save
-- > translate (x + width / 2) (y + height / 2)
-- > scale (1 / (height / 2.)) (1 / (width / 2))
-- > arc 0 0 1 0 (2 * pi)
-- > restore
--
arc ::
     Double -- ^ @xc@ - X position of the center of the arc
  -> Double -- ^ @yc@ - Y position of the center of the arc
  -> Double -- ^ @radius@ - the radius of the arc
  -> Double -- ^ @angle1@ - the start angle, in radians
  -> Double -- ^ @angle2@ - the end angle, in radians
  -> Render ()
arc = liftRender5 Internal.arc

-- | Adds a circular arc of the given radius to the current path. The arc is
-- centered at (@xc@, @yc@), begins at @angle1@ and proceeds in the direction of
-- decreasing angles to end at @angle2@. If @angle2@ is greater than @angle1@ it
-- will be progressively decreased by 2*@pi@ until it is greater than @angle1@.
--
-- See 'arc' for more details. This function differs only in the direction of
-- the arc between the two angles.
--
arcNegative ::
     Double -- ^ @xc@ - X position of the center of the arc
  -> Double -- ^ @yc@ - Y position of the center of the arc
  -> Double -- ^ @radius@ - the radius of the arc
  -> Double -- ^ @angle1@ - the start angle, in radians
  -> Double -- ^ @angle2@ - the end angle, in radians
  -> Render ()
arcNegative = liftRender5 Internal.arcNegative

-- | Adds a cubic Bézier spline to the path from the current point to position
-- (@x3@, @y3@) in user-space coordinates, using (@x1@, @y1@) and (@x2@, @y2@)
-- as the control points. After this call the current point will be (@x3@, @y3@).
--
curveTo ::
     Double -- ^ @x1@ - the X coordinate of the first control point
  -> Double -- ^ @y1@ - the Y coordinate of the first control point
  -> Double -- ^ @x2@ - the X coordinate of the second control point
  -> Double -- ^ @y2@ - the Y coordinate of the second control point
  -> Double -- ^ @x3@ - the X coordinate of the end of the curve
  -> Double -- ^ @y3@ - the Y coordinate of the end of the curve
  -> Render ()
curveTo = liftRender6 Internal.curveTo

-- | Adds a line to the path from the current point to position (@x@, @y@) in
-- user-space coordinates. After this call the current point will be (@x@, @y@).
--
lineTo ::
     Double -- ^ @x@ - the X coordinate of the end of the new line
  -> Double -- ^ @y@ - the Y coordinate of the end of the new line
  -> Render ()
lineTo = liftRender2 Internal.lineTo

-- | If the current subpath is not empty, begin a new subpath. After this call
-- the current point will be (@x@, @y@).
--
moveTo ::
     Double -- ^ @x@ - the X coordinate of the new position
  -> Double -- ^ @y@ - the Y coordinate of the new position
  -> Render ()
moveTo = liftRender2 Internal.moveTo

-- | Adds a closed-subpath rectangle of the given size to the current path at
-- position (@x@, @y@) in user-space coordinates.
--
rectangle ::
     Double -- ^ @x@ - the X coordinate of the top left corner of the rectangle 
  -> Double -- ^ @y@ - the Y coordinate of the top left corner of the rectangle 
  -> Double -- ^ @width@ - the width of the rectangle
  -> Double -- ^ @height@ - the height of the rectangle
  -> Render ()
rectangle = liftRender4 Internal.rectangle

-- | -
--
textPath ::
     String -- ^ -
  -> Render ()
textPath = liftRender1 Internal.textPath

-- | Relative-coordinate version of 'curveTo'. All offsets are relative to the
-- current point. Adds a cubic Bézier spline to the path from the current point
-- to a point offset from the current point by (@dx3@, @dy3@), using points
-- offset by (@dx1@, @dy1@) and (@dx2@, @dy2@) as the control points. After this
-- call the current point will be offset by (@dx3@, @dy3@).
--
-- Given a current point of (x, y), relCurveTo @dx1@ @dy1@ @dx2@ @dy2@ @dx3@ @dy3@
-- is logically equivalent to curveTo (x + @dx1@) (y + @dy1@) (x + @dx2@) (y + @dy2@) (x + @dx3@) (y + @dy3@).
--
relCurveTo ::
     Double -- ^ @dx1@ - the X offset to the first control point
  -> Double -- ^ @dy1@ - the Y offset to the first control point
  -> Double -- ^ @dx2@ - the X offset to the second control point
  -> Double -- ^ @dy2@ - the Y offset to the second control point
  -> Double -- ^ @dx3@ - the X offset to the end of the curve
  -> Double -- ^ @dy3@ - the Y offset to the end of the curve
  -> Render ()
relCurveTo = liftRender6 Internal.relCurveTo

-- | Relative-coordinate version of 'lineTo'. Adds a line to the path from the
-- current point to a point that is offset from the current point by (@dx@, @dy@)
-- in user space. After this call the current point will be offset by (@dx@, @dy@).
--
-- Given a current point of (x, y), relLineTo @dx@ @dy@ is logically equivalent
-- to lineTo (x + @dx@) (y + @dy@).
--
relLineTo ::
     Double -- ^ @dx@ - the X offset to the end of the new line
  -> Double -- ^ @dy@ - the Y offset to the end of the new line
  -> Render ()
relLineTo = liftRender2 Internal.relLineTo

-- | If the current subpath is not empty, begin a new subpath. After this call
-- the current point will offset by (x, y).
--
-- Given a current point of (x, y), relMoveTo @dx@ @dy@ is logically equivalent
-- to moveTo (x + @dx@) (y + @dy@)
--
relMoveTo ::
     Double -- ^ @dx@ -	the X offset
  -> Double -- ^ @dy@ -	the Y offset
  -> Render ()
relMoveTo = liftRender2 Internal.relMoveTo

withRGBPattern :: Double -> Double -> Double -> (Pattern -> Render a) -> Render a
withRGBPattern r g b f =
  bracketR (Internal.patternCreateRGB r g b)
           (\pattern -> do status <- Internal.patternStatus pattern
                           liftIO $ Internal.patternDestroy pattern
                           unless (status == StatusSuccess) $
                             fail =<< Internal.statusToString status)
           (\pattern -> f pattern)
withRGBAPattern :: Double -> Double -> Double -> Double -> (Pattern -> Render a) -> Render a
withRGBAPattern r g b a f =
  bracketR (Internal.patternCreateRGBA r g b a)
           (\pattern -> do status <- Internal.patternStatus pattern
                           liftIO $ Internal.patternDestroy pattern
                           unless (status == StatusSuccess) $
                             fail =<< Internal.statusToString status)
           (\pattern -> f pattern)
withPatternForSurface :: Surface -> (Pattern -> Render a) -> Render a
withPatternForSurface surface f =
  bracketR (Internal.patternCreateForSurface surface)
           (\pattern -> do status <- Internal.patternStatus pattern
                           liftIO $ Internal.patternDestroy pattern
                           unless (status == StatusSuccess) $
                             fail =<< Internal.statusToString status)
           (\pattern -> f pattern)
withLinearPattern :: Double -> Double -> Double -> Double -> (Pattern -> Render a) -> Render a
withLinearPattern x0 y0 x1 y1 f =
  bracketR (Internal.patternCreateLinear x0 y0 x1 y1)
           (\pattern -> do status <- Internal.patternStatus pattern
                           liftIO $ Internal.patternDestroy pattern
                           unless (status == StatusSuccess) $
                             fail =<< Internal.statusToString status)
           (\pattern -> f pattern)
withRadialPattern :: Double -> Double -> Double -> Double -> Double -> Double -> (Pattern -> Render a) -> Render a
withRadialPattern cx0 cy0 radius0 cx1 cy1 radius1 f =
  bracketR (Internal.patternCreateRadial cx0 cy0 radius0 cx1 cy1 radius1)
           (\pattern -> do status <- Internal.patternStatus pattern
                           liftIO $ Internal.patternDestroy pattern
                           unless (status == StatusSuccess) $
                             fail =<< Internal.statusToString status)
           (\pattern -> f pattern)

-- | Adds an opaque color stop to a gradient pattern. The offset specifies the
-- location along the gradient's control vector. For example, a linear gradient's
-- control vector is from (x0,y0) to (x1,y1) while a radial gradient's control
-- vector is from any point on the start circle to the corresponding point on
-- the end circle.
--
-- The color is specified in the same way as in 'setSourceRGB'.
--
-- Note: If the pattern is not a gradient pattern, (eg. a linear or radial
-- pattern), then the pattern will be put into an error status with a status of
-- 'StatusPatternTypeMismatch'.
--
patternAddColorStopRGB ::
     Pattern -- ^ a @Pattern@
  -> Double  -- ^ an offset in the range [0.0 .. 1.0]
  -> Double  -- ^ red component of color
  -> Double  -- ^ green component of color
  -> Double  -- ^ blue component of color
  -> Render ()
patternAddColorStopRGB p offset r g b = liftIO $ Internal.patternAddColorStopRGB p offset r g b

-- | Adds a translucent color stop to a gradient pattern. The offset specifies
-- the location along the gradient's control vector. For example, a linear
-- gradient's control vector is from (x0,y0) to (x1,y1) while a radial gradient's
-- control vector is from any point on the start circle to the corresponding
-- point on the end circle.
--
-- The color is specified in the same way as in setSourceRGBA.
--
-- Note: If the pattern is not a gradient pattern, (eg. a linear or radial
-- pattern), then the pattern will be put into an error status with a status of
-- @StatusPatternTypeMismatch@.
--
patternAddColorStopRGBA ::
     Pattern -- ^ a @Pattern@
  -> Double  -- ^ an offset in the range [0.0 .. 1.0]
  -> Double  -- ^ red component of color
  -> Double  -- ^ green component of color
  -> Double  -- ^ blue component of color
  -> Double  -- ^ alpha component of color
  -> Render ()
patternAddColorStopRGBA p offset r g b a = liftIO $ Internal.patternAddColorStopRGBA p offset r g b a

-- | Sets the pattern's transformation matrix to matrix. This matrix is a
-- transformation from user space to pattern space.
--
-- When a pattern is first created it always has the identity matrix for its
-- transformation matrix, which means that pattern space is initially identical
-- to user space.
--
-- Important: Please note that the direction of this transformation matrix is
-- from user space to pattern space. This means that if you imagine the flow
-- from a pattern to user space (and on to device space), then coordinates in
-- that flow will be transformed by the inverse of the pattern matrix.
--
-- Also, please note the discussion of the user-space locking semantics of 'setSource'.
--
patternSetMatrix ::
     Pattern -- ^ a @Pattern@
  -> Matrix  -- ^ a @Matrix@
  -> Render ()
patternSetMatrix p m = liftIO $ Internal.patternSetMatrix p m

-- | Get the pattern's transformation matrix.
--
patternGetMatrix ::
     Pattern -- ^ a @Pattern@
  -> Render Matrix
patternGetMatrix p = liftIO $ Internal.patternGetMatrix p

-- | -
--
patternSetExtend ::
     Pattern -- ^ a @Pattern@
  -> Extend  -- ^ an @Extent@
  -> Render ()
patternSetExtend p e = liftIO $ Internal.patternSetExtend p e
patternGetExtend :: Pattern -> Render Extend
patternGetExtend p = liftIO $ Internal.patternGetExtend p
patternSetFilter :: Pattern -> Filter -> Render ()
patternSetFilter p f = liftIO $ Internal.patternSetFilter p f
patternGetFilter :: Pattern -> Render Filter
patternGetFilter p = liftIO $ Internal.patternGetFilter p

translate :: Double -> Double -> Render ()
translate = liftRender2 Internal.translate
scale :: Double -> Double -> Render ()
scale = liftRender2 Internal.scale
rotate :: Double -> Render ()
rotate = liftRender1 Internal.rotate
transform :: Matrix -> Render ()
transform = liftRender1 Internal.transform
setMatrix :: Matrix -> Render ()
setMatrix = liftRender1 Internal.setMatrix
getMatrix :: Render Matrix
getMatrix = liftRender0 Internal.getMatrix
identityMatrix :: Render ()
identityMatrix = liftRender0 Internal.identityMatrix
userToDevice :: Double -> Double -> Render (Double,Double)
userToDevice = liftRender2 Internal.userToDevice
userToDeviceDistance :: Double -> Double -> Render (Double,Double)
userToDeviceDistance = liftRender2 Internal.userToDeviceDistance
deviceToUser :: Double -> Double -> Render (Double,Double)
deviceToUser = liftRender2 Internal.deviceToUser
deviceToUserDistance :: Double -> Double -> Render (Double,Double)
deviceToUserDistance = liftRender2 Internal.deviceToUserDistance

selectFontFace :: String -> FontSlant -> FontWeight -> Render ()
selectFontFace = liftRender3 Internal.selectFontFace
setFontSize :: Double -> Render ()
setFontSize = liftRender1 Internal.setFontSize
setFontMatrix :: Matrix -> Render ()
setFontMatrix = liftRender1 Internal.setFontMatrix
getFontMatrix :: Render Matrix
getFontMatrix = liftRender0 Internal.getFontMatrix
showText :: String -> Render ()
showText = liftRender1 Internal.showText
fontExtents :: Render FontExtents
fontExtents = liftRender0 Internal.fontExtents
textExtents :: String -> Render TextExtents
textExtents = liftRender1 Internal.textExtents

fontOptionsCreate :: Render FontOptions
fontOptionsCreate = liftIO $ Internal.fontOptionsCreate
fontOptionsCopy :: FontOptions -> Render FontOptions
fontOptionsCopy a = liftIO $ Internal.fontOptionsCopy a
fontOptionsMerge :: FontOptions -> FontOptions -> Render ()
fontOptionsMerge a b = liftIO $ Internal.fontOptionsMerge a b
fontOptionsHash :: FontOptions -> Render Int
fontOptionsHash a = liftIO $ Internal.fontOptionsHash a
fontOptionsEqual :: FontOptions -> FontOptions -> Render Bool
fontOptionsEqual a b = liftIO $ Internal.fontOptionsEqual a b
fontOptionsSetAntialias :: FontOptions -> Antialias -> Render ()
fontOptionsSetAntialias a b = liftIO $ Internal.fontOptionsSetAntialias a b
fontOptionsGetAntialias :: FontOptions -> Render Antialias
fontOptionsGetAntialias a = liftIO $ Internal.fontOptionsGetAntialias a
fontOptionsSetSubpixelOrder :: FontOptions -> SubpixelOrder-> Render ()
fontOptionsSetSubpixelOrder a b = liftIO $ Internal.fontOptionsSetSubpixelOrder a b
fontOptionsGetSubpixelOrder :: FontOptions -> Render SubpixelOrder
fontOptionsGetSubpixelOrder a = liftIO $ Internal.fontOptionsGetSubpixelOrder a
fontOptionsSetHintStyle :: FontOptions -> HintStyle -> Render ()
fontOptionsSetHintStyle a b = liftIO $ Internal.fontOptionsSetHintStyle a b
fontOptionsGetHintStyle :: FontOptions -> Render HintStyle
fontOptionsGetHintStyle a = liftIO $ Internal.fontOptionsGetHintStyle a
fontOptionsSetHintMetrics :: FontOptions -> HintMetrics -> Render ()
fontOptionsSetHintMetrics a b = liftIO $ Internal.fontOptionsSetHintMetrics a b
fontOptionsGetHintMetrics :: FontOptions -> Render HintMetrics
fontOptionsGetHintMetrics a = liftIO $ Internal.fontOptionsGetHintMetrics a

surfaceCreateSimilar :: Surface -> Content -> Int -> Int -> Render Surface
surfaceCreateSimilar a b c d = liftIO $ Internal.surfaceCreateSimilar a b c d
surfaceFlush :: Surface -> Render ()
surfaceFlush a = liftIO $ Internal.surfaceFlush a
surfaceGetFontOptions :: Surface -> Render FontOptions
surfaceGetFontOptions surface = do
  fontOptions <- fontOptionsCreate
  liftIO $ Internal.surfaceGetFontOptions surface fontOptions
  return fontOptions
surfaceMarkDirty :: Surface -> Render ()
surfaceMarkDirty a = liftIO $ Internal.surfaceMarkDirty a
surfaceMarkDirtyRectangle :: Surface -> Int -> Int -> Int -> Int -> Render ()
surfaceMarkDirtyRectangle a b c d e = liftIO $ Internal.surfaceMarkDirtyRectangle a b c d e
surfaceSetDeviceOffset :: Surface -> Double -> Double -> Render ()
surfaceSetDeviceOffset a b c = liftIO $ Internal.surfaceSetDeviceOffset a b c

withImageSurface :: Format -> Int -> Int -> (Surface -> IO a) -> IO ()
withImageSurface format width height f =
  bracket (liftIO $ Internal.imageSurfaceCreate format width height)
          (\surface -> f surface)
          (\surface -> do status <- Internal.surfaceStatus surface
                          liftIO $ Internal.surfaceDestroy surface
                          unless (status == StatusSuccess) $
                            Internal.statusToString status >>= fail)
imageSurfaceGetWidth :: Surface -> Render Int
imageSurfaceGetWidth a = liftIO $ Internal.imageSurfaceGetWidth a
imageSurfaceGetHeight :: Surface -> Render Int
imageSurfaceGetHeight a = liftIO $ Internal.imageSurfaceGetHeight a

withImageSurfaceFromPNG :: FilePath -> (Surface -> IO a) -> IO ()
withImageSurfaceFromPNG filename f =
  bracket (liftIO $ Internal.imageSurfaceCreateFromPNG filename)
          (\surface -> f surface)
          (\surface -> do status <- Internal.surfaceStatus surface
                          liftIO $ Internal.surfaceDestroy surface
                          unless (status == StatusSuccess) $
                            Internal.statusToString status >>= fail)
surfaceWriteToPNG :: Surface -> FilePath -> IO ()
surfaceWriteToPNG surface filename = do
  status <- Internal.surfaceWriteToPNG surface filename
  unless (status == StatusSuccess) $
    fail =<< Internal.statusToString status
  return ()

version :: Int
version = Internal.version
versionString :: String
versionString = Internal.versionString
