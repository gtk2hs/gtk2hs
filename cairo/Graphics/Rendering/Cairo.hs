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

  , liftIO

  , Render
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

-- newtype Render m = Render (ReaderT Cairo IO m)
--   deriving (Functor, Monad, MonadIO, MonadReader Cairo)
newtype Render m = Render { unRender :: ReaderT Cairo IO m }
  deriving (Functor, Monad, MonadIO, MonadReader Cairo)

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
          (\s -> runReaderT (unRender $ action s) r)

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
setSourceRGB :: Double -> Double -> Double -> Render ()
setSourceRGB = liftRender3 Internal.setSourceRGB
setSourceRGBA :: Double -> Double -> Double -> Double -> Render ()
setSourceRGBA = liftRender4 Internal.setSourceRGBA
setSource :: Pattern -> Render ()
setSource = liftRender1 Internal.setSource
setSourceSurface :: Surface -> Double -> Double -> Render ()
setSourceSurface = liftRender3 Internal.setSourceSurface
getSource :: Render Pattern
getSource = liftRender0 Internal.getSource
setAntialias :: Antialias -> Render ()
setAntialias = liftRender1 Internal.setAntialias
getAntialias :: Render Antialias
getAntialias = liftRender0 Internal.getAntialias
setDash :: [Double] -> Double -> Render ()
setDash = liftRender2 Internal.setDash
setFillRule :: FillRule -> Render ()
setFillRule = liftRender1 Internal.setFillRule
getFillRule :: Render FillRule
getFillRule = liftRender0 Internal.getFillRule
setLineCap :: LineCap -> Render ()
setLineCap = liftRender1 Internal.setLineCap
getLineCap :: Render LineCap
getLineCap = liftRender0 Internal.getLineCap
setLineJoin :: LineJoin -> Render ()
setLineJoin = liftRender1 Internal.setLineJoin
getLineJoin :: Render LineJoin
getLineJoin = liftRender0 Internal.getLineJoin
setLineWidth :: Double -> Render ()
setLineWidth = liftRender1 Internal.setLineWidth
getLineWidth :: Render Double
getLineWidth = liftRender0 Internal.getLineWidth
setMiterLimit :: Double -> Render ()
setMiterLimit = liftRender1 Internal.setMiterLimit
getMiterLimit :: Render Double
getMiterLimit = liftRender0 Internal.getMiterLimit
setOperator :: Operator -> Render ()
setOperator = liftRender1 Internal.setOperator
getOperator :: Render Operator
getOperator = liftRender0 Internal.getOperator
setTolerance :: Double -> Render ()
setTolerance = liftRender1 Internal.setTolerance
getTolerance :: Render Double
getTolerance = liftRender0 Internal.getTolerance
clip :: Render ()
clip = liftRender0 Internal.clip
clipPreserve :: Render ()
clipPreserve = liftRender0 Internal.clipPreserve
resetClip :: Render ()
resetClip = liftRender0 Internal.resetClip
fill :: Render ()
fill = liftRender0 Internal.fill
fillPreserve :: Render ()
fillPreserve = liftRender0 Internal.fillPreserve
fillExtents :: Render (Double,Double,Double,Double)
fillExtents = liftRender0 Internal.fillExtents
inFill :: Double -> Double -> Render Bool
inFill = liftRender2 Internal.inFill
mask :: Pattern -> Render ()
mask = liftRender1 Internal.mask
maskSurface :: Surface -> Double -> Double -> Render ()
maskSurface = liftRender3 Internal.maskSurface
paint :: Render ()
paint = liftRender0 Internal.paint
paintWithAlpha :: Double -> Render ()
paintWithAlpha = liftRender1 Internal.paintWithAlpha
stroke :: Render ()
stroke = liftRender0 Internal.stroke
strokePreserve :: Render ()
strokePreserve = liftRender0 Internal.strokePreserve
strokeExtents :: Render (Double,Double,Double,Double)
strokeExtents = liftRender0 Internal.strokeExtents
inStroke :: Double -> Double -> Render Bool
inStroke = liftRender2 Internal.inStroke
copyPage :: Render ()
copyPage = liftRender0 Internal.copyPage
showPage :: Render ()
showPage = liftRender0 Internal.showPage

getCurrentPoint :: Render (Double,Double)
getCurrentPoint = liftRender0 Internal.getCurrentPoint
newPath :: Render ()
newPath = liftRender0 Internal.newPath
closePath :: Render ()
closePath = liftRender0 Internal.closePath
arc :: Double -> Double -> Double -> Double -> Double -> Render ()
arc = liftRender5 Internal.arc
arcNegative :: Double -> Double -> Double -> Double -> Double -> Render ()
arcNegative = liftRender5 Internal.arcNegative
curveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
curveTo = liftRender6 Internal.curveTo
lineTo :: Double -> Double -> Render ()
lineTo = liftRender2 Internal.lineTo
moveTo :: Double -> Double -> Render ()
moveTo = liftRender2 Internal.moveTo
rectangle :: Double -> Double -> Double -> Double -> Render ()
rectangle = liftRender4 Internal.rectangle
textPath :: String -> Render ()
textPath = liftRender1 Internal.textPath
relCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
relCurveTo = liftRender6 Internal.relCurveTo
relLineTo :: Double -> Double -> Render ()
relLineTo = liftRender2 Internal.relLineTo
relMoveTo :: Double -> Double -> Render ()
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
patternAddColorStopRGB :: Pattern -> Double -> Double -> Double -> Double -> Render ()
patternAddColorStopRGB p offset r g b = liftIO $ Internal.patternAddColorStopRGB p offset r g b
patternAddColorStopRGBA :: Pattern -> Double -> Double -> Double -> Double -> Double -> Render ()
patternAddColorStopRGBA p offset r g b a = liftIO $ Internal.patternAddColorStopRGBA p offset r g b a
patternSetMatrix :: Pattern -> Matrix -> Render ()
patternSetMatrix p m = liftIO $ Internal.patternSetMatrix p m
patternGetMatrix :: Pattern -> Render Matrix
patternGetMatrix p = liftIO $ Internal.patternGetMatrix p
patternSetExtend :: Pattern ->  Extend -> Render ()
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
