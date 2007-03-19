-- -*-haskell-*-
--  SOE implementation based on Gtk and cairo or Gdk.
--
--  Author : Duncan Coutts
--
--  Created: 10 October 2005
--
--  Version $Revision: 1.2 $ from $Date: 2006/01/08 12:12:19 $
--
--  Copyright (C) 2005 Duncan Coutts
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
-- Maintainer  : gtk2hs-devel@lists.sourceforge.net
-- Stability   : stable
-- Portability : portable (depends on GHC)
-- 
-- An alternative implementation of the graphics library used in The Haskell
-- School of Expression, by Paul Hudak, <http://www.haskell.org/soe/>.
--
-- It has exaclty the same interface as the original implementation
-- "Graphics.SOE". See the original for an API reference.
--
module Graphics.SOE.Gtk (
  runGraphics,
  Title,
  Size,
  Window,
  openWindow,
  getWindowSize,
  clearWindow,
  drawInWindow,
  drawInWindowNow,
  setGraphic,
  closeWindow,
  openWindowEx,
  RedrawMode,
  drawGraphic,
  drawBufferedGraphic,
  Graphic,
  emptyGraphic,
  overGraphic ,
  overGraphics,
  Color (..),
  withColor,
  text,
  Point,
  ellipse,
  shearEllipse,
  line,
  polygon,
  polyline,
  polyBezier,
  Angle,
  arc,
  Region,
  createRectangle,
  createEllipse,
  createPolygon,
  andRegion,
  orRegion,
  xorRegion,
  diffRegion,
  drawRegion,
  getKey,
  getLBP,
  getRBP,
  Event (..),
  maybeGetWindowEvent,
  getWindowEvent,
  Word32,
  getWindowTick,
  timeGetTime,
  word32ToInt
  ) where

#if GTK_CHECK_VERSION(2,8,0) && defined(ENABLE_CAIRO)
#define USE_CAIRO
#endif

import Data.Ix (Ix)
import Data.Word (Word32)
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Concurrent (forkIO, yield)
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import qualified System.Time
import qualified Graphics.UI.Gtk as Gtk

#ifdef USE_CAIRO
import qualified Graphics.UI.Gtk.Cairo as Gtk.Cairo
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Matrix as Matrix
#else
import qualified System.IO (hPutStrLn, stderr)
#endif

runGraphics :: IO () -> IO ()
runGraphics main = do
  Gtk.initGUI
  quitVar <- newIORef False
  forkIO (main >> writeIORef quitVar True)
  let loop = do
        yield
        Gtk.mainIteration
        quit <- readIORef quitVar
        if quit then return ()
                else loop
  loop
  -- give any windows a chance to close
  Gtk.flush
  return ()

type Title = String
type Size = (Int, Int)

data Window = Window {
  window :: Gtk.Window,
  canvas :: Gtk.DrawingArea,
  graphicVar :: MVar Graphic,
  eventsChan :: Chan Event,
  timerVar   :: MVar [MVar ()]
}

openWindow :: Title -> Size -> IO Window
openWindow title size =
  openWindowEx title Nothing (Just size) drawBufferedGraphic Nothing

openWindowEx ::
    Title
 -> Maybe Point
 -> Maybe Size
 -> RedrawMode
 -> Maybe Word32
 -> IO Window
openWindowEx title position size (RedrawMode useDoubleBuffer) timer = do
  window <- Gtk.windowNew
  Gtk.windowSetTitle window title

  canvas <- Gtk.drawingAreaNew
  Gtk.containerAdd window canvas
  Gtk.widgetSetCanFocus canvas True
  Gtk.widgetSetRedrawOnAllocate canvas False
  Gtk.widgetSetDoubleBuffered canvas useDoubleBuffer

  case position of
    Nothing     -> return ()
    Just (x, y) -> Gtk.windowMove window x y
  case size of
    Nothing              -> return ()
    Just (width, height) -> Gtk.windowSetDefaultSize window width height

  Gtk.widgetShowAll window
  
  graphicVar <- newMVar emptyGraphic
  eventsChan <- newChan
  timerVar   <- newMVar []

  -- set up the fonts
#ifdef USE_CAIRO
  pc <- Gtk.Cairo.cairoCreateContext Nothing
#else
  pc <- Gtk.widgetCreatePangoContext canvas
#endif
  fd <- Gtk.contextGetFontDescription pc
  Gtk.fontDescriptionSetSize fd 12
  Gtk.fontDescriptionSetFamily fd "Sans"
  Gtk.contextSetFontDescription pc fd

#ifndef USE_CAIRO
  win <- Gtk.widgetGetDrawWindow canvas
  -- set up the graphics context
  gc <- Gtk.gcNew win
#endif
  Gtk.onExpose canvas $ \Gtk.Expose { Gtk.eventArea = eventArea,
                                      Gtk.eventRegion = exposeRegion } -> do
    Graphic graphic <- readMVar graphicVar
    win <- Gtk.widgetGetDrawWindow canvas
#ifdef USE_CAIRO
    Gtk.Cairo.renderWithDrawable win $ do
      -- clip to the exposed region
      Gtk.Cairo.region exposeRegion
      Cairo.clip
      Cairo.paint                 --fill backgound with black
      Cairo.setSourceRGB 1 1 1    --use white default colour
      Cairo.setLineWidth 1
      -- actually do the drawing
      graphic pc
#else
    --fill backgound with black
    Gtk.gcSetValues gc Gtk.newGCValues { Gtk.foreground = colorToRGB Black }
    case eventArea of
      Gtk.Rectangle x y width height ->
        Gtk.drawRectangle win gc True x y width height
    --use white default colour
    Gtk.gcSetValues gc Gtk.newGCValues { Gtk.foreground = colorToRGB White }

    -- actually do the drawing
    graphic (Gtk.toDrawable win) gc pc
#endif
    return True

  Gtk.onDelete window $ \_ -> do writeChan eventsChan Closed
                                 return True
                     
  Gtk.onMotionNotify canvas True $ \Gtk.Motion { Gtk.eventX=x, Gtk.eventY=y} ->
    writeChan eventsChan MouseMove {
      pt = (round x, round y)
    } >> return True
  
  Gtk.onButtonPress canvas $ \event@Gtk.Button { Gtk.eventX=x, Gtk.eventY=y } ->
    writeChan eventsChan Button {
      pt = (round x,round y),
      isLeft = Gtk.eventButton event == Gtk.LeftButton,
      isDown = case Gtk.eventClick event of
                 Gtk.ReleaseClick -> False
                 _                     -> True
    } >> return True

  let keyPressHandler Gtk.Key { Gtk.eventKeyChar = Nothing } = return True
      keyPressHandler Gtk.Key { Gtk.eventKeyChar = Just char, Gtk.eventRelease = release } =
        writeChan eventsChan Key {
                     char = char,
                     isDown = not release
        } >> return True
  Gtk.onKeyPress canvas keyPressHandler
  Gtk.onKeyRelease canvas keyPressHandler

  Gtk.onSizeAllocate canvas $ \_ -> writeChan eventsChan Resize
  
  case timer of
    Nothing -> return ()
    Just delay -> do
      let tick = do
            ps <- takeMVar timerVar
            mapM_ (\p -> putMVar p ()) ps
            putMVar timerVar []
            return True
      Gtk.timeoutAddFull tick Gtk.priorityDefaultIdle (fromIntegral delay)
      return ()

  return Window {
    window  = window,
    canvas  = canvas,
    graphicVar = graphicVar,
    eventsChan = eventsChan,
    timerVar   = timerVar
  }

getWindowSize :: Window -> IO Size
getWindowSize win = Gtk.widgetGetSize (canvas win)

clearWindow :: Window -> IO ()
clearWindow win = setGraphic win emptyGraphic

drawInWindow :: Window -> Graphic -> IO ()
drawInWindow win graphic = do
  modifyMVar_ (graphicVar win) (return . overGraphic graphic)
  Gtk.widgetQueueDraw (canvas win)

drawInWindowNow :: Window -> Graphic -> IO ()
drawInWindowNow = drawInWindow

setGraphic :: Window -> Graphic -> IO ()
setGraphic win graphic = do
  modifyMVar_ (graphicVar win) (\_ -> return graphic)
  Gtk.widgetQueueDraw (canvas win)

closeWindow :: Window -> IO ()
closeWindow win = Gtk.widgetHide (window win)

newtype RedrawMode = RedrawMode Bool

drawGraphic :: RedrawMode
drawGraphic = RedrawMode False

drawBufferedGraphic :: RedrawMode
drawBufferedGraphic = RedrawMode True

data Color = Black
           | Blue
           | Green
           | Cyan
           | Red
           | Magenta
           | Yellow
           | White
  deriving (Eq, Ord, Bounded, Enum, Ix, Show, Read)

type Angle = Double

#ifdef USE_CAIRO

--------------------------------------------------
-- implementation using the new cairo API
--

newtype Graphic = Graphic (Gtk.PangoContext -> Cairo.Render ())

emptyGraphic :: Graphic
emptyGraphic = Graphic (\_ -> return ())

overGraphic :: Graphic -> Graphic -> Graphic
overGraphic (Graphic over) (Graphic base) = Graphic (\pc -> base pc >> over pc)

overGraphics :: [Graphic] -> Graphic
overGraphics = foldl1 overGraphic

colorToRGB :: Color -> (Double, Double, Double)
colorToRGB Black   = (0, 0, 0)
colorToRGB Blue    = (0, 0, 1)
colorToRGB Green   = (0, 1, 0)
colorToRGB Cyan    = (0, 1, 1)
colorToRGB Red     = (1, 0, 0)
colorToRGB Magenta = (1, 0, 1)
colorToRGB Yellow  = (1, 1, 0)
colorToRGB White   = (1, 1, 1)

withColor :: Color -> Graphic -> Graphic
withColor color (Graphic graphic) = Graphic $ \pc -> do
  Cairo.save
  case colorToRGB color of
    (r,g,b) -> Cairo.setSourceRGB r g b
  -- for some reason the SOE withColor uses a line width of 2,
  -- though the default line width outside of withColor is 1.
  Cairo.setLineWidth 2
  graphic pc
  Cairo.restore

text :: Point -> String -> Graphic
text (x,y) str = Graphic $ \pc -> do
  layout <- Cairo.liftIO $ Gtk.layoutEmpty pc
  Gtk.Cairo.updateLayout layout
  Cairo.liftIO $ Gtk.layoutSetText layout str
  Cairo.moveTo (fromIntegral x) (fromIntegral y)
  Gtk.Cairo.showLayout layout

type Point = (Int, Int)

ellipse :: Point -> Point -> Graphic
ellipse pt1 pt2 = Graphic $ \pc -> case normaliseBounds pt1 pt2 of
  Nothing -> return ()
  Just  (x,y,width,height) -> do
    Cairo.save
    Cairo.translate (x + width / 2) (y + height / 2)
    Cairo.scale (width / 2) (height / 2)
    Cairo.arc 0 0 1 0 (2*pi)
    Cairo.fill
    Cairo.restore

shearEllipse :: Point -> Point -> Point -> Graphic
shearEllipse (x1,y1) (x2,y2) (x3,y3) = Graphic $ \pc -> do
  let x = fromIntegral x1
      y = fromIntegral y1
      scalex = fromIntegral $ abs $ x1 - x3
      scaley = fromIntegral $ abs $ y1 - y2
      shearx = fromIntegral $ abs $ x1 - x2
      sheary = fromIntegral $ abs $ y1 - y3

  Cairo.save
  Cairo.transform (Matrix.Matrix scalex sheary shearx scaley x y)
  Cairo.arc 0.5 0.5 0.5 0 (2 * pi)
  Cairo.fill
  Cairo.restore

line :: Point -> Point -> Graphic
line (x1, y1) (x2, y2) = Graphic $ \pc -> do
  Cairo.moveTo (fromIntegral x1) (fromIntegral y1)
  Cairo.lineTo (fromIntegral x2) (fromIntegral y2)
  Cairo.stroke

polygon :: [Point] -> Graphic
polygon [] = Graphic (\_ -> return ())
polygon (p@(x,y):ps) = Graphic $ \pc -> do
  Cairo.moveTo (fromIntegral x) (fromIntegral y)
  sequence_ [ Cairo.lineTo (fromIntegral x) (fromIntegral y)
            | (x,y) <- ps ]
  Cairo.fill

polyline :: [Point] -> Graphic
polyline [] = Graphic (\_ -> return ())
polyline (p@(x,y):ps) = Graphic $ \pc -> do
  Cairo.moveTo (fromIntegral x) (fromIntegral y)
  sequence_ [ Cairo.lineTo (fromIntegral x) (fromIntegral y)
            | (x,y) <- ps ]
  Cairo.stroke

polyBezier :: [Point] -> Graphic
polyBezier [] = Graphic (\_ -> return ())
polyBezier ((x,y):ps) = Graphic $ \pc -> do
  Cairo.moveTo (fromIntegral x) (fromIntegral y)
  let loop ((x1,y1):(x2,y2):(x3,y3):ps) = do
        Cairo.curveTo (fromIntegral x1) (fromIntegral y2)
                      (fromIntegral x2) (fromIntegral y2)
                      (fromIntegral x3) (fromIntegral y3)
        loop ps
      loop _ = return ()
  loop ps
  Cairo.stroke

arc :: Point -> Point -> Angle -> Angle -> Graphic
arc pt1 pt2 start extent = Graphic $ \pc -> case normaliseBounds pt1 pt2 of
  Nothing -> return ()
  Just  (x,y,width,height) -> do
    Cairo.save
    Cairo.translate (x + width / 2) (y + height / 2)
    Cairo.scale (width / 2) (height / 2)
    Cairo.moveTo 0 0
    Cairo.arcNegative 0 0 1 (-start * pi / 180) (-(start+extent) * pi / 180)
    Cairo.fill
    Cairo.restore

data Region = Region {
  regionGraphic :: Int -> Int -> Cairo.Render (),
  regionOriginX :: !Int,
  regionOriginY :: !Int,
  regionWidth   :: !Int,
  regionHeight  :: !Int
}

createRectangle :: Point -> Point -> Region
createRectangle pt1 pt2 =
  let (x,y,width,height) = normaliseBounds' pt1 pt2 
      drawing x y = do
        Cairo.liftIO $ print ("createRectangle",x,y,width,height)
        Cairo.rectangle (fromIntegral x) (fromIntegral y)
                        (fromIntegral width) (fromIntegral height)
        Cairo.fill
   in Region drawing x y width height

createEllipse :: Point -> Point -> Region
createEllipse pt1 pt2 =
  let (x,y,width,height) = normaliseBounds' pt1 pt2
      drawing x y | width==0 || height==0 = return ()
                  | otherwise = do
        Cairo.save
        Cairo.translate (fromIntegral x + fromIntegral width / 2)
                        (fromIntegral y + fromIntegral height / 2)
        Cairo.scale (fromIntegral width / 2) (fromIntegral height / 2)
        Cairo.arc 0 0 1 0 (2*pi)
        Cairo.fill
        Cairo.restore
  in Region drawing x y width height

createPolygon :: [Point] -> Region
createPolygon [] = Region (\_ _ -> return ()) 0 0 0 0
createPolygon (p@(x,y):ps) =
  let minMax (x,y) (minx,maxx,miny,maxy) =
        let minx' = min minx x
            maxx' = max maxx x
            miny' = min miny y
            maxy' = max maxy y
         in seq minx' $ seq maxx' $ seq miny' $ seq maxy' $
            (minx',maxx',miny',maxy')
      (minx,maxx,miny,maxy) = foldr minMax (x,x,y,y) ps          
      drawing x y = do
        Cairo.moveTo (fromIntegral x) (fromIntegral y)
        sequence_ [ Cairo.lineTo (fromIntegral x) (fromIntegral y)
                  | (x,y) <- ps ]
        Cairo.fill
   in Region drawing minx miny (maxx - minx) (maxy - miny)

andRegion, orRegion, xorRegion, diffRegion :: Region -> Region -> Region
andRegion  = combineRegion Cairo.OperatorIn
orRegion   = combineRegion Cairo.OperatorOver
xorRegion  = combineRegion Cairo.OperatorXor
diffRegion = combineRegion Cairo.OperatorDestOut

drawRegion :: Region -> Graphic
drawRegion Region { regionGraphic = graphic,
                    regionOriginX = x,
                    regionOriginY = y
                  } = Graphic $ \_ -> do
  graphic x y

combineRegion :: Cairo.Operator -> Region -> Region -> Region
combineRegion operator a b =
  let x = min (regionOriginX a) (regionOriginX b)
      y = min (regionOriginY a) (regionOriginY b)
      x' = max (regionOriginX a + regionWidth a) (regionOriginX b + regionWidth b)
      y' = max (regionOriginY a + regionHeight a) (regionOriginY b + regionHeight b)
      width  = x' - x
      height = y' - y
      drawing x'' y'' = do
        Cairo.liftIO $ print ("combineRegion",x,y,width,height)
        Cairo.renderWithSimilarSurface Cairo.ContentAlpha width height $
          \surface -> do
          Cairo.renderWith surface $ do
            Cairo.setSourceRGBA 0 0 0 1
            regionGraphic a (regionOriginX a - x)
                            (regionOriginY a - y)
            Cairo.setOperator operator
            regionGraphic b (regionOriginX b - x)
                            (regionOriginY b - y)
          Cairo.liftIO $ print ("mask surface",x,y)
          Cairo.maskSurface surface (fromIntegral x'') (fromIntegral y'')
   in Region drawing x y width height

#else

--------------------------------------------------
-- implementation using the old Gdk API
--

newtype Graphic = Graphic (Gtk.Drawable -> Gtk.GC -> Gtk.PangoContext -> IO ())

emptyGraphic :: Graphic
emptyGraphic = Graphic (\_ _ _ -> return ())

overGraphic :: Graphic -> Graphic -> Graphic
overGraphic (Graphic over) (Graphic base) =
  Graphic (\dw gc pc -> base dw gc pc >> over dw gc pc)

overGraphics :: [Graphic] -> Graphic
overGraphics = foldl1 overGraphic

colorToRGB :: Color -> Gtk.Color
colorToRGB Black   = Gtk.Color 0     0     0
colorToRGB Blue    = Gtk.Color 0     0     65535
colorToRGB Green   = Gtk.Color 0     65535 0
colorToRGB Cyan    = Gtk.Color 0     65535 65535
colorToRGB Red     = Gtk.Color 65535 0     0
colorToRGB Magenta = Gtk.Color 65535 0     65535
colorToRGB Yellow  = Gtk.Color 65535 65535 0
colorToRGB White   = Gtk.Color 65535 65535 65535

withColor :: Color -> Graphic -> Graphic
withColor color (Graphic graphic) = Graphic $ \dw gc pc -> do
  v <- Gtk.gcGetValues gc
  Gtk.gcSetValues gc Gtk.newGCValues {
      Gtk.foreground = colorToRGB color,
      Gtk.lineWidth = 2
    }
  graphic dw gc pc
  Gtk.gcSetValues gc Gtk.newGCValues {
      Gtk.foreground = Gtk.foreground v,
      Gtk.lineWidth  = Gtk.lineWidth v
    }

text :: Point -> String -> Graphic
text (x,y) str = Graphic $ \dw gc pc -> do
  pl <- Gtk.layoutEmpty pc
  Gtk.layoutSetText pl str
  Gtk.drawLayout dw gc x y pl

type Point = (Int, Int)

ellipse :: Point -> Point -> Graphic
ellipse pt1 pt2 = Graphic $ \dw gc pc -> do
  let (x,y,width,height) = normaliseBounds' pt1 pt2
  Gtk.drawArc dw gc True x y width height 0 (360*64)

shearEllipse :: Point -> Point -> Point -> Graphic
shearEllipse (x1,y1) (x2,y2) (x3,y3) = Graphic $ \dw gc pc -> do
  let avg a b = (a + b) `div` 2
      x = avg x2 x3
      y = avg y2 y3
      dx1 = fromIntegral ((x2 - x1) `div` 2)
      dy1 = fromIntegral ((y2 - y1) `div` 2)
      dx2 = fromIntegral ((x3 - x1) `div` 2)
      dy2 = fromIntegral ((y3 - y1) `div` 2)
      ps = [ (x + round(cos a * dx1 + sin a * dx2)
             ,y + round(cos a * dy1 + sin a * dy2))
           | a <- take 60 [0, pi/30 .. ] ]
  Gtk.drawPolygon dw gc True ps

line :: Point -> Point -> Graphic
line p1 p2 = Graphic $ \dw gc pc -> Gtk.drawLine dw gc p1 p2

polygon :: [Point] -> Graphic
polygon ps = Graphic $ \dw gc pc -> Gtk.drawPolygon dw gc True ps

polyline :: [Point] -> Graphic
polyline ps = Graphic $ \dw gc pc -> Gtk.drawLines dw gc ps

polyBezier :: [Point] -> Graphic
polyBezier ps = Graphic $ \dw gc pc -> do
  System.IO.hPutStrLn System.IO.stderr $ "warning: polyBezier is only available "
    ++ "in Grahpics.SOE.Gtk built with Gtk+ 2.8 or later -- using polyline instead"
  Gtk.drawLines dw gc ps

arc :: Point -> Point -> Angle -> Angle -> Graphic
arc pt1 pt2 start extent = Graphic $ \dw gc pc -> do
  let (x,y,width,height) = normaliseBounds' pt1 pt2
  Gtk.drawArc dw gc True x y width height
    (round $ start * 64)
    (round $ (start+extent) * 64)

newtype Region = Region (IO Gtk.Region)

createRectangle :: Point -> Point -> Region
createRectangle pt1 pt2 =
  let (x,y,width,height) = normaliseBounds' pt1 pt2 
      region = Gtk.regionRectangle (Gtk.Rectangle x y width height)
   in Region region

createEllipse :: Point -> Point -> Region
createEllipse (x1, y1) (x2, y2) =
  let rx = (x2 - x1) `div` 2
      ry = (y2 - y1) `div` 2
      cx = x1 + rx
      cy = y1 + ry
      rx' = fromIntegral rx
      ry' = fromIntegral ry
      ps = [ (cx + round (rx' * cos a), cy + round (ry' * sin a))
           | a <- take 60 [0, pi/30 .. ] ]
      region = Gtk.regionPolygon ps Gtk.WindingRule
   in Region region

createPolygon :: [Point] -> Region
createPolygon ps =
  let region = Gtk.regionPolygon ps Gtk.WindingRule
   in Region region

andRegion, orRegion, xorRegion, diffRegion :: Region -> Region -> Region
andRegion  (Region a) (Region b) = Region $ do
  regionA <- a
  regionB <- b
  Gtk.regionIntersect regionA regionB
  return regionA
orRegion   (Region a) (Region b) = Region $ do
  regionA <- a
  regionB <- b
  Gtk.regionUnion regionA regionB
  return regionA
xorRegion  (Region a) (Region b) = Region $ do
  regionA <- a
  regionB <- b
  Gtk.regionXor regionA regionB
  return regionA
diffRegion (Region a) (Region b) = Region $ do
  regionA <- a
  regionB <- b
  Gtk.regionSubtract regionA regionB
  return regionA

drawRegion :: Region -> Graphic
drawRegion (Region mkRegion) = Graphic $ \dw gc pc -> do
  region <- mkRegion
  rects <- Gtk.regionGetRectangles region
  mapM_ (\(Gtk.Rectangle x y width height) ->
           Gtk.drawRectangle dw gc True x y width height)
        rects

#endif

normaliseBounds :: Point -> Point -> Maybe (Double,Double,Double,Double)
normaliseBounds (x1,y1) (x2,y2) = 
  if x1==x2 || y1==y2 then Nothing else Just (x, y, width, height)
  where x = fromIntegral $ min x1 x2
        y = fromIntegral $ min y1 y2
        width  = fromIntegral $ abs $ x1 - x2
        height = fromIntegral $ abs $ y1 - y2

normaliseBounds' :: Point -> Point -> (Int,Int,Int,Int)
normaliseBounds' (x1,y1) (x2,y2) = (x, y, width, height)
  where x = min x1 x2
        y = min y1 y2
        width  = abs $ x1 - x2
        height = abs $ y1 - y2

data Event = Key {
               char :: Char,
               isDown :: Bool
             }
           | Button {
              pt :: Point,
              isLeft :: Bool,
              isDown :: Bool
             }
           | MouseMove {
               pt :: Point
             }
           | Resize
           | Closed
  deriving Show

getWindowEvent_ :: Window -> IO Event
getWindowEvent_ win = readChan (eventsChan win)

getWindowEvent :: Window -> IO Event
getWindowEvent win = do
  event <- getWindowEvent_ win
  -- this says we are ready for another mouse move event
  -- (this is part of the pointer move event flood prevention system)
  case event of
    MouseMove _ -> Gtk.widgetGetDrawWindow (canvas win)
                   >>= Gtk.drawWindowGetPointer
                   >> return ()
    _ -> return ()
  return event

maybeGetWindowEvent :: Window -> IO (Maybe Event)
maybeGetWindowEvent win = do
  noEvents <- isEmptyChan (eventsChan win)
  if noEvents then return Nothing
              else do event <- readChan (eventsChan win)
                      case event of
                        MouseMove _ -> Gtk.widgetGetDrawWindow (canvas win)
                                       >>= Gtk.drawWindowGetPointer
                                       >> return ()
                        _ -> return ()
                      return (Just event)


getKeyEx :: Window -> Bool -> IO Char
getKeyEx win down = loop
  where loop = do e <- getWindowEvent_ win
                  case e of
                    (Key { char = ch, isDown = d })
                      | d == down -> return ch
                    _ -> loop

getKey :: Window -> IO Char
getKey win = getKeyEx win True >> getKeyEx win False

getButton :: Window -> Int -> Bool -> IO Point
getButton win but down = loop
  where loop = do e <- getWindowEvent_ win
                  case e of
                    (Button { pt = pt, isDown = id })
                      | id == down -> return pt
                    _ -> loop

getLBP :: Window -> IO Point
getLBP w = getButton w 1 True

getRBP :: Window -> IO Point
getRBP w = getButton w 2 True

getWindowTick :: Window -> IO ()
getWindowTick win = do
  ps <- takeMVar (timerVar win)
  p  <- newEmptyMVar
  putMVar (timerVar win) (p:ps)
  takeMVar p  --block until the timer fills this MVar

timeGetTime :: IO Word32
timeGetTime = do
  System.Time.TOD sec psec <- System.Time.getClockTime
  return (fromIntegral $ sec * 1000 + psec `div` 1000000000)

word32ToInt :: Word32 -> Int
word32ToInt = fromIntegral
